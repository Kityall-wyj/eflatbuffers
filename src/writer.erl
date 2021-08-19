%%%-------------------------------------------------------------------
%%% @author kityall_wyj
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. 7月 2021 16:23
%%%-------------------------------------------------------------------
-module(writer).
-author("kityall_wyj").

-export([write/4]).

write({_, #{default := Default}}, Default, _, _) ->
	[];

write({_, _}, undefined, _, _) ->
	[];

write({bool, _Options}, true, _, _) ->
	<< 1 >>;

write({bool, _Options}, false, _, _) ->
	<< 0 >>;

write({byte, _Options}, Byte, _, _) when is_integer(Byte) andalso Byte >= -128 andalso Byte =< 127 ->
	<<Byte:8>>;

write({ubyte, _Options}, Byte, _, _) when is_integer(Byte) andalso Byte >= 0 andalso Byte =< 255 ->
	<<Byte:8>>;

write({short, _Options}, Integer, _, _) when is_integer(Integer) andalso Integer =< 32767 andalso Integer >= -32768 ->
	<<Integer:16/little>>;

write({ushort, _Options}, Integer, _, _) when is_integer(Integer) andalso Integer >= 0 andalso Integer =< 65536 ->
	<<Integer:16/little>>;

write({int, _Options}, Integer, _, _) when is_integer(Integer) andalso Integer >= -2147483648 andalso Integer =< 2147483647 ->
	<<Integer:32/little>>;

write({uint, _Options}, Integer, _, _) when is_integer(Integer) andalso Integer >= 0 andalso Integer =< 4294967295 ->
	<<Integer:32/little>>;

write({float, _Options}, Float, _, _) when (is_float(Float) or is_integer(Float)) andalso Float >= -3.4E+38 andalso Float =< +3.4E+38 ->
	<<Float:32/float-little>>;

write({long, _Options}, Integer, _, _) when is_integer(Integer) andalso Integer >= -9223372036854775808 andalso Integer =< 9223372036854775807 ->
	<<Integer:64/little>>;

write({ulong, _Options}, Integer, _, _) when is_integer(Integer) andalso Integer >= 0 andalso Integer =< 18446744073709551615 ->
	<<Integer:64/little>>;

write({double, _Options}, Float, _, _) when (is_float(Float) or is_integer(Float)) andalso Float >= -1.7E+308 andalso Float =< +1.7E+308 ->
	<<Float:64/float-little>>;

write({string, _Options}, String, _, _) when is_binary(String) ->
	<<(byte_size(String)):32/little, String/binary>>;

write({vector, Options}, Values, Path, Schema) when is_list(Values) ->
	{Type, TypeOptions} = maps:get(type, Options),
	VectorLength = length(Values),

%%# we are putting the indices as [i] as a type
%%# so if something goes wrong it's easy to see
%%    # that it was a vector index
	TypeOptionsWithoutDefault = maps:put(default, undefined, TypeOptions),
	IndexTypes = [{I, {Type, TypeOptionsWithoutDefault}} || I <- lists:seq(0, VectorLength - 1)],
	[<<VectorLength:32/little>>, data_buffer_and_data(IndexTypes, Values, Path, Schema)];

write({enum, Options = #{name := EnumName}}, Value, Path, {Tables, _} = Schema) when is_binary(Value) ->
    {enum, EnumOptions} = maps:get(EnumName, Tables),
    Members = maps:get(members, EnumOptions),
    {Type, TypeOptions} = maps:get(type, EnumOptions),
%%    # if we got handed some defaults from outside,
%%    # we put them in here
    TypeOptionsNew = maps:merge(TypeOptions, Options),
    ValueAtom = binary_to_existing_atom(Value, utf8),
    Index = maps:get(ValueAtom, Members),
    case Index of
      undefined -> throw({error, {not_in_enum, ValueAtom, Members}});
      _ -> write({Type, TypeOptionsNew}, Index, Path, Schema)
    end;

%%  # write a complete table
write({table, #{name := TableName}}, Map, Path, {Tables, _Options} = Schema) when is_map(Map) andalso is_atom(TableName) ->
    {table, Options} = maps:get(TableName, Tables),
    Fields = maps:get(fields, Options),
	Fun = fun({Name, Type}, {TypeAcc, ValueAcc}) ->
		case Type of
			{union, #{name := UnionName}} ->
				{union, Options} = maps:get(UnionName, Tables),
				Members = maps:get(members, Options),
				case maps:get(list_to_atom(atom_to_list(Name) ++ "_type"), Map) of
					undefined ->
						TypeAccNew = [{{Name}, {byte, #{default => 0}}} | TypeAcc],
						ValueAccNew = [0 | ValueAcc],
						{TypeAccNew, ValueAccNew};
					UnionType ->
						UnionType1 = list_to_atom(UnionType),
						UnionIndex = maps:get(UnionType1, Members),
						TypeAccNew = [{{Name}, {byte, #{default => 0}}} | [{name, {table, #{name => UnionType1}}} | TypeAcc]],
						ValueAccNew = [UnionIndex + 1, maps:get(Name, Map) | ValueAcc],
						{TypeAccNew, ValueAccNew}
				end;
			_ ->
            {[{{Name}, Type} | TypeAcc], [maps:get(Name, Map, undefined) | ValueAcc]}
		end
		end,
	{NameTypes, Values} = lists:foldl(Fun, {[], []}, lists:reverse(Fields)),
%%    # we are putting the keys as {key} as a type
%%    # so if something goes wrong it's easy to see
%%# that it was a map key
	[DataBuffer, Data] = data_buffer_and_data(NameTypes, Values, Path, Schema),
	Vtable = vtable(DataBuffer),
	Springboard = <<(iolist_size(Vtable) + 4):32/little>>,
	DataBufferLength = <<(iolist_size([Springboard, DataBuffer])):16/little>>,
	VtableLength = <<(iolist_size([Vtable, Springboard])):16/little >>,
	[VtableLength, DataBufferLength, Vtable, Springboard, DataBuffer, Data];


%%# fail if nothing matches
write({Type, _Options}, Data, Path, _) ->
	throw({error, {wrong_type, Type, Data, lists:reverse(Path)}}).

%%# build up [data_buffer, data]
%%# as part of a table or vector
data_buffer_and_data(Types, Values, Path, Schema) ->
	data_buffer_and_data(Types, Values, Path, Schema, {[], [], 0}).

data_buffer_and_data([], [], _path, _schema, {DataBuffer, Data, _}) ->
	[adjust_for_length(DataBuffer), lists:reverse(Data)];

%%# value is undefined so we put a null pointer
data_buffer_and_data([_type | Types], [undefined | Values], Path, Schema, {ScalarAndPointers, Data, DataOffset}) ->
	data_buffer_and_data(Types, Values, Path, Schema, {[[] | ScalarAndPointers], Data, DataOffset});

data_buffer_and_data([{Name, Type} | Types], [Value | Values], Path, Schema, {ScalarAndPointers, Data, DataOffset}) ->
%%# for clean error reporting we
%%# need to accumulate the names of tables (depth)
%%# but not the indices for vectors (width)
	case utils:scalar(Type) of
		true ->
			ScalarData = write(Type, Value, [Name|Path], Schema),
			data_buffer_and_data(Types, Values, Path, Schema, {[ScalarData | ScalarAndPointers], Data, DataOffset});
		false ->
			ComplexData = write(Type, Value, [Name|Path], Schema),
			ComplexDataLength = erlang:iolist_size(ComplexData),
%%# for a table we -> not point to the start but to the springboard
			DataPointer =
				case Type of
					{table, _} ->
						[VtableLength, DataBufferLength, Vtable | _] = ComplexData,
						TableHeaderOffset = erlang:iolist_size([VtableLength, DataBufferLength, Vtable]),
						DataOffset + TableHeaderOffset;
					_ ->
						DataOffset
				end,
			data_buffer_and_data(Types, Values, Path, Schema, {[DataPointer | ScalarAndPointers], [ComplexData | Data], ComplexDataLength + DataOffset})
	end.


%%# so this is a mix of scalars (binary)
%%# and unadjusted pointers (integers)
%%# we adjust the pointers to account
%%# for their poisition in the buffer
adjust_for_length(DataBuffer) ->
	adjust_for_length(DataBuffer, {[], 0}).

adjust_for_length([], {Acc, _}) ->
	Acc;

%%# this is null pointers, we pass
adjust_for_length([[] | DataBuffer], {Acc, Offset}) ->
	adjust_for_length(DataBuffer, {[[]|Acc], Offset});
%%end
%%# this is a scalar, we just pass the data
adjust_for_length([Scalar | DataBuffer], {Acc, Offset}) when is_binary(Scalar) ->
	adjust_for_length(DataBuffer, {[Scalar | Acc], Offset + byte_size(Scalar)});

%%# referenced data, we get it and recurse
adjust_for_length([Pointer | DataBuffer], {Acc, Offset}) when is_integer(Pointer) ->
	OffsetNew = Offset + 4,
	PointerBin = <<(Pointer + OffsetNew):32/little>>,
	adjust_for_length(DataBuffer, {[PointerBin | Acc], OffsetNew});


%%# we get a nested structure so we pass it untouched
adjust_for_length([Iolist | DataBuffer], {Acc, Offset}) when is_list(Iolist) ->
	adjust_for_length(DataBuffer, {[Iolist | Acc], Offset + 4}).



vtable(DataBuffer) ->
	lists:reverse(vtable(DataBuffer, {[], 4})).

vtable([], {Acc, _offset}) ->
	Acc;
vtable([Data | DataBuffer], {Acc, Offset}) ->
	case Data of
		[] ->
%%# this is an undefined value, we put a null pointer
%%# and leave the offset untouched
			vtable(DataBuffer, {[<<0:16/little>> | Acc], Offset});
		ScalarOrPointer ->
			vtable(DataBuffer, {[<<Offset:16/little>> | Acc], Offset + erlang:iolist_size(ScalarOrPointer) })
	end.

scalar(string) -> false;
scalar({vector, _}) -> false;
scalar({table,  _}) -> false;
scalar({enum,  _}) -> true;
scalar(_) -> true.

