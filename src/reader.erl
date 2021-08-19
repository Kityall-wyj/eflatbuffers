%%%-------------------------------------------------------------------
%%% @author kityall_wyj
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 3. 8月 2021 16:23
%%%-------------------------------------------------------------------
-module(reader).
-author("kityall_wyj").

-export([read/4]).

read({bool, _Options }, VtablePointer, Data, _) ->
	case read_from_data_buffer(VtablePointer, Data, 8) of
		<<0>> -> false;
		<<1>> -> true
	end;

read({byte, _Options }, VtablePointer, Data, _) ->
	<<Value:8>> = read_from_data_buffer(VtablePointer, Data, 8),
	Value;

read({ubyte, _Options }, VtablePointer, Data, _) ->
	<<Value:8>> = read_from_data_buffer(VtablePointer, Data, 8),
	Value;

read({short, _Options}, VtablePointer, Data, _) ->
	<<Value:16/little>> = read_from_data_buffer(VtablePointer, Data, 16),
	Value;

read({ushort, _Options}, VtablePointer, Data, _) ->
	<<Value:16/little>> = read_from_data_buffer(VtablePointer, Data, 16),
	Value;

read({int, _Options}, VtablePointer, Data, _) ->
	<<Value:32/little>> = read_from_data_buffer(VtablePointer, Data, 32),
	Value;

read({uint, _Options}, VtablePointer, Data, _) ->
	<<Value:32/little>> = read_from_data_buffer(VtablePointer, Data, 32),
	Value;

read({float, _Options}, VtablePointer, Data, _) ->
	<<Value:32/float-little>> = read_from_data_buffer(VtablePointer, Data, 32),
	Value;

read({long, _Options}, VtablePointer, Data, _) ->
	<<Value:64/little>> = read_from_data_buffer(VtablePointer, Data, 64),
	Value;

read({ulong, _Options}, VtablePointer, Data, _) ->
	<<Value:64/little>> = read_from_data_buffer(VtablePointer, Data, 64),
	Value;

read({double, _Options}, VtablePointer, Data, _) ->
	<<Value:64/float-little>> = read_from_data_buffer(VtablePointer, Data, 64),
	Value;

read({string, _Options}, VtablePointer, Data, _) ->
	<<StringOffset:32/little>> = read_from_data_buffer(VtablePointer, Data, 32),
	StringPointer = VtablePointer + StringOffset,
	<<_:StringPointer/binary, StringLength:32/little, String:StringLength/binary, _/binary>> = Data,
	String;

read({vector, Options}, VtablePointer, Data, Schema) ->
	Type = maps:get(type, Options),
	<<VectorOffset:32/little>> = read_from_data_buffer(VtablePointer, Data, 32),
	VectorPointer = VtablePointer + VectorOffset,
	<<_:VectorPointer/binary, VectorCount:32/little, _/binary>> = Data,
	IsScalar = utils:scalar(Type),
	read_vector_elements(Type, IsScalar, VectorPointer + 4, VectorCount, Data, Schema);

read({enum, #{name := Enum_name}}, VtablePointer, Data, {Tables, _Options} = Schema) ->
	{enum, Options} =  maps:get(Enum_name, Tables),
	Members = maps:get(members, Options),
	Type = maps:get(type, Options),
	Index = read(Type, VtablePointer, Data, Schema),
	case maps:get(Index, Members, undefined) of
		undefined ->
			throw({error, {not_in_enum, Index, Members}});
		Value_atom ->
			erl_types:atom_to_string(Value_atom)
	end;

read({table, #{name := TableName}}, TablePointerPointer, Data, {Tables, _Options} = Schema) when is_atom(TableName) ->
	<<_:TablePointerPointer/binary, TableOffset:32/little, _/binary>> = Data,
	TablePointer = TablePointerPointer + TableOffset,
	{table, Options} = maps:get(TableName, Tables),
	Fields = maps:get(fields, Options),
	<<_:TablePointer/binary, VtableOffset:32/little, _/binary>> = Data,
	VtablePointer = TablePointer - VtableOffset,
	<<_:VtablePointer/binary, VtableLength:16/little, _DataBufferLength:16/little, _/binary>> = Data,
	VtableFieldsPointer = VtablePointer + 4,
	VtableFieldsLength  = VtableLength  - 4,
	<<_:VtableFieldsPointer/binary, Vtable:VtableFieldsLength/binary, _/binary>> = Data,
	DataBufferPointer = TablePointer,
	read_table_fields(Fields, Vtable, DataBufferPointer, Data, Schema);

%% fail if nothing matches
read({Type, _}, _, _, _) ->
	throw({error, {unknown_type, Type}}).

read_vector_elements(_, _, _, 0, _, _) ->
	[];

read_vector_elements(Type, true, VectorPointer, VectorCount, Data, Schema) ->
	Value  = read(Type, VectorPointer, Data, Schema),
	Offset = utils:scalar_size(utils:extract_scalar_type(Type, Schema)),
	[Value | read_vector_elements(Type, true, VectorPointer + Offset, VectorCount - 1, Data, Schema)];

read_vector_elements(Type, false, VectorPointer, VectorCount, Data, Schema) ->
	Value  = read(Type, VectorPointer, Data, Schema),
	Offset = 4,
	[Value | read_vector_elements(Type, false, VectorPointer + Offset, VectorCount - 1, Data, Schema)].

%% this is a utility that just reads data_size bytes from data after data_pointer
read_from_data_buffer(DataPointer, Data, DataSize) ->
	<<_:DataPointer/binary, Value:DataSize/bits, _/binary>> = Data,
	Value.

read_table_fields(Fields, Vtable, DataBufferPointer, Data, Schema) ->
	read_table_fields(Fields, Vtable, DataBufferPointer, Data, Schema, #{}).

%% we might still have more fields but we ran out of vtable slots
%% this happens if the schema has more fields than the data (schema evolution)
read_table_fields(_, <<>>, _, _, _, Map) ->
	Map;

% we might have more data but no more fields
% that means the data is ahead and has more data than the schema
read_table_fields([], _, _, _, _, Map) ->
	Map;

read_table_fields([{Name, {union, #{name := UnionName}}} | Fields], <<DataOffset:16/little, Vtable/binary>>,
	DataBufferPointer, Data, {Tables, _Options} = Schema, Map) ->
% for a union byte field named $fieldname$_type is prefixed
	UnionIndex = read({byte, #{default => 0}}, DataBufferPointer + DataOffset, Data, Schema),
	case UnionIndex of
		0 ->
			% index is null, so field is not set
			% carry on
			read_table_fields(Fields, Vtable, DataBufferPointer, Data, Schema, Map);
		_ ->
% we have a table set so we get the type and
% expect it as the next record in the vtable
			{union, Options} = maps:get(UnionName, Tables),
			Members = maps:get(members, Options),
			UnionType = maps:get(UnionIndex - 1, Members),
			UnionTypeKey = list_to_atom(erl_types:atom_to_string(Name) ++ "_type"),
			MapNew = maps:put(UnionTypeKey, erl_types:atom_to_string(UnionType), Map),
			read_table_fields([{Name, {table, #{name => UnionType}}} | Fields], Vtable, DataBufferPointer, Data, Schema, MapNew)
	end;
% we find a null pointer
% so we set the dafault
read_table_fields([{Name, {enum, Options}} | Fields], <<0, 0, Vtable/binary>>, DataBufferPointer, Data, {Tables, _} = Schema, Map) ->
	{_, EnumOptions} = maps:get(maps:get(name, Options), Tables),
	{_, #{default := Default}} = maps:get(type, EnumOptions),
	MapNew = maps:put(Name, erl_types:atom_to_string(maps:get(Default, maps:get(members, EnumOptions))), Map),
	read_table_fields(Fields, Vtable, DataBufferPointer, Data, Schema, MapNew);

read_table_fields([{Name, {_Type, Options}} | Fields], <<0, 0, Vtable/binary>>, DataBufferPointer, Data, Schema, Map) ->
	MapNew =
	case maps:get(default, Options, undefined) of
		undefined -> Map;
		Default -> maps:put(Name, Default, Map)
	end,
	read_table_fields(Fields, Vtable, DataBufferPointer, Data, Schema, MapNew);

read_table_fields([{Name, Type} | Fields], <<DataOffset:16/little, Vtable/binary>>, DataBufferPointer, Data, Schema, Map) ->
	Value = read(Type, DataBufferPointer + DataOffset, Data, Schema),
	MapNew = maps:put(Name, Value, Map),
	read_table_fields(Fields, Vtable, DataBufferPointer, Data, Schema, MapNew).

