%%%-------------------------------------------------------------------
%%% @author kityall_wyj
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. 7月 2021 17:39
%%%-------------------------------------------------------------------
-module(flatbuffers).
-author("kityall_wyj").

-export([write/2, read/2]).

write(Map, SchemaStr) when is_binary(SchemaStr)->
	write(Map, parser:parse_schema(SchemaStr));
write(Map, {_, #{root_type := RootType} = Options} = Schema) ->
	RootTable = [<<VtableOffset:16/little>> | _] = writer:write({table, #{name => RootType}}, Map, [], Schema),
	FileIdentifier =
		case maps:get(file_identifier, Options, undefined) of
			<<Bin:32>> -> <<Bin:32>>;
			_ -> <<0, 0, 0, 0>>
		end,
	L = [<<(VtableOffset + 4 + byte_size(FileIdentifier)):32/little>>, FileIdentifier, RootTable],
	{ok, iolist_to_binary(L)}.

read(Data, SchemaStr) when is_binary(SchemaStr) ->
	read(Data, parser:parse_schema(SchemaStr));
read(Data, {_, SchemaOptions = #{root_type := RootType}} = Schema) ->
	match_identifiers(Data, SchemaOptions),
	{ok, reader:read({table, #{name => RootType}}, 0, Data, Schema)}.

match_identifiers(<<_:4/binary, IdentifierData:4/binary, _/binary>>, SchemaOptions) ->
	case maps:get(file_identifier, SchemaOptions, undefined) of
		undefined -> ok;
		IdentifierData -> ok;
		IdentifierSchema -> throw({error, {identifier_mismatch, #{data => IdentifierData, schema => IdentifierSchema}}})
	end.

