%%%-------------------------------------------------------------------
%%% @author wang yuejia
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. 8月 2021 20:13
%%%-------------------------------------------------------------------
-module(parser).
-author("wang yuejia").

%% API
-export([parse_schema/1]).

parse_schema(SchemaStr) ->
	SchemaStr1 = binary_to_list(SchemaStr),
	List = string:lexemes(SchemaStr1, [[$\r, $\n] |"\t:; "]),
	try do_parse(List, #{}, #{}, [], [], [])
	catch throw : Error -> Error
	end.

do_parse([], Map, RootMap, _, _, _) ->
	{Map, RootMap};
do_parse(["table", Str, "{" | T], Map, RootMap, Enums, Unions, Tables) ->
	TableName = list_to_atom(Str),
	{T1, Map1} = parse_table(T, TableName, [], #{}, 0, Map, Enums, Unions, Tables),
	do_parse(T1, Map1, RootMap, Enums, Unions, [TableName | Tables]);
do_parse(["table", Str, "{}" | T], Map, RootMap, Enums, Unions, Tables) ->
	TableName = list_to_atom(Str),
	Map1 = Map#{TableName => {table, #{fields => [], indices => #{}}}},
	do_parse(T, Map1, RootMap, Enums, Unions, [TableName | Tables]);
do_parse(["table", Str | T], Map, RootMap, Enums, Unions, Tables) ->
	TableName = list_to_atom(string:trim(Str, trailing, "{")),
	{T1, Map1} = parse_table(T, TableName, [], #{}, 0, Map, Enums, Unions, Tables),
	do_parse(T1, Map1, RootMap, Enums, Unions, [TableName | Tables]);
do_parse(["enum", Str1, Str2, "{" | T], Map, RootMap, Enums, Unions, Tables) ->
	EnumName = list_to_atom(Str1),
	{T1, Map1} = parse_enum(T, EnumName, Str2, #{}, 0, Map),
	do_parse(T1, Map1, RootMap, [EnumName | Enums], Unions, Tables);
do_parse(["enum", Str1, Str2 | T], Map, RootMap, Enums, Unions, Tables) ->
	EnumName = list_to_atom(Str1),
	Str3 = string:trim(Str2, trailing, "{"),
	{T1, Map1} = parse_enum(T, EnumName, Str3, #{}, 0, Map),
	do_parse(T1, Map1, RootMap, [EnumName | Enums], Unions, Tables);
do_parse(["union", Str1, "{" | T], Map, RootMap, Enums, Unions, Tables) ->
	UnionName = list_to_atom(Str1),
	{T1, Map1, UnionLength} = parse_union(T, UnionName, #{}, 0, Map),
	do_parse(T1, Map1, RootMap, Enums, [{UnionName, UnionLength} | Unions], Tables);
do_parse(["union", Str1 | T], Map, RootMap, Enums, Unions, Tables) ->
	UnionName = list_to_atom(string:trim(Str1, trailing, "{")),
	{T1, Map1, UnionLength} = parse_union(T, UnionName, #{}, 0, Map),
	do_parse(T1, Map1, RootMap, Enums, [{UnionName, UnionLength} | Unions], Tables);
do_parse(["root_type", Str | T], Map, RootMap, Enums, Unions, Tables) ->
	do_parse(T, Map, RootMap#{root_type => list_to_atom(Str)}, Enums, Unions, Tables);
do_parse(["file_identifier", Str | T], Map, RootMap, Enums, Unions, Tables) ->
	do_parse(T, Map, RootMap#{file_identifier => string:trim(Str, both, "\"")}, Enums, Unions, Tables);
do_parse(["namespace", Str | T], Map, RootMap, Enums, Unions, Tables) ->
	do_parse(T, Map, RootMap#{namespace => list_to_atom(Str)}, Enums, Unions, Tables);
do_parse(["include", Str | T], Map, RootMap, Enums, Unions, Tables) ->
	do_parse(T, Map, RootMap#{include => string:trim(Str, both, "\"")}, Enums, Unions, Tables);
do_parse(["attribute", Str | T], Map, RootMap, Enums, Unions, Tables) ->
	do_parse(T, Map, RootMap#{attribute => string:trim(Str, both, "\"")}, Enums, Unions, Tables);
do_parse(["file_extension", Str | T], Map, RootMap, Enums, Unions, Tables) ->
	do_parse(T, Map, RootMap#{file_extension => string:trim(Str, both, "\"")}, Enums, Unions, Tables);
do_parse(T, _, _, _, _, _) ->
	throw({error, "error type:", T}).

parse_table(["}" | T], TableName, Fields, Indices, _Order, Map, _Enums, _Unions, _Tables) ->
	Data = {table, #{fields => lists:reverse(Fields), indices => Indices}},
	{T, Map#{TableName => Data}};
parse_table([Str1, Str2, "=", Default| T], TableName, Fields, Indices, Order, Map, Enums, Unions, Tables) ->
	{FieldType, Length} = parse_type(Str2, Enums, Unions, Tables, Default),
	{Fields1, Indices1} = parse_table_field(Str1, FieldType, Fields, Indices, Order),
	parse_table(T, TableName, Fields1, Indices1, Order + Length, Map, Enums, Unions, Tables);
parse_table([Str1, Str2, "(deprecated)" | T], TableName, Fields, Indices, Order, Map, Enums, Unions, Tables) ->
	{FieldType, Length} = parse_type(Str2, Enums, Unions, Tables),
	{Fields1, Indices1} = parse_table_field(Str1, FieldType, Fields, Indices, Order),
	parse_table(T, TableName, Fields1, Indices1, Order + Length, Map, Enums, Unions, Tables);
parse_table([Str1, Str2 | T], TableName, Fields, Indices, Order, Map, Enums, Unions, Tables) ->
	{FieldType, Length} = parse_type(Str2, Enums, Unions, Tables),
	{Fields1, Indices1} = parse_table_field(Str1, FieldType, Fields, Indices, Order),
	parse_table(T, TableName, Fields1, Indices1, Order + Length, Map, Enums, Unions, Tables).

parse_table_field(Str, FieldType, Fields, Indices, Order) ->
	FieldName = list_to_atom(Str),
	Fields1 = [{FieldName, FieldType} | Fields],
	Indices1 = Indices#{FieldName => {Order, FieldType}},
	{Fields1, Indices1}.

parse_enum(["}" | T], EnumName, EnumType, Members, _Order, Map) ->
	{Type, _} = parse_type(EnumType, [], [], []),
	Data = {enum, #{members => Members, type => Type}},
	{T, Map#{EnumName => Data}};
parse_enum([Enum | T], EnumName, EnumType, Members, Order, Map) ->
	EnumAtom = list_to_atom(string:lowercase(Enum)),
	Members1 = Members#{Order => EnumAtom, EnumAtom => Order},
	parse_enum(T, EnumName, EnumType, Members1, Order + 1, Map).

parse_union(["}" | T], EnumName, Members, Order, Map) ->
	Data = {union, #{members => Members}},
	{T, Map#{EnumName => Data}, Order};
parse_union([Enum | T], UnionName, Members, Order, Map) ->
	EnumAtom = list_to_atom(Enum),
	Members1 = Members#{Order => EnumAtom, EnumAtom => Order},
	parse_union(T, UnionName, Members1, Order + 1, Map).

parse_type("bool", _, _, _) -> {{bool, #{default => false}}, 1};
parse_type("byte", _, _, _) -> {{byte, #{default => 0}}, 1};
parse_type("ubyte", _, _, _) -> {{ubyte, #{default => 0}}, 1};
parse_type("short", _, _, _) -> {{short, #{default => 0}}, 1};
parse_type("ushort", _, _, _) -> {{ushort, #{default => 0}}, 1};
parse_type("int", _, _, _) -> {{int, #{default => 0}}, 1};
parse_type("uint", _, _, _) -> {{uint, #{default => 0}}, 1};
parse_type("float", _, _, _) -> {{float, #{default => 0}}, 1};
parse_type("long", _, _, _) -> {{long, #{default => 0}}, 1};
parse_type("ulong", _, _, _) -> {{ulong, #{default => 0}}, 1};
parse_type("double", _, _, _) -> {{double, #{default => 0}}, 1};
parse_type("string", _, _, _) -> {{string, #{}}, 1};
parse_type(Type, Enums, Unions, Tables) ->
	case check_type_invaild(Type) of
		{atom, Type1} ->
			parse_complex_type(Type1, Enums, Unions, [Type1 | Tables]);
		{list, Type1} ->
			{TypeData, _} = parse_complex_type(Type1, Enums, Unions, Tables),
			{{vector, #{type => TypeData}}, 1};
		_ -> throw({error, "string is nomatch:", Type})
	end.

check_type_invaild(Type) ->
	case re:run(Type, "^[a-zA-Z]\\w*[a-zA-Z0-9]$") of
		nomatch ->
			case re:run(Type, "^\\[[a-zA-Z]\\w*[a-zA-Z0-9]\\]$") of
				nomatch ->
					nomatch;
				_ ->
					{list, list_to_atom(string:trim(Type, both, "[]"))}
			end;
		_ ->
			{atom, list_to_atom(Type)}
	end.

parse_type("bool", _, _, _, Default) -> {{bool, #{default => list_to_atom(Default)}}, 1};
parse_type("byte", _, _, _, Default) -> {{byte, #{default => list_to_integer(Default)}}, 1};
parse_type("ubyte", _, _, _, Default) -> {{ubyte, #{default => list_to_integer(Default)}}, 1};
parse_type("short", _, _, _, Default) -> {{short, #{default => list_to_integer(Default)}}, 1};
parse_type("ushort", _, _, _, Default) -> {{ushort, #{default => list_to_integer(Default)}}, 1};
parse_type("int", _, _, _, Default) -> {{int, #{default => list_to_integer(Default)}}, 1};
parse_type("uint", _, _, _, Default) -> {{uint, #{default => list_to_integer(Default)}}, 1};
parse_type("float", _, _, _, Default) -> {{float, #{default => list_to_float(Default)}}, 1};
parse_type("long", _, _, _, Default) -> {{long, #{default => list_to_integer(Default)}}, 1};
parse_type("ulong", _, _, _, Default) -> {{ulong, #{default => list_to_integer(Default)}}, 1};
parse_type("double", _, _, _, Default) -> {{double, #{default => list_to_float(Default)}}, 1};
parse_type("string", _, _, _, _) -> {{string, #{}}, 1};
parse_type(Type, Enums, Unions, Tables, _) -> parse_type(Type, Enums, Unions, Tables).

parse_complex_type(Type, Enums, Unions, Tables) ->
	parse_complex_type1(Type, Enums, Unions, Tables).

parse_complex_type1(Type, Enums, Unions, Tables) ->
	case lists:member(Type, Enums) of
		true ->
			{{enum, #{name => Type}}, 1};
		_ ->
			parse_complex_type2(Type, Enums, Unions, Tables)
	end.
parse_complex_type2(Type, Enums, Unions, Tables) ->
	case lists:keyfind(Type, 1, Unions) of
		{_, Length} ->
			{{union, #{name => Type}}, Length};
		_ ->
			parse_complex_type3(Type, Enums, Unions, Tables)
	end.
parse_complex_type3(Type, Enums, Unions, Tables) ->
	case lists:member(Type, Tables) of
		true ->
			{{table, #{name => Type}}, 1};
		_ ->
			parse_type(atom_to_list(Type), Enums, Unions, Tables)
	end.
