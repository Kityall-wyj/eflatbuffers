%%%-------------------------------------------------------------------
%%% @author kityall_wyj
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. 8月 2021 11:24
%%%-------------------------------------------------------------------
-module(test).
-author("kityall_wyj").

%% API
-export([test/0]).
test() ->
	Map = #{my_string => <<"hello world!">>, my_bool => true},
	Schema = load_schema(string_table),
	{ok, Bin} = eflatbuffers:write(Map, Schema),
	{ok, Map} = eflatbuffers:read(Bin, Schema).

load_schema(Name) ->
	{ok, Schema} = file:read_file("test/schemas/" ++ atom_to_list(Name) ++ ".fbs"),
	Schema.
