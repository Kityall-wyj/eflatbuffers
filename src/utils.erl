%%%-------------------------------------------------------------------
%%% @author wang yuejia
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. 7月 2021 20:16
%%%-------------------------------------------------------------------
-module(utils).
-author("wang yuejia").

%% API
-export([scalar/1, scalar_size/1, extract_scalar_type/2]).

scalar({Type , _Options }) -> scalar(Type);
scalar(string) -> false;
scalar(vector) -> false;
scalar(table) -> false;
scalar(enum) -> true;
scalar(_) -> true.

scalar_size({Type, _Options }) -> scalar_size(Type);
scalar_size(byte ) -> 1;
scalar_size(ubyte) -> 1;
scalar_size(bool ) -> 1;
scalar_size(short ) -> 2;
scalar_size(ushort) -> 2;
scalar_size(int  ) -> 4;
scalar_size(uint ) -> 4;
scalar_size(float) -> 4;
scalar_size(long  ) -> 8;
scalar_size(ulong ) -> 8;
scalar_size(double) -> 8;
scalar_size(Type) -> throw({error, {unknown_scalar, Type}}).

extract_scalar_type({enum, #{name := EnumName }}, {Tables, _Options}) ->
	{enum, #{type := Type }} =  maps:get(EnumName, Tables),
	Type;
extract_scalar_type(Type, _) -> Type.

