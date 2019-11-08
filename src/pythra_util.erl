%%%-------------------------------------------------------------------
%%% @author heyoka
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Nov 2019 10:41
%%%-------------------------------------------------------------------
-module(pythra_util).
-author("heyoka").

%% API
-export([proplist_to_binary/1, split_last/1, split_dotted/1, is_dotted/1]).

%% @doc convert all the keys to binary
proplist_to_binary(Proplist) ->
   F = fun
          ({Key, Val}) when is_atom(Key) -> {atom_to_binary(Key, latin1), Val};
          ({Key, Val}) when is_list(Key) -> {list_to_binary(Key), Val}
       end,
   lists:map(F, Proplist).

-spec split_last(list()) -> tuple().
split_last(Args) when is_list(Args) ->
   lists:split(length(Args)-1, Args).

split_dotted(DotName) ->
   Parts = string:tokens(atom_to_list(DotName), "."),
   {First, [Last]} = split_last(Parts),
   lists:map(
      fun(E) -> list_to_atom(E) end,
      [string:join(First, "."), Last]).

is_dotted(Name) ->
   length(string:tokens(atom_to_list(Name), ".")) > 1.
