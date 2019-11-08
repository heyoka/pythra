%%%-------------------------------------------------------------------
%%% @author heyoka
%%% @copyright (C) 2019
%%% @doc
%%%
%%% @end
%%% Created : 08. Nov 2019 20:32
%%%-------------------------------------------------------------------
-module(pythra_tests).
-author("heyoka").

-include_lib("eunit/include/eunit.hrl").

simple_test() ->
   {ok, P} = pythra:start_link(),
   Obj = pythra:init(P, 'user.pyclass', 'Pyclass', [55]),
   ?assertEqual(55, pythra:method(P, Obj, get_value)),
   pythra:stop(P).
