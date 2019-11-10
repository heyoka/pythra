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

simple_func_test() ->
   {ok, P} = pythra:start_link(),
   Ret = pythra:func(P, 'user.pythratest', test, [5]),
   ?assertEqual(5, Ret),
   pythra:stop(P).

simple_func_map_test() ->
   {ok, P} = pythra:start_link(),
   Ret = pythra:func(P, 'user.pythratest', test, [#{<<"val">> => 5}]),
   ?assertEqual(#{<<"val">> => 5}, Ret),
   pythra:stop(P).


simple_class_test() ->
   {ok, P} = pythra:start_link(),
   Obj = pythra:init(P, 'user.pyclass', 'Pyclass', [55]),
   ?assertEqual(55, pythra:method(P, Obj, get_value)),
   pythra:stop(P).

object_map_dict_test() ->
   {ok, P} = pythra:start_link(),
   Obj = pythra:init(P, 'user.pyclass', 'Pyclass', [#{<<"val">> => 55}]),
   ?assertEqual(#{<<"val">> => 55}, pythra:method(P, Obj, get_value)),
   pythra:stop(P).

inheritance_test() ->
   {ok, P} = pythra:start_link(),
   Obj = pythra:init(P, 'user.childclass', 'Child', [55, 99]),
   ?assertEqual(55, pythra:method(P, Obj, get_value1)),
   ?assertEqual(99, pythra:method(P, Obj, get_value2)),
   pythra:stop(P).

inheritance_tuple_test() ->
   {ok, P} = pythra:start_link(),
   Obj = pythra:init(P, 'user.childclass', 'Child', [{55, 44, 33}, {99, 88}]),
   ?assertEqual({55, 44, 33}, pythra:method(P, Obj, get_value1)),
   ?assertEqual({99, 88}, pythra:method(P, Obj, get_value2)),
   pythra:stop(P).

inheritance_map_test() ->
   {ok, P} = pythra:start_link(),
   Obj = pythra:init(P, 'user.childclass', 'Child', [#{<<"val1">> => 55}, #{<<"val2">> => 99}]),
   ?assertEqual(#{<<"val1">> => 55}, pythra:method(P, Obj, get_value1)),
   ?assertEqual(#{<<"val2">> => 99}, pythra:method(P, Obj, get_value2)),
   pythra:stop(P).

inheritance_list_test() ->
   {ok, P} = pythra:start_link(),
   Obj = pythra:init(P, 'user.childclass', 'Child', [[55, 44, 33], [99, 88]]),
   ?assertEqual([55, 44, 33], pythra:method(P, Obj, get_value1)),
   ?assertEqual([99, 88], pythra:method(P, Obj, get_value2)),
   pythra:stop(P).

inheritance_list_of_maps_test() ->
   {ok, P} = pythra:start_link(),
   Obj = pythra:init(P, 'user.childclass', 'Child', [[#{<<"val1">> => 55},#{<<"val1">> => 156}], #{<<"val2">> => 99}]),
   ?assertEqual([#{<<"val1">> => 55},#{<<"val1">> => 156}], pythra:method(P, Obj, get_value1)),
   pythra:stop(P).

