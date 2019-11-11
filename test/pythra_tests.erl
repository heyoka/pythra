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
   ?assertEqual({"Map",[{<<"val">>,5}]}, Ret),
   pythra:stop(P).


simple_class_test() ->
   {ok, P} = pythra:start_link(),
   Obj = pythra:init(P, 'user.pyclass', 'Pyclass', [55]),
   ?assertEqual(55, pythra:method(P, Obj, get_value)),
   pythra:stop(P).

object_map_dict_test() ->
   {ok, P} = pythra:start_link(),
   Obj = pythra:init(P, 'user.pyclass', 'Pyclass', [#{<<"val">> => 55}]),
   ?assertEqual({"Map",[{<<"val">>,55}]}, pythra:method(P, Obj, get_value)),
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
   ?assertEqual({"Map",[{<<"val1">>,55}]}, pythra:method(P, Obj, get_value1)),
   ?assertEqual({"Map",[{<<"val2">>,99}]}, pythra:method(P, Obj, get_value2)),
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

inheritance_list_of_maps2_test() ->
   {ok, P} = pythra:start_link(),
   LMap = [#{<<"df">> => <<"02.005">>,<<"id">> => <<"oi23u4oi23u4oi32u34oi2u3">>,<<"ts">> => 1573409579742,
      <<"val">> => 6.987905296554468,<<"vs">> => 1},#{<<"df">> => <<"02.005">>,
      <<"id">> => <<"oi23u4oi23u4oi32u34oi2u3">>,<<"ts">> => 1573409580743,
      <<"val">> => 8.785576710041802,<<"vs">> => 1},#{<<"df">> => <<"02.005">>,
      <<"id">> => <<"oi23u4oi23u4oi32u34oi2u3">>,<<"ts">> => 1573409581744,<<"val">> => 1.8826815278288,<<"vs">> => 1}],
   Obj = pythra:init(P, 'user.childclass', 'Child', [LMap, #{<<"val2">> => 99}]),
   ?assertEqual(LMap, pythra:method(P, Obj, get_value1)),
   pythra:stop(P).


