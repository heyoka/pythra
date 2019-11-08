%%%-------------------------------------------------------------------
%%% @author heyoka
%%% @copyright (C) 2019
%%% @doc
%%% for docs see https://github.com/lfex/py
%%% @end
%%% Created : 08. Nov 2019 11:14
%%%-------------------------------------------------------------------
-module(pythra).
-author("heyoka").

%% API
-export([start_link/0]).

-export([
   pythra_call/3, pythra_call/4,
   init/2, init/3, init/4, init/5,
   const/2, const/3, const/4,
   attr/3,
   method/3, method/4, method/5,
   func/2, func/3, func/4, func/5,
   general_call/6]).


start_link() ->
   Version = "3",
   Path = "./python/",
   Opts = [{python_path, Path}, {python, "python"++Version}],
   {ok, Py} = python:start_link(Opts),
   on_start(Py),
   {ok, Py}.

on_start(ProcPid) ->
   python:call(ProcPid, pythra, 'init.setup', []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% call functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% erlport calls

pythra_call(Python, Mod, Func) ->
   pythra_call(Python, Mod, Func, []).

pythra_call(Python, Mod, Func, Args) ->
   python:call(Python, Mod, Func, Args).

%%% Creating Python class instances
%%%
init(Python, ModClass) ->
   [Mod, Class] = pythra_util:split_dotted(ModClass),
   init(Python, Mod, Class).

init(Python, ModClass, Args) when is_list(Args) ->
   [Module, Class] = pythra_util:split_dotted(ModClass),
   init(Python, Module, Class, Args, []);
init(Python, Module, Class) ->
   init(Python, Module, Class, [], []).

init(Python, ModClass, Args, KwArgs) when is_list(Args) ->
   [Module, Class] = pythra_util:split_dotted(ModClass),
   init(Python, Module, Class, Args, KwArgs);
init(Python, Module, Class, Args) ->
   init(Python, Module, Class, Args, []).

init(Python, Module, Class, Args, KwArgs) ->
   func(Python, Module, Class, Args, KwArgs).

%%% Python object and module constants
const(Python, ModAttr) ->
   [Module, AttrName] = pythra_util:split_dotted(ModAttr),
   const(Python, Module, AttrName).

const(Python, Mod, AttrName) when is_atom(Mod) ->
   Attr = atom_to_binary(AttrName, latin1),
   pythra_call(Python, pythra, 'obj.const', [Mod, Attr]);
const(Python, Obj, Type) ->
   method(Python, Obj, list_to_atom("__" ++ atom_to_list(Type) ++ "__")).

const(Python, Mod, Func, Type) ->
   Call = list_to_atom(atom_to_list(Func) ++ ".__" ++ atom_to_list(Type) ++ "__"),
   pythra_call(Python, Mod, Call).

%%% Python object attributes
%%
attr(Python, Obj, AttrName) when is_list(AttrName) ->
   attr(Python, Obj, list_to_atom(AttrName));
attr(Python, Obj, AttrName) when is_atom(AttrName) ->
   Attr = atom_to_binary(AttrName, latin1),
   pythra_call(Python, pythra, 'obj.attr', [Obj, Attr]).

%%% Python method calls
%%
method(Python, Obj, MethodName) ->
   method(Python, Obj, MethodName, [], []).

method(Python, Obj, MethodName, Args) ->
   method(Python, Obj, MethodName, Args, []).

method(Python, Obj, MethodName, Args, KwArgs) ->
   general_call(Python, Obj, MethodName, Args, KwArgs, 'obj.call_method').

%%call_dotten(FuncName) ->

%%% Python module function and function object calls
%%
func(Python, FuncName) when is_atom(FuncName) ->
   case pythra_util:is_dotted(FuncName) of
      true ->
         [Func, FuncName1] = pythra_util:split_dotted(FuncName),
         func(Python, Func, FuncName1);
      false ->
         func(Python, FuncName, [], [])
   end;
func(Python, Callable) ->
   func(Python, Callable, [], []).

func(Python, FuncName, Args) when is_atom(FuncName), is_list(Args) ->
   case pythra_util:is_dotted(FuncName) of
      true ->
         [Func, FuncName1] = pythra_util:split_dotted(FuncName),
         func(Python, Func, FuncName1, [], []);

      false -> func(Python, FuncName, Args, [])
   end;
func(Python, Module, FuncName) when is_atom(Module) ->
   func(Python, Module, FuncName, [], []);
func(Python, Callable, Args) ->
   func(Python, Callable, Args, []).

func(Python, FuncName, Args, RawKwArgs) when is_atom(FuncName) andalso is_list(Args) ->
   case pythra_util:is_dotted(FuncName) of
      true ->
         [Func, FuncName1] = pythra_util:split_dotted(FuncName),
         func(Python, Func, FuncName1, Args, RawKwArgs);
      false ->
         %% now call to the call_callable function in the python module 'pythra.obj'
         KwArgs = pythra_util:proplist_to_binary(RawKwArgs),
         pythra_call(Python, pythra, 'obj.call_callable', [FuncName, Args, KwArgs])

   end;
func(Python, Module, FuncName, Args) when is_atom(Module) ->
   func(Python, Module, FuncName, Args);
func(Python, Callable, Args, RawKwArgs) ->
   KwArgs = pythra_util:proplist_to_binary(RawKwArgs),
   pythra_call(Python, pythra, 'obj.call_callable', [Callable, Args, KwArgs]).

func(Python, Module, FuncName, Args, KwArgs) ->
   %% Now call to the call_func function in the Python module 'pythra.obj'
   general_call(Python, atom_to_binary(Module, latin1), FuncName, Args, KwArgs, 'obj.call_func').

general_call(Python, Obj, AttrName, Args, RawKwArgs, Type) ->
   Attr = atom_to_binary(AttrName, latin1),
   KwArgs = pythra_util:proplist_to_binary(RawKwArgs),
   pythra_call(Python, pythra, Type, [Obj, Attr, Args, KwArgs]).