%%%-------------------------------------------------------------------
%%% A helper name outside the macro environment keeps function-call intent
%%% distinct from direct macro invocation syntax.
%%%-------------------------------------------------------------------
-module(macro_pass_function_helper_test).

-include("macro.hrl").

-export([value/0]).

-import_macro(macro_uniform_a).
-local_macro([outer/1]).

value() ->
    outer(ok).

outer(Ast) ->
    macro_function_helper:to_a(Ast).
