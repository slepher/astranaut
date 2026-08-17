%%%-------------------------------------------------------------------
%%% Ordinary wrapper around a function that is also exported as a macro.
%%%-------------------------------------------------------------------
-module(macro_function_helper).

-export([to_a/1]).

to_a(Ast) ->
    macro_uniform_a:to_a(Ast).
