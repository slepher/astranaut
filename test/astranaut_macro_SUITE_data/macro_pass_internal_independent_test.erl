%%%-------------------------------------------------------------------
%%% Different internal_function lists compile when closures do not overlap.
%%%-------------------------------------------------------------------
-module(macro_pass_internal_independent_test).

-include("quote.hrl").
-include("macro.hrl").

-export([value_a/0, value_b/0]).

-local_macro({[macro_a/1], [{as_attr, macro_a},
                            {internal_function, [{helper_a, 1}]}]}).
-local_macro({[macro_b/1], [{as_attr, macro_b},
                            {internal_function, [{helper_b, 1}]}]}).

-macro_a(ok).
-macro_b(ok).

value_a() ->
    generated_a().

value_b() ->
    generated_b().

macro_a(Ast) ->
    helper_a(Ast).

macro_b(Ast) ->
    helper_b(Ast).

helper_a(_Ast) ->
    astranaut_lib:gen_function(generated_a,
                               astranaut_lib:abstract_form({internal_independent_a, ok})).

helper_b(_Ast) ->
    astranaut_lib:gen_function(generated_b,
                               astranaut_lib:abstract_form({internal_independent_b, ok})).
