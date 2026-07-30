%%%-------------------------------------------------------------------
%%% closure_roots can add helpers missed by static call scanning.
%%%-------------------------------------------------------------------
-module(macro_pass_closure_roots_test).

-include("quote.hrl").
-include("macro.hrl").

-export([value/0]).

-local_macro({[entry/1], [{closure_roots, [{hidden_helper, 1}]}]}).

value() ->
    entry(ok).

entry(Ast) ->
    Fun = fun hidden_helper/1,
    Fun(Ast).

hidden_helper(Ast) ->
    quote({extra_helper, unquote(Ast)}).
