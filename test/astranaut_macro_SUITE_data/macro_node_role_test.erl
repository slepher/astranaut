%%%-------------------------------------------------------------------
%%% Test macro generating local function call AST. Macro return validation
%%% should be structural and must not reject locally-defined module calls.
%%%-------------------------------------------------------------------
-module(macro_node_role_test).

-include("quote.hrl").
-include("macro.hrl").

-export([test_node_roles/0, helper/0]).

-local_macro([gen_local_call/0]).

test_node_roles() ->
    _B = gen_local_call(),
    ok.

gen_local_call() ->
    quote(helper()).

helper() ->
    ok.
