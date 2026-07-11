%%%-------------------------------------------------------------------
%%% A stateful function macro must not overwrite the function-pass State.
%%%-------------------------------------------------------------------
-module(macro_pass_scoped_function_state_test).

-include("macro.hrl").

-export([value/0]).

-import_macro(macro_pass_external_helper).

value() ->
    macro_pass_external_helper:stateful_function().
