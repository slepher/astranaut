%%%-------------------------------------------------------------------
%%% Pre macro returns are validated locally before traversal descends.
%%%-------------------------------------------------------------------
-module(macro_validator_pre_local_error_test).

-include("quote.hrl").
-include("macro.hrl").

-export([pattern_error/0, helper/0]).

-import_macro(macro_validator_slots).

-baseline(yep).

pattern_error() ->
    macro_validator_slots:pre_pattern_outer().

helper() ->
    ok.
