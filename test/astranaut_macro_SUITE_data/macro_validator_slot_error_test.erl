%%%-------------------------------------------------------------------
%%% Invalid macro return test module for validator slot failures.
%%%-------------------------------------------------------------------
-module(macro_validator_slot_error_test).

-include("quote.hrl").
-include("macro.hrl").

-export([pattern_error/0, guard_error/0, expression_error/0, helper/0]).

-import_macro(macro_validator_slots).

-baseline(yep).

pattern_error() ->
    macro_validator_slots:pattern_outer().

guard_error() ->
    macro_validator_slots:guard_outer().

expression_error() ->
    macro_validator_slots:expression_outer().

helper() ->
    ok.
