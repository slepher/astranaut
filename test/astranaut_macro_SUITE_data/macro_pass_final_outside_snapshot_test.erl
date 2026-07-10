%%%-------------------------------------------------------------------
%%% Final expansion still visits generated code outside the locked snapshot.
%%%-------------------------------------------------------------------
-module(macro_pass_final_outside_snapshot_test).

-include("quote.hrl").
-include("macro.hrl").

-export([local_value/0, final_external_value/0]).

-import_macro(macro_uniform_a).
-import_macro(macro_pass_external_helper).

-generate_final_function(ok).

-local_macro([entry/0]).

local_value() ->
    entry().

entry() ->
    quote({locked_snapshot, ok}).
