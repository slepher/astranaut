%%%-------------------------------------------------------------------
%%% Remaining external attribute pass coverage.
%%%-------------------------------------------------------------------
-module(macro_pass_external_remaining_test).

-include("macro.hrl").

-export([alias_value/0]).
-export([alias_attr_value/0]).
-export([chained_attr_value/0]).
-export([pass_generated_value/0]).
-export([non_env_attr_value/0]).
-export([final_external_value/0]).
-export([injected_attrs_value/0]).

-import_macro(macro_uniform_a).
-import_macro(macro_pass_external_helper).

-generate_use_alias(ok).
-generate_alias_consumer(ok).
-generate_attr_chain(ok).
-generate_import_and_attr(ok).
-generate_seen_attr(ok).
-import_macro(macro_pass_inject_attrs).
-pass_seen_attr(source_late).
-generate_late_seen_attr(ok).
-generate_non_env_attr(ok).
-generate_final_function(ok).

alias_value() ->
    generated_to_a(ok).

injected_attrs_value() ->
    macro_pass_inject_attrs:generated_injected_attrs().
