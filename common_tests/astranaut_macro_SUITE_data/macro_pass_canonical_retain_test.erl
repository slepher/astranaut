-module(macro_pass_canonical_retain_test).
-include("macro.hrl").
-export([value/0, second_value/0, ordinary/0, helper/0, entry/0]).
-import_macro(macro_pass_inject_attrs).
-use_macro({macro_pass_inject_attrs, generated_injected_attrs/1, [{alias, snapshot_value}]}).
-pass_seen_attr(early).
-local_macro([entry/0]).
-pass_seen_attr(late).
-local_macro([second/0]).

value() -> entry().
second_value() -> second().
ordinary() -> snapshot_value().
entry() -> astranaut_lib:abstract_form(helper()).
second() -> astranaut_lib:abstract_form(helper()).
helper() -> snapshot_value().
