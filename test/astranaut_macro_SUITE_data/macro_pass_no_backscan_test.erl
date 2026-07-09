%%%-------------------------------------------------------------------
%%% Earlier attributes must not be rescanned after a later import.
%%%-------------------------------------------------------------------
-module(macro_pass_no_backscan_test).

-include("macro.hrl").

-export([pass_generated_value/0]).

-import_macro(macro_pass_boot).

-generated_value(before_import).
-bootstrap_import(ok).
-generated_value(after_import).
