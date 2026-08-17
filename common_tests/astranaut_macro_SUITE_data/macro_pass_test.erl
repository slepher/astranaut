%%%-------------------------------------------------------------------
%%% Test generated external macro imports during the external attribute pass.
%%%-------------------------------------------------------------------
-module(macro_pass_test).

-include("macro.hrl").

-export([pass_generated_value/0]).

-import_macro(macro_pass_boot).

-bootstrap_import(ok).
-generated_value(ok).
