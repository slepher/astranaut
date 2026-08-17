%%%-------------------------------------------------------------------
%%% local_macro and export_macro are independent declarations on the same FA.
%%%-------------------------------------------------------------------
-module(macro_pass_export_and_local_test).

-include("macro.hrl").

-export([local_value/0]).
-local_macro([shared/0]).
-export_macro([shared/0]).

local_value() ->
    shared().

shared() ->
    astranaut_lib:abstract_form(shared_macro).
