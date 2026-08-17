%%%-------------------------------------------------------------------
%%% External macro exception and returned diagnostic ownership coverage.
%%%-------------------------------------------------------------------
-module(macro_error_external_test).

-include("macro.hrl").

-export([run/0]).

-baseline(yep).

-import_macro(macro_error_external_provider).

run() ->
    {macro_error_external_provider:raise(),
     macro_error_external_provider:return_error(),
     macro_error_external_provider:return_warning()}.
