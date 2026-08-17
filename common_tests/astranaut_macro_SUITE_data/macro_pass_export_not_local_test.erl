%%%-------------------------------------------------------------------
%%% An exported macro is callable as a macro only after another module imports
%%% it.  In its defining module, an unqualified call stays an Erlang call.
%%%-------------------------------------------------------------------
-module(macro_pass_export_not_local_test).

-include("macro.hrl").

-export([value/0]).
-export_macro([exported_only/0]).

value() ->
    exported_only().

exported_only() ->
    astranaut_lib:abstract_form(exported_only).
