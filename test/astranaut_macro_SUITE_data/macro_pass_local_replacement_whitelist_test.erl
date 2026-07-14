%%%-------------------------------------------------------------------
%%% A local macro replacement introduces another local macro call.  The
%%% entry function's original AST mentions only bridge/0; leaf/0 must be
%%% matched, invoked, and recorded while processing bridge/0's returned AST.
%%%-------------------------------------------------------------------
-module(macro_pass_local_replacement_whitelist_test).

-include("macro.hrl").

-export([value/0]).

-local_macro([leaf/0]).
-local_macro([bridge/0]).
-local_macro([entry/0]).

value() ->
    entry().

entry() ->
    bridge().

bridge() ->
    {call, 0, {atom, 0, leaf}, []}.

leaf() ->
    %% entry/0 is itself compiled as a macro function.  The nested expansion
    %% therefore needs one quotation level for entry/0's runtime return and a
    %% second for the value inserted at its eventual call site.
    astranaut_lib:abstract_form(
      astranaut_lib:abstract_form({replacement_whitelist, ok})).
