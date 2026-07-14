%%%-------------------------------------------------------------------
%%% An external macro introduces the first reference to a pending local macro.
%%% The local dependency must be compiled on demand before expansion retries.
%%%-------------------------------------------------------------------
-module(macro_pass_external_replacement_local_test).

-include("macro.hrl").

-export([value/0]).

-import_macro(macro_uniform_a).

-local_macro([leaf/0]).
-local_macro([entry/0]).

value() ->
    entry().

entry() ->
    macro_uniform_a:emit_local_call(leaf).

leaf() ->
    astranaut_lib:abstract_form(
      astranaut_lib:abstract_form({external_replacement_local, ok})).
