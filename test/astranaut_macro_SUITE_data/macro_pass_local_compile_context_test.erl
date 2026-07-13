%%%-------------------------------------------------------------------
%%% Local macro function forms compile with the forms passed before the
%%% declaration.  Attributes later in the source remain invisible even when
%%% a later attribute triggers on-demand compilation.
%%%-------------------------------------------------------------------
-module(macro_pass_local_compile_context_test).

-include("macro.hrl").

-export([value/0]).

-import_macro(macro_pass_inject_attrs).
-pass_seen_attr(early).

-local_macro({[make_value/1], [as_attr]}).

-pass_seen_attr(late).
-make_value(ok).

make_value(_Ast) ->
    Body = astranaut_lib:abstract_form(
             macro_pass_inject_attrs:generated_injected_attrs()),
    astranaut_lib:gen_function(value, Body).
