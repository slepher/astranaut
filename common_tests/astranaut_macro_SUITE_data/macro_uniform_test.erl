%%%-------------------------------------------------------------------
%%% Test module for uniform macro expansion.
%%%-------------------------------------------------------------------
-module(macro_uniform_test).

-include("quote.hrl").
-include("macro.hrl").

-export([later_generates_earlier/0]).
-export([earlier_generates_later/0]).
-export([nested_external/0]).
-export([outer_preserves_raw_child/0]).
-export([generated_chain/0]).
-export([direct_macro_function_call/0]).
-export([attribute_generated_local_macro/0]).
-export([local_generates_external/0]).

-import_macro(macro_uniform_a).
-import_macro(macro_uniform_b).

-inject_generated_local_macro(ok).

-local_macro([local_to_external/0]).

later_generates_earlier() ->
    macro_uniform_b:gen_a(ok).

earlier_generates_later() ->
    macro_uniform_a:gen_b(ok).

nested_external() ->
    macro_uniform_a:to_a(macro_uniform_b:to_b(ok)).

outer_preserves_raw_child() ->
    macro_uniform_a:outer_capture(macro_uniform_b:to_b(ok)).

generated_chain() ->
    macro_uniform_a:gen_b_to_a(ok).

direct_macro_function_call() ->
    macro_uniform_a:direct_b(ok).

attribute_generated_local_macro() ->
    generated_local_macro().

local_generates_external() ->
    local_to_external().

local_to_external() ->
    quote(macro_uniform_a:to_a(ok)).
