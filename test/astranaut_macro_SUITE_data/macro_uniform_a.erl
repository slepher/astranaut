%%%-------------------------------------------------------------------
%%% Test macros for uniform macro expansion.
%%%-------------------------------------------------------------------
-module(macro_uniform_a).

-include("quote.hrl").
-include("macro.hrl").

-export([format_error/1]).

-export_macro([to_a/1, gen_b/1, gen_b_to_a/1, direct_b/1, recurse_a/1, fail_after_b/1]).
-export_macro({[outer_capture/1], [{order, outer}]}).

to_a(Ast) ->
    quote({a, {from_a, unquote(Ast)}}).

gen_b(Ast) ->
    quote({a_generated, macro_uniform_b:to_b(unquote(Ast))}).

gen_b_to_a(Ast) ->
    quote({a_generated_chain, macro_uniform_b:gen_a(unquote(Ast))}).

direct_b(Ast) ->
    B = macro_uniform_b:to_b(Ast),
    quote({a_direct, unquote(B)}).

recurse_a({integer, _Pos, N} = Ast) when N > 0 ->
    NextN = N - 1,
    quote({a_recurse, unquote(Ast), macro_uniform_b:recurse_b(_I@NextN)});
recurse_a(Ast) ->
    quote({a_done, unquote(Ast)}).

outer_capture({call, _Pos1,
               {remote, _Pos2, {atom, _Pos3, macro_uniform_b}, {atom, _Pos4, to_b}},
               _Args}) ->
    quote({outer_seen_raw_b_call});
outer_capture(Ast) ->
    quote({outer_seen_other, unquote(Ast)}).

fail_after_b(Ast) ->
    {error, {uniform_a_error, Ast}}.

format_error({uniform_a_error, Ast}) ->
    io_lib:format("uniform macro a failed after receiving ~p", [Ast]);
format_error(Error) ->
    astranaut_macro:format_error(Error).
