%%% External macros used by macro_2000.erl.
-module(macro_bench_macros).

-include("quote.hrl").
-include("macro.hrl").

-export_macro([single/1, outer/1, inner/1, expands_to_single/1]).

single(Ast) ->
    quote({single, unquote(Ast)}).

outer(Ast) ->
    quote({outer, unquote(Ast)}).

inner(Ast) ->
    quote({inner, unquote(Ast)}).

expands_to_single(Ast) ->
    quote(macro_bench_macros:single(unquote(Ast))).
