%%%-------------------------------------------------------------------
%%% Test macros for generated macro_options during the external pass.
%%%-------------------------------------------------------------------
-module(macro_pass_depth).

-include("quote.hrl").
-include("macro.hrl").

-export_macro([chain_a/0, chain_b/0]).

chain_a() ->
    quote(macro_pass_depth:chain_b()).

chain_b() ->
    quote(depth_done).
