%%%-------------------------------------------------------------------
%%% Test macros generating various erl_syntax node types
%%% to verify node_roles validation in expression context.
%%%-------------------------------------------------------------------
-module(macro_node_roles).

-include("quote.hrl").
-include("macro.hrl").

-export_macro([gen_lc/0]).

gen_lc() ->
    quote([X || X <- [1, 2, 3]]).

