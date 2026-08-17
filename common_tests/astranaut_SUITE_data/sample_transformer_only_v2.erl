%%%-------------------------------------------------------------------
%%% Test-only formatter fixture with the invalid only-/2 protocol.
%%%-------------------------------------------------------------------
-module(sample_transformer_only_v2).

-export([format_error/2]).

format_error(_Error, _Options) ->
    "only v2 formatter".
