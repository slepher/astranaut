-module(compile_meta_crash_transformer).

-export([parse_transform/2]).

parse_transform(_Forms, _Opts) ->
    erlang:error(injected_crash).
