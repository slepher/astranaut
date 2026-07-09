-module(compile_meta_invalid_transformer).

-export([parse_transform/2]).

parse_transform(_Forms, _Opts) ->
    invalid_return.
