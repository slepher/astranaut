-module(compile_meta_identity_transformer).

-export([parse_transform/2]).

parse_transform(Forms, _Opts) ->
    Forms.
