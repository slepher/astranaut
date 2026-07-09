-module(compile_meta_error_transformer).

-export([parse_transform/2]).

parse_transform(Forms, _Opts) ->
    File = astranaut_lib:analyze_forms_file(Forms),
    {error, [{File, [{4, ?MODULE, injected_error}]}], []}.
