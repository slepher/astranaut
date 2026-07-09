-module(compile_meta_warning_transformer).

-export([parse_transform/2]).

parse_transform(Forms, _Opts) ->
    File = astranaut_lib:analyze_forms_file(Forms),
    {warning, Forms, [{File, [{3, ?MODULE, injected_warning}]}]}.
