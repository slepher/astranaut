%%% A /2-only formatter is deliberately not a macro formatter protocol.
-module(macro_only_v2_formatter_provider).

-include("quote.hrl").
-include("macro.hrl").

-export([format_error/2]).
-export_macro([emit_v2/0]).

format_error(_Error, _Options) ->
    "invalid only-v2 formatter".

emit_v2() ->
    quote({only_v2_external, ok}).
