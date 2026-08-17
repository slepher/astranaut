%%% A /2-only formatter must not establish local formatter identity.
-module(macro_local_formatter_only_v2_test).

-include("quote.hrl").
-include("macro.hrl").

-export([format_error/2, value/0]).

-baseline(yep).

format_error(_Error, _Options) ->
    "invalid local only-v2 formatter".

-local_macro([emit_warning/0]).

value() ->
    emit_warning().

emit_warning() ->
    {warning, quote(ok), invalid_macro_attribute}.
