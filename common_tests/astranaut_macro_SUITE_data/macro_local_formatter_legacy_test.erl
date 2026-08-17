%%% Legacy local formatter integration fixture.
-module(macro_local_formatter_legacy_test).

-include("quote.hrl").
-include("macro.hrl").

-export([format_error/1, value/0]).

-baseline(yep).

format_error(legacy_local_formatter_warning) ->
    "legacy local formatter warning".

-local_macro([emit_warning/0]).

value() ->
    emit_warning().

emit_warning() ->
    {warning, quote(ok), legacy_local_formatter_warning}.
