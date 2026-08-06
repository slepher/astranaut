%%% Strict local formatter integration fixture.
-module(macro_local_formatter_strict_test).

-include("quote.hrl").
-include("macro.hrl").

-export([format_error/1, value/0]).

-baseline(yep).

format_error(strict_local_formatter_warning) ->
    strict_local_formatter_message().

strict_local_formatter_message() ->
    "strict local formatter warning".

-local_macro([emit_warning/0]).

value() ->
    emit_warning().

emit_warning() ->
    {warning, quote(ok), strict_local_formatter_warning}.
