%%% Strict local formatter integration fixture.
-module(macro_local_formatter_strict_test).

-include("quote.hrl").
-include("macro.hrl").

-export([format_error/1, format_error/2, value/0]).

-baseline(yep).

format_error(Error) ->
    format_error(Error, #{}).

format_error(Error, Options) ->
    astranaut_lib:format_error(
      Error, Options, fun(strict_local_formatter_warning) -> format_error_1(strict_local_formatter_warning) end,
      fun astranaut_macro:format_error/2).

format_error_1(strict_local_formatter_warning) ->
    strict_local_formatter_message().

strict_local_formatter_message() ->
    "strict local formatter warning".

-local_macro([emit_warning/0]).

value() ->
    emit_warning().

emit_warning() ->
    {warning, quote(ok), strict_local_formatter_warning}.
