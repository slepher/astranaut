%%% Local providers without format_error/1 warn once across declarations.
-module(macro_missing_formatter_local_test).

-include("quote.hrl").
-include("macro.hrl").

-export([value/0]).

-baseline(yep).

-local_macro([first/0]).
-local_macro([second/0]).

value() ->
    {first(), second()}.

first() ->
    quote({missing_local_first, ok}).

second() ->
    quote({missing_local_second, ok}).
