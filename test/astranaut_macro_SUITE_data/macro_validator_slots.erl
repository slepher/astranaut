%%%-------------------------------------------------------------------
%%% Test macros for validator slot failures after recursive expansion.
%%%-------------------------------------------------------------------
-module(macro_validator_slots).

-include("quote.hrl").
-include("macro.hrl").

-export([format_error/1]).

-export_macro([pattern_outer/0, pattern_inner/0,
               guard_outer/0, guard_inner/0,
               expression_outer/0, expression_inner/0]).

pattern_outer() ->
    quote(
      case ok of
          macro_validator_slots:pattern_inner() -> ok
      end).

pattern_inner() ->
    quote(helper()).

guard_outer() ->
    quote(
      case ok of
          X when macro_validator_slots:guard_inner() -> X
      end).

guard_inner() ->
    quote(helper()).

expression_outer() ->
    quote(macro_validator_slots:expression_inner()).

expression_inner() ->
    {function, 1, foo, 0, [{clause, 1, [], [], [{atom, 1, ok}]}]}.

format_error(Error) ->
    astranaut_macro:format_error(Error).
