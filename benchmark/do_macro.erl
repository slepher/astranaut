%%% Minimal Erlando-compatible do macro provider for the real benchmark.
-module(do_macro).

-compile({parse_transform, astranaut_macro}).

-export_macro([do/1]).
-export([format_error/1]).

do(Ast) ->
    astranaut_do:do(
      Ast,
      #{monad => monad,
        monad_fail => monad_fail}).

format_error(Reason) ->
    astranaut_do:format_error(Reason).
