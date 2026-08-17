-module(macro_local_retain_warning_test).

-include("macro.hrl").

-baseline(yep).

-export([value/0]).
-local_macro([retained_macro/0]).
-local_macro_retain([retained_macro/0, ordinary/0, missing/0]).

value() ->
    ordinary().

ordinary() ->
    ok.

retained_macro() ->
    ok.
