-module(macro_local_declaration_invalid_test).

-include("macro.hrl").

-export([value/0]).

-baseline(yep).

-local_macro({bad, -1}).

value() ->
    ok.
