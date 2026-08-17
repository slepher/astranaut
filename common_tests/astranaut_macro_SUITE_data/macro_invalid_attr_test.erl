%%%-------------------------------------------------------------------
%%% Test errors from invalid macro attribute bodies.
%%%-------------------------------------------------------------------
-module(macro_invalid_attr_test).

-include("macro.hrl").

-baseline(yep).

-use_macro(foo).
-local_macro(bar).

ok() ->
    ok.
