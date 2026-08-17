%%%-------------------------------------------------------------------
%%% Test errors from -use_macro declarations.
%%%-------------------------------------------------------------------
-module(macro_use_error_test).

-include("macro.hrl").

-baseline(yep).

-import_macro(macro_uniform_a).
-use_macro({macro_uniform_a, missing_export/1}).
-use_macro({missing_local/0}).
-use_macro({[{bad_arity, -1}], []}).

ok() ->
    ok.
