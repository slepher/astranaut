-module(macro_pass_closure_roots_missing_error_test).

-include("quote.hrl").
-include("macro.hrl").

-baseline(yep).
-local_macro({[entry/1], [{as_attr, entry},
                          {closure_roots, [{missing_helper, 1}]}]}).

-entry(ok).

value() ->
    entry(ok).

entry(_Ast) ->
    [].
