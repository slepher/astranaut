-module(macro_pass_extra_missing_error_test).

-include("quote.hrl").
-include("macro.hrl").

-baseline(yep).
-local_macro({[entry/1], [{as_attr, entry},
                          {extra_functions, [{missing_helper, 1}]}]}).

-entry(ok).

value() ->
    entry(ok).

entry(_Ast) ->
    [].
