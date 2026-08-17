-module(macro_local_internal_option_warning_test).

-include("macro.hrl").

-baseline(yep).

-local_macro({entry/0, [{internal_function, true}]}).

entry() ->
    {atom, 0, ok}.
