-module(macro_options_local_options_warning_test).

-include("macro.hrl").

-baseline(yep).

-macro_options([{closure_roots, [{helper, 0}]},
                {internal_function, true}]).

-export([value/0]).

value() ->
    ok.
