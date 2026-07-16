-module(macro_options_local_options_warning_test).

-include("macro.hrl").

-baseline(yep).

-macro_options([{extra_functions, [{helper, 0}]},
                {internal_function, true}]).

-export([value/0]).

value() ->
    ok.
