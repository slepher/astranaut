-module(macro_pass_generated_options_error_test).

-include("quote.hrl").
-include("macro.hrl").

-baseline(yep).
-export([value/0]).

-import_macro(macro_pass_boot).

-bootstrap_depth_limit(ok).

value() ->
    macro_pass_depth:chain_a().
