-module(rebinding_invalid_option_test).
-compile({parse_transform, astranaut_rebinding}).
-rebinding_all([{strict, invalid}]).
-export([run/0]).

run() -> ok.
