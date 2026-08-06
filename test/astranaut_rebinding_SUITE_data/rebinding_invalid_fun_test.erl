-module(rebinding_invalid_fun_test).
-compile({parse_transform, astranaut_rebinding}).
-rebinding_fun(42).
-export([run/0]).

run() -> ok.
