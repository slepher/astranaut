-module(macro_pass_scan_local_attr_test).

-include("quote.hrl").
-include("macro.hrl").

-export([value/0]).

-local_macro({[make_value/1], [as_attr]}).

-make_value(ok).

make_value(_Ast) ->
    Body = quote(ok),
    astranaut_lib:gen_function(value, Body).
