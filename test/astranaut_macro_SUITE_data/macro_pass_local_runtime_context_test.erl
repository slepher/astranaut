%%%-------------------------------------------------------------------
%%% Local and external attribute macros share the same call-site runtime rule.
%%%-------------------------------------------------------------------
-module(macro_pass_local_runtime_context_test).

-include("macro.hrl").

-export([value/0]).

-local_macro({[make_value/2],
              [{as_attr, make_value}, {inject_attrs, pass_seen_attr}]}).

-pass_seen_attr(call_site).
-make_value(ok).

make_value(_Ast, #{pass_seen_attr := Attrs}) ->
    astranaut_lib:gen_function(
      value, astranaut_lib:abstract_form({runtime_attrs, Attrs})).
