-module(macro_pass_locked_helper_error_test).

-include("quote.hrl").
-include("macro.hrl").

-baseline(yep).
-local_macro([entry/1]).
-local_macro({[rewrite_helper/1], [as_attr]}).

-rewrite_helper(ok).

entry(Ast) ->
    helper(Ast).

helper(Ast) ->
    Ast.

rewrite_helper(_Ast) ->
    Helper = quote(fun(_Arg) -> mutated end),
    astranaut_lib:gen_function(helper, Helper).

