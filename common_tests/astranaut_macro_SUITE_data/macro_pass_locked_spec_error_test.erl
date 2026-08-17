-module(macro_pass_locked_spec_error_test).

-include("quote.hrl").
-include("macro.hrl").

-baseline(yep).
-local_macro([entry/1]).
-local_macro({[rewrite_helper_spec/1], [as_attr]}).

-rewrite_helper_spec(ok).

entry(Ast) ->
    helper(Ast).

-spec helper(term()) -> term().
helper(Ast) ->
    Ast.

rewrite_helper_spec(_Ast) ->
    Spec = {type, 0, 'fun',
            [{type, 0, product, [{type, 0, term, []}]},
             {type, 0, term, []}]},
    astranaut_lib:gen_attribute_node(spec, 0, {{helper, 1}, [Spec]}).
