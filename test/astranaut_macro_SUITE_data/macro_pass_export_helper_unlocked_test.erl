-module(macro_pass_export_helper_unlocked_test).

-include("quote.hrl").
-include("macro.hrl").

-baseline(yep).
-export([dummy/0]).

-export_macro([entry/1]).
-local_macro({[rewrite_helper/1], [as_attr]}).

-rewrite_helper(ok).

entry(Ast) ->
    helper(Ast).

-spec helper(term()) -> term().
helper(Ast) ->
    Ast.

rewrite_helper(_Ast) ->
    Spec = {type, 0, 'fun',
            [{type, 0, product, [{type, 0, term, []}]},
             {type, 0, term, []}]},
    astranaut_lib:gen_attribute_node(spec, 0, {{helper, 1}, [Spec]}).

dummy() ->
    ok.
