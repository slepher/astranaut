-module(macro_pass_local_macro_error_test).

-include("quote.hrl").
-include("macro.hrl").

-baseline(yep).
-local_macro({[gen_local/1], [as_attr]}).

-gen_local(ok).

gen_local(_Ast) ->
    astranaut_lib:gen_attribute_node(local_macro, 0, [{generated_local, 0}]).

