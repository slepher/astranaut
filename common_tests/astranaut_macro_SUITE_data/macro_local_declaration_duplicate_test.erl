-module(macro_local_declaration_duplicate_test).

-include("macro.hrl").

-baseline(yep).

-local_macro({[gen/1], [as_attr]}).
-local_macro([gen/1]).

-gen(ok).

gen(_Ast) ->
    astranaut_lib:gen_attribute_node(pass_seen_attr, 0, generated).
