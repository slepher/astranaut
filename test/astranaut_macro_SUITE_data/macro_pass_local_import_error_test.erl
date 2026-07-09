-module(macro_pass_local_import_error_test).

-include("quote.hrl").
-include("macro.hrl").

-baseline(yep).
-local_macro({[gen_import/1], [as_attr]}).

-gen_import(ok).

gen_import(_Ast) ->
    astranaut_lib:gen_attribute_node(import_macro, 0, macro_uniform_a).

