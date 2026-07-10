-module(macro_pass_local_body_env_error_test).

-include("macro.hrl").

-baseline(yep).
-local_macro([bad_env/0]).

value() ->
    bad_env().

bad_env() ->
    astranaut_lib:gen_attribute_node(import_macro, 0, macro_uniform_a).
