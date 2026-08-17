-module(macro_pass_local_generated_import_test).

-include("macro.hrl").

-export([pass_generated_value/0]).

-local_macro({[local_import/1], [as_attr]}).

-local_import(ok).
-generated_value(ok).

local_import(_Ast) ->
    astranaut_lib:gen_attribute_node(import_macro, 0, macro_pass_generated).
