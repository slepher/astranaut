-module(macro_pass_local_no_backscan_test).

-include("macro.hrl").

-export([pass_generated_value/0]).

%% This attribute precedes the local macro that imports generated_value/1.
%% If the scan revisits passed forms it would generate a duplicate function.
-generated_value(before_import).

-local_macro({[local_import/1], [as_attr]}).
-local_import(ok).
-generated_value(ok).

local_import(_Ast) ->
    astranaut_lib:gen_attribute_node(import_macro, 0, macro_pass_generated).
