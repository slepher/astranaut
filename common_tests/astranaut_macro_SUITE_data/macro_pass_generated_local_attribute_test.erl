-module(macro_pass_generated_local_attribute_test).

-include("quote.hrl").
-include("macro.hrl").

-export([value/0]).

-local_macro({[bootstrap/1], [as_attr]}).

-bootstrap(ok).

bootstrap(_Ast) ->
    [astranaut_lib:gen_attribute_node(local_macro, 0, {[{local_value, 1}], [as_attr]}),
     astranaut_lib:gen_attribute_node(local_value, 0, ok)].

local_value(_Ast) ->
    astranaut_lib:gen_function(value, quote(ok)).
