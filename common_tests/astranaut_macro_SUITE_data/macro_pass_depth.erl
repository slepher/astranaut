%%%-------------------------------------------------------------------
%%% Test macros for generated macro_options during the external pass.
%%%-------------------------------------------------------------------
-module(macro_pass_depth).

-include("quote.hrl").
-include("macro.hrl").

-export_macro([chain_a/0, chain_b/0]).
-export_macro({[buffer_chain/1], [{as_attr, buffer_chain}]}).
-export_macro({[buffer_finish/1], [{as_attr, buffer_finish}]}).
-export_macro({[buffer_self/1], [{as_attr, buffer_self}]}).
-export_macro({[buffer_siblings/1], [{as_attr, buffer_siblings}]}).

chain_a() ->
    quote(macro_pass_depth:chain_b()).

chain_b() ->
    quote(depth_done).

buffer_chain(_Ast) ->
    [astranaut_lib:gen_function(
       buffer_head, astranaut_lib:abstract_form(buffer_head)),
     astranaut_lib:gen_attribute_node(buffer_finish, 0, ok)].

buffer_finish(_Ast) ->
    astranaut_lib:gen_function(
      buffer_tail, astranaut_lib:abstract_form(buffer_tail)).

buffer_self(_Ast) ->
    astranaut_lib:gen_attribute_node(buffer_self, 0, again).

buffer_siblings(_Ast) ->
    [astranaut_lib:gen_attribute_node(buffer_finish, 0, first),
     astranaut_lib:gen_attribute_node(buffer_finish, 0, second)].
