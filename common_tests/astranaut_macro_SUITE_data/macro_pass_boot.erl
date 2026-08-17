%%%-------------------------------------------------------------------
%%% Test macros for macro pass ordering.
%%%-------------------------------------------------------------------
-module(macro_pass_boot).

-include("macro.hrl").

-export_macro({[bootstrap_import/1], [{as_attr, bootstrap_import}]}).
-export_macro({[bootstrap_depth_limit/1], [{as_attr, bootstrap_depth_limit}]}).

bootstrap_import(_Ast) ->
    astranaut_lib:gen_attribute_node(import_macro, 0, macro_pass_generated).

bootstrap_depth_limit(_Ast) ->
    [astranaut_lib:gen_attribute_node(macro_options, 0, {max_depth, 1}),
     astranaut_lib:gen_attribute_node(import_macro, 0, macro_pass_depth)].
