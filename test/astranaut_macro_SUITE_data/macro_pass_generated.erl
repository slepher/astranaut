%%%-------------------------------------------------------------------
%%% Test macros imported by an earlier generated import.
%%%-------------------------------------------------------------------
-module(macro_pass_generated).

-include("macro.hrl").

-export_macro({[generated_value/1], [{as_attr, generated_value}]}).

generated_value(_Ast) ->
    astranaut_lib:gen_function(pass_generated_value,
                               astranaut_lib:abstract_form({pass_generated, ok})).
