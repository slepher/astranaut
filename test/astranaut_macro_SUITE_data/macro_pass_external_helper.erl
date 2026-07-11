%%%-------------------------------------------------------------------
%%% External attribute macros that generate forms used by local macros.
%%%-------------------------------------------------------------------
-module(macro_pass_external_helper).

-include("quote.hrl").
-include("macro.hrl").

-export_macro({[generate_helper/1,
                generate_use_alias/1,
                generate_alias_consumer/1,
                generate_attr_chain/1,
                chained_attr/1,
                generate_import_and_attr/1,
                generate_seen_attr/1,
                generate_late_seen_attr/1,
                generate_non_env_attr/1,
                non_env_chained_attr/1,
                generate_final_function/1,
                generate_delayed_macro_call/1,
                stateful_attribute/1], [{as_attr, true}]}).

-export_macro([stateful_function/0]).

generate_helper(_Ast) ->
    Body = quote(astranaut_lib:abstract_form({external_generated_helper, ok})),
    astranaut_lib:gen_function(generated_helper, Body).

generate_use_alias(_Ast) ->
    astranaut_lib:gen_attribute_node(use_macro, 0,
                                     {macro_uniform_a, {to_a, 1}, [{alias, generated_to_a}]}).

generate_alias_consumer(_Ast) ->
    Body = quote(generated_to_a(ok)),
    astranaut_lib:gen_function(alias_attr_value, Body).

generate_attr_chain(_Ast) ->
    astranaut_lib:gen_attribute_node(chained_attr, 0, ok).

chained_attr(_Ast) ->
    astranaut_lib:gen_function(chained_attr_value,
                               astranaut_lib:abstract_form({external_attr_chain, ok})).

generate_import_and_attr(_Ast) ->
    [astranaut_lib:gen_attribute_node(import_macro, 0, macro_pass_generated),
     astranaut_lib:gen_attribute_node(generated_value, 0, ok)].

generate_seen_attr(_Ast) ->
    astranaut_lib:gen_attribute_node(pass_seen_attr, 0, early).

generate_late_seen_attr(_Ast) ->
    astranaut_lib:gen_attribute_node(pass_seen_attr, 0, generated_late).

generate_non_env_attr(_Ast) ->
    astranaut_lib:gen_attribute_node(non_env_chained_attr, 0, ok).

non_env_chained_attr(_Ast) ->
    astranaut_lib:gen_function(non_env_attr_value,
                               astranaut_lib:abstract_form({external_non_env_attr, ok})).

generate_final_function(_Ast) ->
    Body = quote(macro_uniform_a:to_a(ok)),
    astranaut_lib:gen_function(final_external_value, Body).

generate_delayed_macro_call(_Ast) ->
    Body = quote(macro_uniform_a:to_a(ok)),
    astranaut_lib:gen_function(delayed_value, Body).

%% Deliberately returns a traverse computation that mutates its private State.
%% The caller's scan State must remain intact after this macro completes.
stateful_attribute(_Ast) ->
    Form = astranaut_lib:gen_function(stateful_value,
                                      astranaut_lib:abstract_form(stateful)),
    astranaut_traverse:then(
      astranaut_traverse:put(overwritten),
      astranaut_traverse:return(Form)).

%% The function-pass counterpart of stateful_attribute/1.
stateful_function() ->
    astranaut_traverse:then(
      astranaut_traverse:put(overwritten),
      astranaut_traverse:return(astranaut_lib:abstract_form(function_stateful))).
