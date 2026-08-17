%%%-------------------------------------------------------------------
%%% Test macros for pass-ordered inject_attrs.
%%%-------------------------------------------------------------------
-module(macro_pass_inject_attrs).

-include("macro.hrl").

-export_macro({[generated_injected_attrs/1], [{inject_attrs, pass_seen_attr}]}).
-export_macro({[injected_attrs_attribute/2],
               [{as_attr, injected_attrs_attribute}, {inject_attrs, pass_seen_attr}]}).

generated_injected_attrs(#{pass_seen_attr := Attrs}) ->
    astranaut_lib:abstract_form({injected_attrs, Attrs}).

injected_attrs_attribute(_Ast, #{pass_seen_attr := Attrs}) ->
    astranaut_lib:gen_function(
      injected_attribute_attrs_value,
      astranaut_lib:abstract_form({injected_attribute_attrs, Attrs})).
