%%%-------------------------------------------------------------------
%%% Test macros for pass-ordered inject_attrs.
%%%-------------------------------------------------------------------
-module(macro_pass_inject_attrs).

-include("macro.hrl").

-export_macro({[generated_injected_attrs/1], [{inject_attrs, pass_seen_attr}]}).

generated_injected_attrs(#{pass_seen_attr := Attrs}) ->
    astranaut_lib:abstract_form({injected_attrs, Attrs}).
