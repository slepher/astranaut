%%%-------------------------------------------------------------------
%%% Test macros for uniform macro expansion.
%%%-------------------------------------------------------------------
-module(macro_uniform_b).

-include("quote.hrl").
-include("macro.hrl").

-export_macro([to_b/1, gen_a/1, recurse_b/1]).
-export_macro({[inject_generated_local_macro/1], [{as_attr, inject_generated_local_macro}]}).

to_b(Ast) ->
    quote({b, {from_b, unquote(Ast)}}).

gen_a(Ast) ->
    quote({b_generated, macro_uniform_a:to_a(unquote(Ast))}).

recurse_b({integer, _Pos, N} = Ast) when N > 0 ->
    NextN = N - 1,
    quote({b_recurse, unquote(Ast), macro_uniform_a:recurse_a(_I@NextN)});
recurse_b(Ast) ->
    quote({b_done, unquote(Ast)}).

inject_generated_local_macro(_Ast) ->
    LocalMacroAttr = astranaut_lib:gen_attribute_node(local_macro, 0, [{generated_local_macro, 0}]),
    MacroCall = quote(macro_uniform_a:to_a(attribute_generated)),
    LocalMacroFun =
        astranaut_lib:gen_function(
          generated_local_macro,
          astranaut_lib:abstract_form(MacroCall)),
    [LocalMacroAttr, LocalMacroFun].
