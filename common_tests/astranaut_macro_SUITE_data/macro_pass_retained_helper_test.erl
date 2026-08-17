-module(macro_pass_retained_helper_test).

-include("quote.hrl").
-include("macro.hrl").

-export([value/0, entry/1, helper/1]).

-import_macro(macro_uniform_a).
-local_macro([entry/1]).

value() ->
    entry(ok).

entry(Ast) ->
    helper(Ast).

%% Exporting this frozen helper retains the complete closure. It must compare
%% equal under the declaration and final environments, then remain available
%% to the final function-body pass.
helper(Ast) ->
    quote(macro_uniform_a:to_a(unquote(Ast))).
