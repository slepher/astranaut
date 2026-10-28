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

%% Exporting this frozen helper retains the complete canonical closure.
%% The final pass must reuse it without interpreting its calls a second time.
helper(Ast) ->
    quote(macro_uniform_a:to_a(unquote(Ast))).
