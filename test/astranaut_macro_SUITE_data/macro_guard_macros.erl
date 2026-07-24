%%% Macros used to exercise source-function guard traversal.
-module(macro_guard_macros).

-include("quote.hrl").
-include("macro.hrl").

-export_macro([identity/1, is_even/1]).

identity(Ast) ->
    Ast.

is_even(Ast) ->
    quote((unquote(Ast) rem 2) =:= 0).
