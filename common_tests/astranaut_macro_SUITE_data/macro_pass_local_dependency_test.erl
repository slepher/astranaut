-module(macro_pass_local_dependency_test).

-include("quote.hrl").
-include("macro.hrl").

-export([value/0]).

%% Neither macro is used during the attribute scan. Finalization must still
%% load wrap/1 before expanding make_value/1, then rebuild the full generation.
-local_macro([wrap/1]).
-local_macro([make_value/1]).

value() ->
    make_value(ok).

wrap(Ast) ->
    quote({wrapped, unquote(Ast)}).

make_value(Ast) ->
    quote(wrap(unquote(Ast))).
