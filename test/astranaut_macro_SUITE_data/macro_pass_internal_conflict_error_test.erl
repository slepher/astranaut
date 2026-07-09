-module(macro_pass_internal_conflict_error_test).

-include("quote.hrl").
-include("macro.hrl").

-export([dummy/0]).

-baseline(yep).
-local_macro({[macro_a/1], [{extra_functions, [{shared, 1}]},
                            {as_attr, macro_a},
                            {internal_function, true}]}).
-local_macro({[macro_b/1], [{extra_functions, [{shared, 1}]},
                            {as_attr, macro_b},
                            {internal_function, []}]}).

-macro_a(ok).

macro_a(Ast) ->
    shared(Ast).

macro_b(Ast) ->
    shared(Ast).

shared(Ast) ->
    Ast.

dummy() ->
    ok.
