-module(macro_pass_local_declaration_group_test).

-include("quote.hrl").
-include("macro.hrl").

-export([value/0, foo/1, bar/1]).

%% Members of one declaration share one macro environment. Exporting them also
%% retains both frozen heads for final expansion; bar/1 must still call foo/1
%% as an ordinary function in both the generated module and the retained code.
-local_macro([foo/1, bar/1]).

value() ->
    bar(ok).

foo(Ast) ->
    quote({group_foo, unquote(Ast)}).

bar(Ast) ->
    foo(Ast).
