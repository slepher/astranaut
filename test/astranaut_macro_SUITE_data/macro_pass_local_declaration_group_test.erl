-module(macro_pass_local_declaration_group_test).

-include("quote.hrl").
-include("macro.hrl").

-export([value/0]).

%% Members of one declaration share one macro environment. bar/1 calls foo/1
%% as an ordinary function inside the generated local-macro module.
-local_macro([foo/1, bar/1]).

value() ->
    bar(ok).

foo(Ast) ->
    quote({group_foo, unquote(Ast)}).

bar(Ast) ->
    foo(Ast).
