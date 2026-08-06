%%%-------------------------------------------------------------------
%%% Multiple failing macro siblings must all be analysed in one pass.
%%%-------------------------------------------------------------------
-module(macro_sibling_errors_test).

-include("quote.hrl").
-include("macro.hrl").

-export([run/0, format_error/1, format_error/2]).

format_error(Error) -> format_error(Error, #{}).
format_error(Error, Options) -> astranaut_lib:dispatch_error(Error, Options, fun(sibling_return_error) -> format_error_1(sibling_return_error); ({macro_exception, _, _, _} = MacroError) -> astranaut_macro:format_error(MacroError, #{default => throw}) end).
format_error_1(sibling_return_error) -> io_lib:write(sibling_return_error).

-local_macro([generate_sibling_errors/0,
              raise_macro/0, return_error_macro/0, invalid_return_macro/0]).

run() ->
    generate_sibling_errors().

generate_sibling_errors() ->
    quote({raise_macro(), return_error_macro(), invalid_return_macro()}).

raise_macro() ->
    erlang:error(sibling_exception).

return_error_macro() ->
    {error, sibling_return_error}.

invalid_return_macro() ->
    {not_ast}.
