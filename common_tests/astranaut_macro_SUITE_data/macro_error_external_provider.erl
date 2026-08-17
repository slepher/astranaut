%%%-------------------------------------------------------------------
%%% External formatter ownership coverage for macro exceptions.
%%%-------------------------------------------------------------------
-module(macro_error_external_provider).

-include("quote.hrl").
-include("macro.hrl").

-export([format_error/1]).

-export_macro([raise/0, return_error/0, return_warning/0]).

raise() ->
    erlang:error(external_macro_exception).

return_error() ->
    {error, external_return_error}.

return_warning() ->
    {warning, quote(ok), external_return_warning}.

format_error(external_return_error) ->
    io_lib:write(external_return_error);
format_error(external_return_warning) ->
    io_lib:write(external_return_warning).
