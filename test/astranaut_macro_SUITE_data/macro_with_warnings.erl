%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2018, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  8 Dec 2018 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(macro_with_warnings).

-include_lib("astranaut/include/quote.hrl").
-include_lib("astranaut/include/macro.hrl").

%% API
-export([test_cons/2]).

-export([test_attributes/0]).
-export([unquote_splicing_warnings/1]).
-export([format_error/1]).

-local_macro([function_macro/1]).
-local_macro([noop_warnings/0]).
-local_macro({[cons_macro/1], [as_attr]}).
-local_macro([exception_error/0, return_error/0]).

-baseline(yep).

-exec_macro({function_macro, [a]}).
-exec_macro({function_macro, [b]}).
-exec_macro({function_macro, [c, d]}).
-cons_macro([e, f, g]).

%%%===================================================================
%%% API
%%%===================================================================

test_attributes() ->
    noop_warnings().

test_cons(A, C) ->
    cons_macro([
                A,
                cons_macro([
                            noop_warnings()
                           ]),
                noop_warnings(),
                C
                ]).

unquote_splicing_warnings(Ast) ->
    quote({unquote_splicing(Head), tail}) = Ast,
    Head.

function_macro(a) ->
    astranaut_lib:gen_exported_function(
      test_local,
      quote(
        fun() ->
                ok
        end));
function_macro(b) ->
    Ast = astranaut_lib:gen_exported_function(
            test_local_b,
            quote(
              fun() ->
                      ok
              end)),
    {warning, Ast, noop_function}.

error_macro_1() ->
    exception_error().

error_macro_2() ->
    return_error().

noop_warnings() ->
    {warning, quote(ok), noop}.

exception_error() ->
    erlang:error(foo).

return_error() ->
    {error, bar}.

cons_macro(Ast) ->
    Ast.

format_error(bar) ->
    "oops, bar";
format_error(noop) ->
    "oops, noop";
format_error(Error) ->
    astranaut_macro:format_error(Error).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
