%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2018, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  8 Dec 2018 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_warnings).

-include("quote.hrl").
-compile({parse_transform, astranaut_warnings_pt}).

%% API
-export([test_cons/2]).

-export([cons_macro/1]).
-export([test_attributes/0]).
-export([function_macro/1]).
-export([noop_warnings/0]).
-export([format_error/1]).

-use_macro({function_macro/1}).
-use_macro({noop_warnings/0, [formatter]}).
-use_macro({cons_macro/1}).

-exec_macro({function_macro, [a]}).
-exec_macro({function_macro, [b]}).

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

function_macro(a) ->
    astranaut:exported_function(
      test_local,
      quote(
        fun() ->
                ok
        end));
function_macro(b) ->
    Ast = astranaut:exported_function(
            test_local_b,
            quote(
              fun() ->
                      ok
              end)),
    {warning, Ast, noop_function}.

noop_warnings() ->
    {warning, quote(ok), noop}.

cons_macro(Ast) ->
    Ast.

format_error(Error) ->
    astranaut_traverse:format_error(Error).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
