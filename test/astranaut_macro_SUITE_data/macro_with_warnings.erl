%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2018, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  8 Dec 2018 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(macro_with_warnings).

-macro_options(debug_module).

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

noop_warnings() ->
    {warning, quote(ok), noop}.

cons_macro(Ast) ->
    Ast.

format_error(Error) ->
    erl_af:format_error(Error).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
