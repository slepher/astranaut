%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2018, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  8 Dec 2018 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_example).

-define(DEBUG_OPT, [{debug, false}, {debug_ast, false}]).

-include("astranaut.hrl").

%% API
-export([test/0, test_in_function/0]).
-export([function_macro/0, in_function_macro/0]).
-export([test_try_catch/0, test_case/0, test_function/0]).
-export([test_pattern/0, test_clause/0]).
-export([test_quote_string/0]).

-export([macro_try_catch/0, macro_case/3, macro_function/2, macro_pattern/1, macro_clause/1]).
-export([macro_quote_string/0]).

-use_macro({astranaut_example_macros, test_macro/0, ?DEBUG_OPT}).
-use_macro({function_macro/0, ?DEBUG_OPT}).
-use_macro({in_function_macro/0, ?DEBUG_OPT}).
-use_macro({macro_try_catch/0, ?DEBUG_OPT}).
-use_macro({macro_case/3, ?DEBUG_OPT}).
-use_macro({macro_function/2, ?DEBUG_OPT}).
-use_macro({macro_pattern/1, ?DEBUG_OPT}).
-use_macro({macro_clause/1, ?DEBUG_OPT}).
-use_macro({macro_quote_string/0, ?DEBUG_OPT}).

-exec_macro({astranaut_example_macros, test_macro, []}).
-exec_macro({function_macro, []}).

%%%===================================================================
%%% API
%%%===================================================================
-spec test_in_function() -> ok.
test_in_function() ->
    in_function_macro().

test_try_catch() ->
    macro_try_catch().

test_case() ->
    macro_case(one_plus(), 2, 3).

test_function() ->
    F = macro_function(['_', '_'], ok),
    F(x).

test_pattern() ->
    macro_pattern(hello(foo, bar, world)).

test_clause() ->
    macro_clause({hello, world}).

test_quote_string() ->
    macro_quote_string().

function_macro() ->
    astranaut:exported_function(
      test_local,
      quote(
        fun() ->
                ok
        end)).

in_function_macro() ->
    quote(ok).

macro_try_catch() ->
    Class = {var, 0, 'Class0'},
    Exception = {var, 0, 'Exception0'},
    Stack = {var, 0, 'Stack0'},
    Expr = [Class, Exception, Stack],
    quote(
      try
          exit(throw)
      catch
          '@Class':'@Exception':_@Stack ->
              erlang:raise(_L@Expr)
      end).

macro_quote_string() ->
    quote_code("test_case()").

macro_case(Body, TrueClause, FalseClause) ->
    quote(
      case unquote(Body) of
          unquote(TrueClause) ->
              true;
          unquote(FalseClause) ->
              false
      end).

macro_function(Pattern, Middle) ->
    quote(fun(Head, _L@Pattern, Body) ->
                  {Head, _@Middle, Body}
      end).

macro_pattern(Ast) ->
    quote(_A@Hello(_@Foo, _L@World)) = Ast,
    quote({_A@Hello, _@Foo, _L@World}).

macro_clause(quote = {hello, _A@World = World2} = C) ->
    quote({hello2, _A@World, _@World2,_@C});
macro_clause(_) ->
    quote(fail).

one_plus() ->
    1 + 1.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
