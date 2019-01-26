%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2018, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  8 Dec 2018 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_example).

-define(DEBUG_OPT, [{debug, false}, {debug_ast, false}, formatter]).

-include("astranaut.hrl").

%% API
-export([test/0, test_in_function/0]).
-export([function_macro/1, in_function_macro/0]).
-export([test_try_catch/0, test_case/0, test_function/0]).
-export([test_pattern/0, test_clause/0]).
-export([test_quote_string/0]).
-export([test_attributes/0]).
-export([test_group_args/0]).
-export([test_cons/2]).

-export([macro_try_catch/0, macro_case/3, macro_function/2, macro_pattern/1, macro_clause/1]).
-export([macro_quote_string/0, test_imported_macro/0, macro_with_attributes/1]).
-export([macro_group_args/1, cons_macro/1]).
-export([format_error/1]).

-use_macro({astranaut_example_macros, test_macro/0, ?DEBUG_OPT}).
-use_macro({function_macro/1, ?DEBUG_OPT}).
-use_macro({in_function_macro/0, ?DEBUG_OPT}).
-use_macro({macro_try_catch/0, ?DEBUG_OPT}).
-use_macro({macro_case/3, ?DEBUG_OPT}).
-use_macro({macro_function/2, ?DEBUG_OPT}).
-use_macro({macro_pattern/1, ?DEBUG_OPT}).
-use_macro({macro_clause/1, ?DEBUG_OPT}).
-use_macro({macro_quote_string/0, ?DEBUG_OPT}).
-use_macro({macro_with_attributes/1, [{attrs, [include]}|?DEBUG_OPT]}).
-use_macro({macro_group_args/1, [{group_args, true}|?DEBUG_OPT]}).
-use_macro({cons_macro/1, ?DEBUG_OPT}).

-use_macro({astranaut_example_macros, exported_macro/0, [{alias, imported_macro}]}).

-exec_macro({astranaut_example_macros, test_macro, []}).
-exec_macro({function_macro, [a]}).
-exec_macro({function_macro, [b]}).

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

test_quote_string_fun() ->
    ok.

test_attributes() ->
    macro_with_attributes().

test_group_args() ->
    macro_group_args(hello, world).

test_imported_macro() ->
    F = imported_macro(),
    F().

test_cons(A, C) ->
    cons_macro([
                A,
                erlang:now(),
                macro_with_attributes(),
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
    {warning, Ast, noop}.

macro_group_args([Ast1, Ast2]) ->
    Asts = [Ast1, Ast2],
    quote({unquote_splicing(Asts)}).

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
          _@Class:_@Exception:_@Stack ->
              erlang:raise(_L@Expr)
      end).

macro_quote_string() ->
    quote_code("test_quote_string_fun()").

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

macro_with_attributes(#{file := File, line := Line, module := Module}) ->
    {warning, quote(ok), {attributes, File, Line, Module}}.

one_plus() ->
    1 + 1.

cons_macro(Ast) ->
    Ast.

format_error({attributes, File, Line, Module}) ->
    io_lib:format("~p ~s:~p", [Module, File, Line]);
format_error(noop) ->
    io_lib:format("oops: noop", []);
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
