%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2019, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 29 Jan 2019 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_macro_example).

-include_lib("astranaut/include/quote.hrl").
-include_lib("astranaut/include/macro.hrl").
%% API

-export_macro({[macro_group_args/1], [group_args]}).
-export_macro({[macro_with_attributes/1], [attrs]}).
-export_macro({[macro_exported_function/2], [auto_export]}).
-export_macro({[macro_with_vars/0], []}).

-export([quote_ok/0]).
-export([quote_unquote/1, quote_binding/1]).
-export([quote_unquote_splicing/2, quote_unquote_splicing_mix/2]).
-export([quote_match_pattern/1, quote_function_pattern/1, quote_case_pattern/1]).
-export([quote_code/0]).
-export([quote_line_1/1, quote_line_2/1]).
-export([macro_function/2, macro_exported_function/2]).
-export([macro_try_catch/0, macro_case/3]).
-export([macro_with_attributes/1, macro_group_args/1]).
-export([macro_with_vars_1/1, macro_with_vars_2/1]).

-use_macro({quote_ok/0}).
-use_macro({macro_exported_function/2}).

-exec_macro({macro_exported_function, [hello, world]}).

%%%===================================================================
%%% API
%%%===================================================================
quote_ok() ->
    quote(ok).

quote_unquote(Ast) ->
    quote({ok, unquote(Ast)}).

quote_binding(Ast) ->
    quote({ok, _@Ast}).

quote_unquote_splicing(Ast1, Ast2) ->
    quote({ok, {hello, unquote_splicing([Ast1, Ast2]), world}}).

quote_unquote_splicing_mix(Ast1, Ast2) ->
    AstList = [Ast1, Ast2],
    Fun1 = 
        quote(
          fun(unquote_splicing = AstList) ->
                  List = [hello, unquote_splicing(AstList), world],
                  Tuple = {hello, unquote_splicing(AstList), world},
                  {ok, List, Tuple}
          end),
    Fun2 = 
        quote(
          fun(unquote = Ast1, Two) ->
                  {error, unquote(Ast1), Two}
          end),
    astranaut:merge_clauses([Fun1, Fun2]).

quote_match_pattern(Ast) ->
    quote(_A@Hello(_@Foo, _L@World)) = Ast,
    quote({_A@Hello, _@Foo, _L@World}).

quote_function_pattern(quote = {hello, _A@World = World2} = C) ->
    quote({ok, {hello2, _A@World, _@World2, _@C}});
quote_function_pattern(Ast) ->
    quote({error, unquote(Ast)}).

quote_case_pattern(Ast) ->
    case Ast of
        quote(_V@Function(_@Argument)) ->
            quote({ok, _V@Function(unquote(Argument) + 1)});
        quote(_@Module:_@Function(_L@Arguments)) ->
            quote({ok, {_@Module, _@Function, _L@Arguments}});
        _Other ->
            quote({error, unquote(Ast)})
    end.

quote_code() ->
    quote_code("test_fun()").

quote_line_1(Ast) ->
    Line = erl_syntax:get_pos(Ast),
    quote({hello, unquote(Ast)}, Line).

quote_line_2(Ast) ->
    Line = erl_syntax:get_pos(Ast),
    Ast1 = quote({hello, unquote(Ast)}, Line + 3),
    quote({ok, unquote(Ast1)}, #{line => Line + 2}).

macro_case(Body, TrueClause, FalseClause) ->
    quote(
      case unquote(Body) of
          unquote(TrueClause) ->
              true;
          unquote(FalseClause) ->
              false
      end).

-ifdef(OTP_RELEASE).
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
-else.
macro_try_catch() ->
    Class = {var, 0, 'Class0'},
    Exception = {var, 0, 'Exception0'},
    Stack = astranaut:replace_line(quote(erlang:get_stacktrace()), 0),
    Expr = [Class, Exception, Stack],
    quote(
      try
          exit(throw)
      catch
          _@Class:_@Exception ->
              erlang:raise(_L@Expr)
      end).
-endif.


macro_function(Pattern, Middle) ->
    quote(fun(Head, _L@Pattern, Body) ->
                  {Head, _@Middle, Body}
      end).

macro_exported_function(Name, Pattern1) ->
    astranaut:function(
      Name,
      quote(
        fun(_A@Pattern1) ->
                ok;
           (_Other) ->
                {error, _Other}
        end)).

macro_with_attributes(#{file := File, line := Line, module := Module}) ->
    quote({ok, {_S@File, _I@Line, _A@Module}}).

macro_group_args(Asts) ->
    quote({ok, {unquote_splicing(Asts)}}).

macro_with_vars_1(Ast) ->
    quote(
      begin
          A = 10,
          _ = A + 10,
          _ = A + 20,
          B = unquote(Ast),
          A + B
      end
     ).

macro_with_vars_2(Ast) ->
    quote(
      begin
          A = 10,
          B = unquote(Ast),
          A + B
      end
     ).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
