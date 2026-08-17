%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2026, Chen Slepher
%%% @doc Quote context macro fixtures.
%%% @end
%%% Created : 04 Aug 2026 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(macro_quote_context_example).

-include("quote.hrl").
-include("macro.hrl").

%% API
-export_macro([hygienic_macro/1]).
-export_macro([no_context_capture_macro/1]).
-export_macro([named_fun_macro/0]).
-export_macro([no_context_named_fun/0]).
-export_macro([same_context_macro/1]).
-export_macro([different_context_macro/2]).
-export_macro([unquote_identity_macro/1]).
-export_macro({[attr_no_counter_macro/1], [{as_attr, attr_no_counter}]}).
-export_macro([already_expanded_macro/0]).

%%%===================================================================
%%% API
%%%===================================================================
hygienic_macro(Value) ->
    quote(
      begin
          Temp = unquote(Value),
          Temp
      end).

no_context_capture_macro(Value) ->
    quote(
      begin
          Shared = unquote(Value),
          Shared
      end, no_context).

named_fun_macro() ->
    quote(
      fun Fact(0) -> 1;
         Fact(N) -> N * Fact(N - 1)
      end).

no_context_named_fun() ->
    quote(
      fun Fact(0) -> 1;
         Fact(N) -> N * Fact(N - 1)
      end, no_context).

same_context_helper(Value) ->
    quote(Var = unquote(Value)).

same_context_macro(Value) ->
    Bind = same_context_helper(Value),
    quote(
      begin
          unquote(Bind),
          Var
      end).

different_context_a(Value) ->
    quote(
      begin
          Var = unquote(Value),
          Var
      end, #{context => ctx_a}).

different_context_b(Value) ->
    quote(
      begin
          Var = unquote(Value),
          Var
      end, #{context => ctx_b}).

different_context_macro(Value1, Value2) ->
    A = different_context_a(Value1),
    B = different_context_b(Value2),
    quote({unquote(A), unquote(B)}).

unquote_identity_macro(Value) ->
    quote(
      begin
          MacroVar = unquote(Value) + 1,
          {MacroVar, unquote(Value)}
      end).

attr_no_counter_macro(_Value) ->
    astranaut_lib:gen_function(
      generated_attr_fun,
      quote(
        fun() ->
            AttrVar = 42,
            AttrVar
        end)).

already_expanded_macro() ->
    {match, 0,
     {var, 0, 'Already@astranaut_quote@macro_quote_context_example@1'},
     {integer, 0, 42}}.
