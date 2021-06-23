%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2020, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  2 Jul 2020 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_do).

-include("quote.hrl").

%% API
-export([parse_transform/2, format_error/1]).

%%%===================================================================
%%% API
%%%===================================================================
parse_transform(Forms, _Opts) ->
    TransformOpts = #{formatter => ?MODULE, traverse => post},
    astranaut_return:to_compiler(astranaut:map(fun walk_node/2, Forms, TransformOpts)).

format_error(non_empty_do) ->
    "A 'do' construct cannot be empty";
format_error(non_last_expression) ->
    "The last statement in a 'do' construct must be an expression";
format_error(Reason) ->
    astranaut:format_error(Reason).

%%%===================================================================
%%% Internal functions
%%%===================================================================

walk_node({call, _Pos, {atom, __Pos1, do}, [{lc, _Pos2, _Monad, _Comprehensions} = LCNode]}, #{}) ->
    do(LCNode);
walk_node(Node, _Attrs) ->
    Node.

do({lc, Pos, {atom, _Pos3, _MonadModule} = Monad, Comprehensions}) ->
    astranaut_return:lift_m(
      fun(Expressions) ->
              quote((fun() ->
                             unquote_splicing(Expressions)
                     end)(), Pos)
      end, do_comprehensions(Comprehensions, Monad));
do(_Ast) ->
    {error, expected_list_comprehension}.

%%  'do' syntax transformation:
do_comprehensions([], _Monad) ->
    astranaut_return:error_fail(non_empty_do);
do_comprehensions([{GenerateOrMatch, _Pos, _Pattern, _Expr}], _Monad)
  when GenerateOrMatch =:= generate orelse GenerateOrMatch =:= match ->
    astranaut_return:error_fail(non_last_expression);
do_comprehensions([Expr], Monad) ->
    %% Don't do '>>' chaining on the last elem
   [update_expression(Expr, Monad)];
do_comprehensions([Expr | Exprs], Monad) ->
    Expr1 = update_expression(Expr, Monad),
    astranaut_return:lift_m(
      fun(Exprs1) ->
              bind_expression(Expr1, Exprs1, Monad)
      end, do_comprehensions(Exprs, Monad)).

bind_expression({generate, Pos, {var, _Pos, _Var} = Pattern, Expr}, Exprs, Monad) ->
    %% "Pattern <- Expr, Tail" where Pattern is a simple variable
    %% is transformed to
    %% "Monad:Bind(Expr, fun (Pattern) -> Tail end)"
    %% without a fail to match clause
    [quote(astranaut_monad:bind(
             unquote(Expr),
             fun(unquote = Pattern) ->
                     unquote_splicing(Exprs)
             end, unquote(Monad)), Pos)];
bind_expression({generate, Pos, Pattern, Expr}, Exprs, Monad) ->
    PosExpr = astranaut_lib:abstract_form(Pos, Pos),
    String = astranaut_lib:abstract_form(astranaut_lib:ast_to_string(Pattern), Pos),
    %% "Pattern <- Expr, Tail" where Pattern is not a simple variable
    %% is transformed to
    %% "Monad:Bind(Expr, fun (Pattern) -> Tail; (Var) -> exit({monad_badmatch, Var, Pos, PatternString}) end)"
    %% without a fail to match clause
    [quote(astranaut_monad:bind(
             unquote(Expr),
             fun(unquote = Pattern) ->
                     unquote_splicing(Exprs);
                (Var) ->
                     exit({monad_badmatch, Var, unquote(PosExpr), unquote(String)})
             end, unquote(Monad)), Pos)];
bind_expression({match, _Pos, _Pattern, _Expr} = Expr, Exprs, _Monad) ->
    %% Handles 'let binding' in do expression a-la Haskell
    %% Value = Expr, Tail is not transformed.
    [Expr|Exprs];
bind_expression(Expr, Exprs, Monad) ->
    %% Expr, Tail
    %% is transformed to
    %% "Monad:bind(Expr, fun(_) -> Tail end)
    Pos = astranaut_syntax:get_pos(Expr),
    [quote(astranaut_monad:bind(unquote(Expr), fun(_) -> unquote_splicing(Exprs) end, unquote(Monad)), Pos)].

update_expression(Expression, Monad) ->
    astranaut:smap(
      fun({call, Pos, {atom, _Pos1, fail}, [Arg]}) ->
              %% 'return' calls of a particular form:
              %% return(Argument), and
              %% Transformed to:
              %% "Monad:return(Argument)" in monadic context
              quote(astranaut_monad:fail(unquote(Arg), unquote(Monad)), Pos);
         ({call, Pos, {atom, _Pos1, return}, [Arg]}) ->
              %% 'fail' calls of a particular form:
              %% fail(Argument)
              %% Transformed to:
              %% "Monad:fail(Argument)" in monadic context
              quote(astranaut_monad:return(unquote(Arg), unquote(Monad)), Pos);
         (_Node) ->
              keep
      end, Expression, #{formatter => ?MODULE, traverse => pre}).
