%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2019, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 20 Sep 2019 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_rebinding).

%% API
-export([parse_transform/2, format_error/1]).

%%%===================================================================
%%% API
%%%===================================================================
parse_transform(Forms, _Options) ->
    erlang:system_flag(backtrace_depth, 30),
    dbg:tracer(),
    %% dbg:tpl(astranaut_rebinding_scope, exit_funcall_argument, cx),
    %% dbg:tpl(astranaut_traverse, m_subtrees, cx),
    dbg:p(all, [c]),
    {RebindingOptionsRec, Warnings} = 
        astranaut_rebinding_options:rebinding_options(Forms),
    FormsMonad = 
        astranaut_traverse_monad:then(
          astranaut_traverse_monad:warnings(Warnings),
          astranaut_traverse_monad:map_m(
            fun(Form) ->
                    walk_form(Form, RebindingOptionsRec)
            end, Forms)),
    File = astranaut:file(Forms),
    Return = astranaut_traverse_monad:eval(FormsMonad, #{}),
    astranaut_traverse:parse_transform_return(Return, File).

format_error(Message) ->
    case io_lib:deep_char_list(Message) of
        true -> Message;
        _    -> io_lib:write(Message)
    end.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
walk_form({function, Line, Name, Arity, Clauses} = Function, RebindingOptionsRec) ->
    case astranaut_rebinding_options:match_rebinding(Name, Arity, RebindingOptionsRec) of
        {ok, RebindingOptions} ->
            ClausesM = 
                astranaut_traverse_monad:map_m(
                  fun(Clause) ->
                          Return = walk_function_clause(Clause),
                          astranaut_traverse:fun_return_to_monad(Return, Clause, #{with_state => true})
                  end, Clauses),
            astranaut_traverse_monad:lift_m(
              fun(Clauses1) ->
                      Function1 = {function, Line, Name, Arity, Clauses1},
                      case maps:get(debug, RebindingOptions, false) of
                          true ->
                              io:format("~s~n", [astranaut:safe_to_string(Function1)]);
                          false ->
                              ok
                      end,
                      Function1
                          
              end, ClausesM);
        error ->
            astranaut_traverse_monad:return(Function)
    end;
walk_form(Form, _RebindingOptionsRec) ->
    astranaut_traverse_monad:return(Form).

walk_function_clause(Clause) ->
    Context0 = astranaut_rebinding_scope:new_context(),
    astranaut_traverse:map_with_state(
        fun(Node, Acc, Attr) ->
                walk_node(Node, Acc, Attr)
        end, Context0, Clause, #{node => form, traverse => all, match_right_first => true, parent => fun_expr}).

walk_node(Node, #{} = Context, #{step := pre} = Attr) ->
    NodeType = erl_syntax:type(Node),
    case is_scope_group(NodeType) of
        true ->
            Context1 = astranaut_rebinding_scope:entry_scope_group(Context),
            walk_node_1(Node, Context1, Attr);
        false ->
            walk_node_1(Node, Context, Attr)
    end;
walk_node(Node, #{} = Context, #{step := post} = Attr) ->
    NodeType = erl_syntax:type(Node),
    case is_scope_group(NodeType) of
        true ->
            Context1 = astranaut_rebinding_scope:exit_scope_group(Context),
            walk_node_1(Node, Context1, Attr);
        false ->
            walk_node_1(Node, Context, Attr)
    end;
walk_node(Node, Context, #{} = Attr) ->
    {Node1, Context1} = walk_node_1(Node, Context, Attr),
    %% io:format("walk node ~p ~p ~p ~p~n", [Node, Node1, Context, Context1]),
    {Node1, Context1}.

is_scope_group(case_expr) ->
    true;
is_scope_group(catch_expr) ->
    true;
is_scope_group(receive_expr) ->
    true;
is_scope_group(try_expr) ->
    true;
is_scope_group(application) ->
    true;
is_scope_group(_Type) ->
    false.

walk_node_1({ComprehensionType, _Line, _Expression, _Qualifiers} = Node, #{} = Context, #{step := pre} = Attr) 
  when (ComprehensionType == lc) or (ComprehensionType == bc) ->
    NodeM = walk_comprehension(Node), 
    {Node1, Context1} = 
        astranaut_traverse:monad_to_traverse_fun_return(NodeM, #{init => Context, with_state => true}),
    {Node2, Context2} = walk_node(Node1, Context1, Attr#{step => post}),
    astranaut_traverse:traverse_fun_return(#{node => Node2, state => Context2, continue => true});
walk_node_1({call, _Line, _Function, _Args} = Node, #{} = Context, #{step := pre} = Attr) ->
    NodeM = walk_function_call(Node),
    {Node1, Context1} = 
        astranaut_traverse:monad_to_traverse_fun_return(NodeM, #{init => Context, with_state => true}),
    {Node2, Context2} = walk_node(Node1, Context1, Attr#{step => post}),
    astranaut_traverse:traverse_fun_return(#{node => Node2, state => Context2, continue => true});
walk_node_1({'match', Line, Patterns, Expressions},
          #{} = Context, 
          #{step := pre, node := expression}) ->
    Opts = #{node => expression, traverse => all},
    F = astranaut_traverse:transform_mapfold_f(fun walk_node/3, Opts),
    NodeM =
        astranaut_traverse_monad:bind(
          astranaut_traverse:map_m(F, Expressions, Opts#{node => expression}),
          fun(NExpressions) ->
                  astranaut_traverse_monad:bind(
                    astranaut_rebinding_scope:with_match_left_pattern(
                      astranaut_traverse:map_m(F, Patterns, Opts#{node => pattern})),
                    fun(NPatterns) ->
                            astranaut_traverse_monad:return({'match', Line, NPatterns, NExpressions})
                    end)
          end),
    {Node1, Context1} = 
        astranaut_traverse:monad_to_traverse_fun_return(NodeM, #{init => Context, with_state => true}),
    astranaut_traverse:traverse_fun_return(#{node => Node1, state => Context1, continue => true});
walk_node_1({clause, _Line, _Patterns, _Match, _Body} = Node, 
          #{} = Context, 
          #{step := pre, parent := Parent}) ->
    ScopeType = clause_scope_type(Parent),
    NodeM = walk_clause(Node, ScopeType),
    {Node1, Context1} = 
        astranaut_traverse:monad_to_traverse_fun_return(NodeM, #{init => Context, with_state => true}),
    astranaut_traverse:traverse_fun_return(#{node => Node1, state => Context1, continue => true});
walk_node_1({named_fun, _Line, _Name, _Clauses} = Node, 
          #{} = Context, 
          #{step := pre}) ->
    NodeM = walk_named_fun(Node),
    {Node1, Context1} = 
        astranaut_traverse:monad_to_traverse_fun_return(NodeM, #{init => Context, with_state => true}),
    astranaut_traverse:traverse_fun_return(#{node => Node1, state => Context1, continue => true});
%% rename var if current node is expression.
walk_node_1({var, _Line, '_'} = Var, #{} = Acc, #{}) ->
    {Var, Acc};
walk_node_1({op, _Line1, '+', {var, _Line3, _Varname} = Var}, 
            #{pattern := match_left} = Context, #{node := pattern, step := pre}) ->
    Var1 = astranaut_rebinding_scope:rename_var(Var, Context),
    astranaut_traverse:traverse_fun_return(#{node => Var1, state => Context, continue => true});
walk_node_1({var, _Line, _Varname} = Var, #{} = Acc, #{node := expression}) ->
    {astranaut_rebinding_scope:rename_var(Var, Acc), Acc};
%% rename var if current node is guard.
walk_node_1({var, _Line, _Varname} = Var, #{} = Acc, #{node := guard}) ->
    {astranaut_rebinding_scope:rename_var(Var, Acc), Acc};
%% rebind var if current node is function pattern.
walk_node_1({var, _Line, _Varname} = Var, #{pattern := function_clause} = Context, #{node := pattern}) ->
    {Var1, Context1} = astranaut_rebinding_scope:rebind_var(Var, Context),
    %% io:format("rebind function clause pattern ~p ~p ~p ~p ~n", [Var, Var1, Context, Context1]),
    {Var1, Context1};
%% rebind var if current node is match pattern.
walk_node_1({var, _Line, _Varname} = Var, 
            #{pattern := match_left} = Context, #{node := pattern}) ->
    {Var1, Context1} = astranaut_rebinding_scope:rebind_var(Var, Context),
    {Var1, Context1};
walk_node_1({var, _Line, _Varname} = Var, 
            #{pattern := comprehension_generate} = Context, #{node := pattern}) ->
    {Var1, Context1} = astranaut_rebinding_scope:rebind_var(Var, Context),
    {Var1, Context1};

walk_node_1({var, _Line, _Varname} = Var, #{} = Context, #{}) ->
    {astranaut_rebinding_scope:rename_var(Var, Context), Context};
walk_node_1(Node, Acc, #{}) ->
    {Node, Acc}.

walk_function_call({call, Line, Function, FunctionArgs} = Node) ->
    Parent = erl_syntax:type(Node),
    Opts = #{node => expression, traverse => all, parent => Parent},
    F = astranaut_traverse:transform_mapfold_f(fun walk_node/3, Opts),
    astranaut_traverse_monad:bind(
      astranaut_traverse:map_m(F, Function, Opts),
      fun(Function1) ->
              astranaut_traverse_monad:bind(
                map_m_function_args(F, FunctionArgs, Opts, []),
                fun(FunctionArgs1) ->
                        astranaut_traverse_monad:return({call, Line, Function1, FunctionArgs1})
                end)
      end).

clause_scope_type(fun_expr) ->
    shadowed;
clause_scope_type(named_fun_expr) ->
    shadowed;
clause_scope_type(_Other) ->
    nonfun_clause.

walk_clause({clause, _Line, _Patterns, _Guards, _Expressions} = Node, ScopeType) ->
    Opts = #{node => expression, traverse => all},
    F = astranaut_traverse:transform_mapfold_f(fun walk_node/3, Opts),
    PatternType = astranaut_rebinding_scope:scope_type_pattern(ScopeType),

    Sequence = fun([PatternTreesM|RestTreesM]) ->
                       PatternTreesM1 = astranaut_rebinding_scope:with_pattern(PatternType, PatternTreesM),
                       astranaut_traverse_monad:sequence_m([PatternTreesM1|RestTreesM])
               end,
    astranaut_rebinding_scope:with_scope_type(
      ScopeType,
      astranaut_traverse:map_m(F, Node, Opts#{sequence_children => Sequence})).

walk_named_fun({named_fun, Line,  Name, Clauses} = Node) ->
    Parent = erl_syntax:type(Node),
    Opts = #{node => expression, traverse => all, parent => Parent},
    NameVar = {var, Line, Name},
    F = astranaut_traverse:transform_mapfold_f(fun walk_node/3, Opts),
    astranaut_rebinding_scope:with_shadowed(
      astranaut_traverse_monad:bind(
        astranaut_rebinding_scope:with_function_clause_pattern(
          astranaut_traverse:map_m(F, NameVar, Opts#{node => pattern, parent => named_fun_expr})),
        fun({var, _Line, Name1}) ->
                astranaut_traverse_monad:bind(
                  astranaut_traverse:map_m(F, Clauses, Opts),
                  fun(Clauses1) ->
                          astranaut_traverse_monad:return({named_fun, Line, Name1, Clauses1})
                  end)
        end)).
 
map_m_function_args(_F, [], _Opts, FunctionArgs1) ->
    astranaut_traverse_monad:return(lists:reverse(FunctionArgs1));
map_m_function_args(F, [FunctionArgExpression|T], Opts, FunctionArgs1) ->
    astranaut_traverse_monad:bind(
      astranaut_rebinding_scope:with_funcall_argument(
        astranaut_traverse:map_m(F, FunctionArgExpression, Opts)),
      fun(FunctionArgExpression1) ->
              map_m_function_args(F, T, Opts, [FunctionArgExpression1|FunctionArgs1])
      end).

walk_comprehension({ComprehensionType, Line, Expression, Qualifiers} = Node) ->
    Parent = erl_syntax:type(Node),
    Opts = #{node => expression, traverse => all, parent => Parent},
    F = astranaut_traverse:transform_mapfold_f(fun walk_node/3, Opts),
    astranaut_rebinding_scope:with_shadowed(
      astranaut_traverse_monad:bind(
        map_m_qualifiers(F, Qualifiers, Opts, []),
        fun(Qualifiers1) ->
                astranaut_traverse_monad:bind(
                  astranaut_traverse:map_m(F, Expression, Opts),
                  fun(Expression1) ->
                          astranaut_traverse_monad:return({ComprehensionType, Line, Expression1, Qualifiers1})
                  end)
        end)).

map_m_qualifiers(F, [{GenerateType, Line, Pattern, Expression}|RestQualifiers], Opts, Acc)
  when (GenerateType == generate) or (GenerateType == b_generate) ->
    astranaut_traverse_monad:bind(
      astranaut_rebinding_scope:with_shadowed(
        astranaut_traverse:map_m(F, Expression, Opts)),
      fun(Expression1) ->
                astranaut_traverse_monad:bind(
                  astranaut_rebinding_scope:with_comprehension_generate_pattern(
                    astranaut_traverse:map_m(F, Pattern, Opts#{node => pattern})),
                  fun(Pattern1) ->
                          Generate1 = {GenerateType, Line, Pattern1, Expression1},
                          map_m_qualifiers(F, RestQualifiers, Opts, [Generate1|Acc])
                  end)
      end);
map_m_qualifiers(F, [Expression|RestQualifiers], Opts, Acc) ->
    astranaut_traverse_monad:bind(
      astranaut_traverse:map_m(F, Expression, Opts),
      fun(Expression1) ->
              map_m_qualifiers(F, RestQualifiers, Opts, [Expression1|Acc])
      end);
map_m_qualifiers(_F, [], _Opts, Acc) ->
    astranaut_traverse_monad:return(lists:reverse(Acc)).
