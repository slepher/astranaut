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
    {RebindingOptionsRec, Warnings} = 
        astranaut_rebinding_options:rebinding_options(Forms),
    FormsMonad = 
        astranaut_traverse_m:then(
          astranaut_traverse_m:warnings(Warnings),
          astranaut_monad:map_m(
            fun(Form) ->
                    walk_form(Form, RebindingOptionsRec)
            end, Forms, astranaut_traverse_m)),
    Return = astranaut_traverse_m:run(FormsMonad, ?MODULE, #{}),
    Return1 = astranaut_monad:lift_m(
                fun({Forms1, _}) ->
                        Forms1
                end, Return, astranaut_return_m),
    astranaut_traverse:parse_transform_return(Return1).

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
                astranaut_monad:map_m(
                  fun(Clause) ->
                          Return = walk_function_clause(Clause, RebindingOptions),
                          astranaut_traverse:fun_return_to_monad(Return, Clause, #{with_state => true})
                  end, Clauses, astranaut_traverse_m),
            astranaut_monad:lift_m(
              fun(Clauses1) ->
                      Function1 = {function, Line, Name, Arity, Clauses1},
                      case maps:get(debug, RebindingOptions, false) of
                          true ->
                              io:format("~s~n", [astranaut:safe_to_string(Function1)]);
                          false ->
                              ok
                      end,
                      Function1
                          
              end, ClausesM, astranaut_traverse_m);
        error ->
            astranaut_traverse_m:return(Function)
    end;
walk_form(Form, _RebindingOptionsRec) ->
    astranaut_traverse_m:return(Form).

walk_function_clause(Clause, RebindingOptions) ->
    Context0 = astranaut_rebinding_scope:new_context(),
    astranaut_traverse:mapfold(
        fun(Node, Acc, Attr) ->
                Attr1 = maps:merge(Attr, maps:with(astranaut_rebinding_options:keys(), RebindingOptions)),
                walk_node(Node, Acc, Attr1)
        end, Context0, Clause, #{node => form, traverse => all, match_right_first => true, parent => fun_expr}).

walk_node( Node, #{} = Context, #{step := pre, node := expression} = Attr) ->
    NodeType = erl_syntax:type(Node),
    case is_scope_group(NodeType, Attr) of
        true ->
            Context1 = astranaut_rebinding_scope:entry_scope_group(Context),
            walk_node_1(Node, Context1, Attr);
        false ->
            walk_node_1(Node, Context, Attr)
    end;
walk_node(Node, #{} = Context, #{step := post, node := expression} = Attr) ->
    NodeType = erl_syntax:type(Node),
    case is_scope_group(NodeType, Attr) of
        true ->
            Context1 = astranaut_rebinding_scope:exit_scope_group(Context),
            walk_node_1(Node, Context1, Attr);
        false ->
            walk_node_1(Node, Context, Attr)
    end;

walk_node(Node, Context, #{} = Attr) ->
    walk_node_1(Node, Context, Attr).

is_scope_group(case_expr, #{}) ->
    true;
is_scope_group(if_expr, #{}) ->
    true;
is_scope_group(catch_expr, #{}) ->
    true;
is_scope_group(receive_expr, #{}) ->
    true;
is_scope_group(try_expr, #{}) ->
    true;
is_scope_group(application, #{strict := true}) ->
    true;
is_scope_group(map_expr, #{strict := true}) ->
    true;
is_scope_group(record_expr, #{strict := true}) ->
    true;
is_scope_group(infix_expr, #{strict := true}) ->
    true;
is_scope_group(tuple, #{strict := true}) ->
    true;
is_scope_group(list, #{strict := true}) ->
    true;
is_scope_group(_Type, #{}) ->
    false.

%% walk comprehension
walk_node_1({ComprehensionType, _Line, _Expression, _Qualifiers} = Node, #{} = Context, #{step := pre} = Attr) 
  when (ComprehensionType == lc) or (ComprehensionType == bc) ->
    walk_comprehension(Node, Context, Attr);

%% walk comprehension generate
walk_node_1({GenerateType, _Line, _Pattern, _Expression} = Node, #{} = Context, #{step := pre} = Attr)
    when (GenerateType == generate) or (GenerateType == b_generate) ->
    walk_generate(Node, Context, Attr);

%% walk function call
walk_node_1({call, _Line, _Function, _Args} = Node, #{} = Context, 
            #{step := pre, node := expression, strict := true} = Attr) ->
    walk_function_call(Node, Context, Attr);

walk_node_1({op, _Line, _Op, _Left, _Right} = Node, #{} = Context, 
            #{step := pre, node := expression, strict := true} = Attr) ->
    walk_operator_call(Node, Context, Attr);

walk_node_1({tuple, _Line, _TupleElements} = Node, #{} = Context, 
            #{step := pre, node := expression, strict := true} = Attr) ->
    walk_tuple(Node, Context, Attr);

walk_node_1({cons, _Line, _Head, _Tail} = Node, #{} = Context, 
            #{step := pre, node := expression, strict := true} = Attr) ->
    walk_cons(Node, Context, Attr);

walk_node_1({map, _Line, _MapAssociations} = Node, #{} = Context, 
            #{step := pre, node := expression, strict := true} = Attr) ->
    walk_map(Node, Context, Attr);

walk_node_1({map, _Line, _Map, _MapAssociations} = Node, #{} = Context, 
            #{step := pre, node := expression, strict := true} = Attr) ->
    walk_map(Node, Context, Attr);

walk_node_1({record, _Line, _Name, _RecFields} = Node, #{} = Context, 
            #{step := pre, node := expression, strict := true} = Attr) ->
    walk_record(Node, Context, Attr);

walk_node_1({record, _Line, _Rec, _Name, _RecFields} = Node, #{} = Context, 
            #{step := pre, node := expression, strict := true} = Attr) ->
    walk_record(Node, Context, Attr);

%% walk match 
walk_node_1({match, _Line, _Patterns, _Expressions} = Node, #{} = Context, #{step := pre, node := expression} = Attr) ->
    walk_match(Node, Context, Attr);

%% walk function clause and other clauses
walk_node_1({clause, _Line, _Patterns, _Match, _Body} = Node, #{} = Context, #{step := pre} = Attr) ->
    walk_clause(Node, Context, Attr);

%% walk named fun
walk_node_1({named_fun, _Line, _Name, _Clauses} = Node,  #{} = Context, #{step := pre} = Attr) ->
    walk_named_fun(Node, Context, Attr);

%% do nothing to _
walk_node_1({var, _Line, '_'} = Var, #{} = Context, #{}) ->
    {Var, Context};

%% pin var if var is with + before
walk_node_1({op, _Line1, '+', {var, _Line3, _Varname} = Var}, 
            #{pattern := PatternType} = Context, #{node := pattern, step := pre}) 
  when PatternType == match_left; PatternType == clause_match ->
    Var1 = rename_pinned_var(Var, Context),
    astranaut_walk_return:new(#{node => Var1, state => Context, continue => true});

%% rename var if current node is expression.
walk_node_1({var, _Line, _Varname} = Var, #{} = Context, #{node := expression}) ->
    Var1 = rename_var(Var, Context),
    {Var1, Context};

%% rename var if current node is guard.
walk_node_1({var, _Line, _Varname} = Var, #{} = Context, #{node := guard}) ->
    Var1 = rename_var(Var, Context),
    {Var1, Context};

%% rename var if current node is clause match pattern.
walk_node_1({var, _Line, _Varname} = Var, #{pattern := clause_match} = Context, #{clause_pinned := true}) ->
    Var1 = rename_clause_match_var(Var, Context),
    {Var1, Context};

%% rename var if current node is clause match pattern.
walk_node_1({var, _Line, _Varname} = Var, #{pattern := clause_match} = Context, #{}) ->
    rebind_clause_match_var(Var, Context);

%% rebind var if current node is function pattern.
walk_node_1({var, _Line, _Varname} = Var, #{pattern := function_clause} = Context, #{node := pattern}) ->
    rebind_function_clause_var(Var, Context);

%% rebind var if current node is match pattern.
walk_node_1({var, _Line, _Varname} = Var, #{pattern := match_left} = Context, #{node := pattern}) ->
    rebind_match_left_var(Var, Context);

%% rebind var if current node is comprehension_generate pattern.
walk_node_1({var, _Line, _Varname} = Var, 
            #{pattern := comprehension_generate} = Context, #{node := pattern}) ->
    rebind_comprehension_generate_var(Var, Context);

walk_node_1(Node, Acc, #{}) ->
    {Node, Acc}.

clause_scope_type(fun_expr) ->
    shadowed;
clause_scope_type(named_fun_expr) ->
    shadowed;
clause_scope_type(_Other) ->
    nonfun_clause.

walk_comprehension(Node, Context, Attr) ->
    Sequence = fun astranaut_traverse:deep_r_sequence_m/1,
    FNode = fun astranaut_rebinding_scope:with_shadowed/1,
    walk_sequence_children(Sequence, FNode, Node, Context, #{}, Attr).

walk_generate(Node, Context, Attr) ->
    Sequence = fun([PatternMs, ExpressionMs]) ->
                       PatternsM1 = astranaut_rebinding_scope:with_comprehension_generate_pattern(PatternMs),
                       ExpressionsM1 = astranaut_rebinding_scope:with_shadowed(ExpressionMs),
                       %% walk expression first
                       astranaut_traverse:deep_r_sequence_m([PatternsM1, ExpressionsM1])
               end,
    walk_sequence_children(Sequence, Node, Context, #{}, Attr).

walk_function_call(Node, Context, Attr) ->
    Sequence = 
        fun([FunctionM, FunctionArgMs]) ->
                FunctionArgMs1 = lists:map(fun astranaut_rebinding_scope:with_funcall_argument/1, FunctionArgMs),
                astranaut_traverse:deep_sequence_m([FunctionM, FunctionArgMs1])
        end,
    walk_sequence_children(Sequence, Node, Context, #{}, Attr).

walk_operator_call(Node, Context, Attr) ->
    Sequence = 
        fun([LeftMs, OpM, RightMs]) ->
                LeftMs1 = lists:map(fun astranaut_rebinding_scope:with_funcall_argument/1, LeftMs),
                RightMs1 = lists:map(fun astranaut_rebinding_scope:with_funcall_argument/1, RightMs),
                astranaut_traverse:deep_sequence_m([LeftMs1, OpM, RightMs1])
        end,
    walk_sequence_children(Sequence, Node, Context, #{}, Attr).

walk_tuple(Node, Context, Attr) ->
    Sequence = 
        fun([TupleElementMs]) ->
                TupleElementMs1 = lists:map(fun astranaut_rebinding_scope:with_funcall_argument/1, TupleElementMs),
                astranaut_traverse:deep_sequence_m([TupleElementMs1])
        end,
    walk_sequence_children(Sequence, Node, Context, #{}, Attr).

walk_cons(Node, Context, Attr) ->
    Sequence = 
        fun([HeadMs]) ->
                HeadMs1 = lists:map(fun astranaut_rebinding_scope:with_funcall_argument/1, HeadMs),
                astranaut_traverse:deep_sequence_m([HeadMs1]);
           ([HeadMs, TailMs]) ->
                HeadMs1 = lists:map(fun astranaut_rebinding_scope:with_funcall_argument/1, HeadMs),
                TailMs1 = lists:map(fun astranaut_rebinding_scope:with_funcall_argument/1, TailMs),
                astranaut_traverse:deep_sequence_m([HeadMs1, TailMs1])
        end,
    walk_sequence_children(Sequence, Node, Context, #{}, Attr).

walk_map(Node, Context, Attr) ->
    Sequence = 
        fun([MapElementMs]) ->
                MapElementMs1 = lists:map(fun astranaut_rebinding_scope:with_funcall_argument/1, MapElementMs),
                astranaut_traverse:deep_sequence_m([MapElementMs1]);
           ([MapMs, MapElementMs]) ->
                MapMs1 = lists:map(fun astranaut_rebinding_scope:with_funcall_argument/1, MapMs),
                MapElementMs1 = lists:map(fun astranaut_rebinding_scope:with_funcall_argument/1, MapElementMs),
                astranaut_traverse:deep_sequence_m([MapMs1, MapElementMs1])
        end,
    walk_sequence_children(Sequence, Node, Context, #{}, Attr).

walk_record(Node, Context, Attr) ->
    Sequence = 
        fun([NameMs, RecFieldMs]) ->
                RecFieldMs1 = lists:map(fun astranaut_rebinding_scope:with_funcall_argument/1, RecFieldMs),
                astranaut_traverse:deep_sequence_m([NameMs, RecFieldMs1]);
           ([RecMs, NameMs, RecFieldMs]) ->
                RecMs1 = lists:map(fun astranaut_rebinding_scope:with_funcall_argument/1, RecMs),
                RecFieldMs1 = lists:map(fun astranaut_rebinding_scope:with_funcall_argument/1, RecFieldMs),
                astranaut_traverse:deep_sequence_m([RecMs1, NameMs, RecFieldMs1])
        end,
    walk_sequence_children(Sequence, Node, Context, #{}, Attr).


walk_match(Node, Context, Attr) ->
    Sequence = fun([PatternMs, ExpressionM]) ->
                       PatternsM = astranaut_monad:sequence_m(PatternMs, astranaut_traverse_m),
                       PatternsM1 = astranaut_rebinding_scope:with_match_left_pattern(PatternsM),
                       %% walk expression first
                       astranaut_traverse:deep_r_sequence_m([PatternsM1, ExpressionM])
               end,
    walk_sequence_children(Sequence, Node, Context, #{}, Attr).

walk_clause(Node, Context, #{parent := Parent} = Attr) ->
    ScopeType = clause_scope_type(Parent),
    PatternType = astranaut_rebinding_scope:scope_type_pattern(ScopeType),

    Sequence = fun([PatternMs|RestTreesM]) ->
                       PatternsM1 = astranaut_rebinding_scope:with_pattern(PatternType, PatternMs),
                       astranaut_traverse:deep_sequence_m([PatternsM1|RestTreesM])
               end,
    FNode = fun(NodeM) -> astranaut_rebinding_scope:with_scope_type(ScopeType, NodeM) end,
    walk_sequence_children(Sequence, FNode, Node, Context, #{}, Attr).

walk_named_fun(Node, Context, Attr) ->
    Sequence = fun([NameTreeMs|RestTreeMs]) ->
                       NameTreesM1 = astranaut_rebinding_scope:with_function_clause_pattern(NameTreeMs),
                       astranaut_traverse:deep_sequence_m([NameTreesM1|RestTreeMs])
               end,
    FNode = fun astranaut_rebinding_scope:with_shadowed/1,
    walk_sequence_children(Sequence, FNode, Node, Context, #{}, Attr).

rename_pinned_var(Var, Context) ->
    astranaut_rebinding_scope:rename_var(Var, Context).

rename_var(Var, Context) ->
    astranaut_rebinding_scope:rename_var(Var, Context).

rename_clause_match_var(Var, Context) ->
    astranaut_rebinding_scope:rename_var(Var, Context).

rebind_clause_match_var(Var, Context) ->
    astranaut_rebinding_scope:rebind_var(Var, Context).

rebind_function_clause_var(Var, Context) ->
    astranaut_rebinding_scope:rebind_var(Var, Context).

rebind_match_left_var(Var, Context) ->
    astranaut_rebinding_scope:rebind_var(Var, Context).

rebind_comprehension_generate_var(Var, Context) ->
    astranaut_rebinding_scope:rebind_var(Var, Context).

walk_sequence_children(Sequence, Node, Context, #{} = Opts0, Attr) ->
    walk_sequence_children(Sequence, fun(NodeM) -> NodeM end, Node, Context, #{} = Opts0, Attr).

walk_sequence_children(Sequence, FNode, Node, Context, #{} = Opts0, Attr) ->
    Opts = maps:merge(#{node => expression, traverse => all, children => true, sequence_children => Sequence}, Opts0),
    F = astranaut_traverse:transform_mapfold_f(
          fun(Node1, Context1, Attr1) ->
                  Attr2 = maps:merge(Attr1, maps:with(astranaut_rebinding_options:keys(), Attr)),
                  walk_node(Node1, Context1, Attr2)
          end, Opts),
    NodeM = FNode(astranaut_traverse:map_m(F, Node, Opts)),
    continue_node_m(NodeM, Context, Attr).

continue_node_m(NodeM, Context, Attr) ->
    astranaut_traverse_m:then(
      astranaut_traverse_m:put(Context),
      astranaut_traverse_m:bind(
        NodeM,
        fun(Node1) ->
                astranaut_traverse_m:bind(
                  astranaut_traverse_m:get(),
                  fun(Context1) ->
                          PostNode = walk_node(Node1, Context1, Attr#{step => post}),
                          NodeM1 = astranaut_traverse:fun_return_to_monad(PostNode, Node1, #{with_state => true}),
                          astranaut_traverse_m:set_continue(NodeM1)
                  end)
        end)).
