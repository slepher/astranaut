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
-export([parse_transform/2]).

%%%===================================================================
%%% API
%%%===================================================================
parse_transform(Forms, _Options) ->
    erlang:system_flag(backtrace_depth, 30),
    %dbg:tracer(),
    %dbg:tpl(astranaut_rebinding, add_var, cx),
    %dbg:p(self(), [c]),
    FormsMonad = 
        astranaut_traverse_monad:map_m(
          fun(Form) ->
                  walk_form(Form)
          end, Forms),
    File = astranaut:file(Forms),
    Return = astranaut_traverse_monad:eval(FormsMonad, #{}),
    astranaut_traverse:map_traverse_return(
      fun(Forms1) ->
              io:format("~s~n", [astranaut:safe_to_string(Forms1)])
      end, Return),
    astranaut_traverse:parse_transform_return(Return, File).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
walk_form({function, LINE, Name, Arity, Clauses}) ->
    ClausesM = 
        astranaut_traverse_monad:map_m(
          fun(Clause) ->
                  Return = walk_function_clause(Clause),
                  astranaut_traverse:fun_return_to_monad(Return, Clause, #{with_state => true})
          end, Clauses),
    astranaut_traverse_monad:lift_m(
      fun(Clauses1) ->
              {function, LINE, Name, Arity, Clauses1}
      end, ClausesM);
walk_form(Form) ->
    astranaut_traverse_monad:return(Form).

walk_function_clause(Clause) ->
    Context0 = #{global_varnames => ordsets:new(), 
                 local_varnames => ordsets:new(),
                 global_rename_map => maps:new(),
                 local_rename_map => maps:new(), 
                 scope_varnames_stack => [], 
                 scope_rename_map_stack => [],
                 clause_stack => [fun_expr]},
    astranaut_traverse:map_with_state(
        fun(Node, Acc, Attr) ->
                walk_node(Node, Acc, Attr)
        end, Context0, Clause, #{node => form, traverse => all, match_right_first => true}).

walk_node(Node, #{clause_stack := ClauseStack} = Context, #{step := pre} = Attr) ->
    NodeType = erl_syntax:type(Node),
    case clause_parent_type(NodeType) of
        none ->
            walk_node_1(Node, Context, Attr);
        ClauseParentType ->
            Context1 = Context#{clause_stack => [ClauseParentType|ClauseStack]},
            walk_node_1(Node, Context1, Attr)
    end;
walk_node(Node, #{clause_stack := [ClauseParentType|ClauseStack]} = Context, #{step := post} = Attr) ->
    NodeType = erl_syntax:type(Node),
    ClauseParentType1 = clause_parent_type(NodeType),
    case match_clause_partent_type(ClauseParentType, ClauseParentType1) of
        true ->
            Context1 = Context#{clause_stack => ClauseStack},
            walk_node_1(Node, Context1, Attr);
        false ->
            walk_node_1(Node, Context, Attr)
    end;
walk_node(Node, Context, #{} = Attr) ->
    walk_node_1(Node, Context, Attr).

clause_parent_type(case_expr) ->
    case_expr;
clause_parent_type(catch_expr) ->
    catch_expr;
clause_parent_type(receive_expr) ->
    receive_expr;
clause_parent_type(try_expr) ->
    try_expr;
clause_parent_type(fun_expr) ->
    fun_expr;
clause_parent_type(named_fun_expr) ->
    fun_expr;
clause_parent_type(match_expr) ->
    {match_expr, false};
clause_parent_type(_Type) ->
    none.

match_clause_partent_type({match_expr, _}, {match_expr, _}) ->
    true;
match_clause_partent_type(ClauseParentType, ClauseParentType) ->
    true;
match_clause_partent_type(_ClauseParentType1, _ClauseParentType2) ->
    false.

walk_node_1({'match', Line, Patterns, Expressions}, 
          #{} = Context, 
          #{step := pre, node := expression} = Attr) ->
    Opts = #{node => expression, traverse => all, match_right_first => true},
    F = astranaut_traverse:transform_mapfold_f(fun walk_node/3, Opts),
    NodeM =
        astranaut_traverse_monad:bind(
          astranaut_traverse:map_m(F, Expressions, Opts#{node => expression}),
          fun(NExpressions) ->
                  astranaut_traverse_monad:bind(
                    astranaut_traverse:map_m(F, Patterns, Opts#{node => pattern}),
                    fun(NPatterns) ->
                            astranaut_traverse_monad:return({'match', Line, NPatterns, NExpressions})
                    end)
          end),
    {Node1, Context1} = 
        astranaut_traverse:monad_to_traverse_fun_return(NodeM, #{init => Context, with_state => true}),
    {Node2, Context2} = 
        walk_node(Node1, Context1, Attr#{step => post}),
    astranaut_traverse:traverse_fun_return(#{node => Node2, state => Context2, continue => true});
walk_node_1({named_fun, _Line, _Name, _Clauses} = Node, 
          #{} = Context, 
          #{step := pre}) ->
    Context1 = entry_scope(Context),
    {Node, Context1};
walk_node_1({named_fun, _Line, _Name, _Clauses} = Node, 
            #{} = Context,
            #{step := post}) ->
    Context1 = exit_scope(Context),
    {Node, Context1};
walk_node_1({clause, _Line, _Patterns, _Match, _Body} = Node, 
          #{clause_stack := [fun_expr|_T]} = Context, 
          #{step := pre}) ->
    Context1 = entry_function_scope(Context),
    io:format("entry function scope ~p~n ~p~n", [Context, Context1]),
    {Node, Context1};
walk_node_1({clause, _Line, _Patterns, _Match, _Body} = Node, 
          #{clause_stack := [fun_expr|_T]} = Context, 
          #{step := post}) ->
    Context1 = exit_function_scope(Context),
    io:format("exit function scope ~p~n ~p~n", [Context, Context1]),
    {Node, Context1};
walk_node_1({clause, _Line, _Patterns, _Match, _Body} = Node, 
          #{} = Context, 
          #{step := pre}) ->
    Context1 = entry_scope(Context),
    io:format("entry scope ~p~n ~p~n", [Context, Context1]),
    {Node, Context1};
walk_node_1({clause, _Line, _Patterns, _Match, _Body} = Node, 
          #{} = Context, 
          #{step := post}) ->
    Context1 = exit_scope(Context),
    {Node, Context1};
%% rename var if current node is expression.
walk_node_1({var, _Line, '_'} = Var, #{} = Acc, #{}) ->
    {Var, Acc};
walk_node_1({op, _Line1, '+', {var, _Line3, _Varname} = Var}, 
            #{clause_stack := [{match_expr, _}|_T]} = Context, #{node := pattern, step := pre}) ->
    Var1 = rename_var(Var, Context),
    astranaut_traverse:traverse_fun_return(#{node => Var1, state => Context, continue => true});
walk_node_1({var, _Line, _Varname} = Var, #{} = Acc, #{node := expression}) ->
    {rename_var(Var, Acc), Acc};
%% rename var if current node is guard.
walk_node_1({var, _Line, _Varname} = Var, #{} = Acc, #{node := guard}) ->
    {rename_var(Var, Acc), Acc};
%% rebind var if current node is function pattern.
walk_node_1({var, _Line, _Varname} = Var, 
          #{clause_stack := [fun_expr|_T]} = Context, #{node := pattern}) ->
    add_var(Var, Context);
%% rebind var if current node is match pattern.
walk_node_1({var, _Line, _Varname} = Var, 
            #{clause_stack := [{match_expr, PattenVisited}|T]} = Context, #{node := pattern}) ->
    Context1 = 
        case PattenVisited of
            false ->
                entry_pattern_scope(Context#{clause_stack := [{match_expr, true}|T]});
            true ->
                Context
        end,
    {Var1, Context2} = add_var(Var, Context1),
    io:format("var ~p ~p~n context 1 ~p~n context 2 ~p~n", [Var, Var1, Context1, Context2]),
    {Var1, Context2};
walk_node_1({var, _Line, _Varname} = Var, #{} = Context, #{}) ->
    {rename_var(Var, Context), Context};
walk_node_1(Node, Acc, #{}) ->
    {Node, Acc}.


add_var({var, Line, Varname} = Var, 
        #{global_varnames := GlobalVarnames,
          local_varnames := LocalVarnames,
          global_rename_map := GlobalRenameMap,
          local_rename_map := LocalRenameMap,
          pattern_varnames := PatternVarnames
         } = Context) ->
    case ordsets:is_element(Varname, PatternVarnames) of
        true ->
            Var1 = rename_var(Var, Context),
            {Var1, Context};
        false ->
            case ordsets:is_element(Varname, GlobalVarnames) of
                true ->
                    Varname1 = new_variable_name(Varname, GlobalVarnames),
                    Var1 = {var, Line, Varname1},
                    GlobalVarnames1 = ordsets:add_element(Varname1, GlobalVarnames),
                    LocalVarnames1 = ordsets:add_element(Varname1, LocalVarnames),
                    PatternVarnames1 = ordsets:add_element(Varname, PatternVarnames),
                    GlobalRenameMap1 = maps:put(Varname, Varname1, GlobalRenameMap),
                    LocalRenameMap1 = maps:put(Varname, Varname1, LocalRenameMap),

                    Context1 = Context#{global_varnames => GlobalVarnames1,
                                        local_varnames => LocalVarnames1,
                                        global_rename_map := GlobalRenameMap1,
                                        local_rename_map := LocalRenameMap1,
                                        pattern_varnames => PatternVarnames1
                                       },
                    {Var1, Context1};
                false ->
                    GlobalVarnames1 = ordsets:add_element(Varname, GlobalVarnames),
                    LocalVarnames1 = ordsets:add_element(Varname, LocalVarnames),
                    Context1 = Context#{global_varnames => GlobalVarnames1, local_varnames => LocalVarnames1},
                    {Var, Context1}
            end
    end.

rename_var({var, Line, Varname} = Var, #{global_rename_map := RenameMap}) ->
    case maps:find(Varname, RenameMap) of
        {ok, Varname1} ->
            {var, Line, Varname1};
        error ->
            Var
    end.

new_variable_name(Variable, Variables) ->
    new_variable_name(Variable, Variables, 1).

new_variable_name(Variable, Variables, N) ->
    Variable1 = add_suffix(Variable, N),
    case ordsets:is_element(Variable1, Variables) of
        true ->
            new_variable_name(Variable, Variables, N + 1);
        false ->
            Variable1
    end.

entry_function_scope(Context) ->
    Context1 = entry_varname_scope(Context),
    Context2 = entry_rename_map_scope(Context1),
    entry_pattern_scope(Context2).

exit_function_scope(Context) ->
    Context1 = exit_varname_scope(Context),
    exit_rename_map_scope(Context1).

entry_scope(Context) ->
    Context1 = entry_rename_map_scope(Context),
    entry_pattern_scope(Context1).

exit_scope(Context) ->
    exit_rename_map_scope(Context).

entry_rename_map_scope(#{local_rename_map := LocalRenameMap,
                         scope_rename_map_stack := ScokeRenameMapStack
                        } = Context) ->
    ScokeRenameMapStack1 = [LocalRenameMap|ScokeRenameMapStack],
    LocalRenameMap1 = maps:new(),
    Context#{local_rename_map => LocalRenameMap1,
             scope_rename_map_stack => ScokeRenameMapStack1}.

exit_rename_map_scope(#{scope_rename_map_stack := [LocalRenameMap|ParentLocalRenameMap]
            } = Context) ->
    GlobalRenameMap = 
        lists:foldl(
          fun(RenameMap, Acc) ->
                  maps:merge(RenameMap, Acc)
          end, LocalRenameMap, ParentLocalRenameMap),
    Context#{global_rename_map => GlobalRenameMap,
             local_rename_map => LocalRenameMap,
             scope_rename_map_stack => ParentLocalRenameMap}.

entry_varname_scope(#{local_varnames := LocalVarnames, 
                      scope_varnames_stack := ScopeVarnamesStack
                     } = Context) ->
    ScopeVarnamesStack1 = [LocalVarnames|ScopeVarnamesStack],
    LocalVarnames1 = ordsets:new(),
    Context#{local_varnames => LocalVarnames1, 
             scope_varnames_stack => ScopeVarnamesStack1, 
             function_vars => ordsets:new()}.

exit_varname_scope(#{scope_varnames_stack := [LocalVarnames|ParentLocalVarnames] = ScopeVarnameStack
                    } = Context) ->
    GlobalVarnames = ordsets:union(ScopeVarnameStack),
    Context#{global_varnames => GlobalVarnames, 
             local_varnames => LocalVarnames,
             scope_varnames_stack => ParentLocalVarnames}.

entry_pattern_scope(#{} = Context) ->
    Context#{pattern_varnames => ordsets:new()}.

add_suffix(Variable, N) ->
    list_to_atom(atom_to_list(Variable) ++ "_" ++ integer_to_list(N)).
