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
                 clause_stack => [function]},
    astranaut_traverse:map_with_state(
        fun(Node, Acc, Attr) ->
                walk_node(Node, Acc, Attr)
        end, Context0, Clause, #{node => form, traverse => all, match_right_first => true}).

walk_node({'case', _Line, _Expression, _Clauses} = Node, 
          #{clause_stack := ClauseStack} = Acc, #{step := pre}) ->
    Acc1 = Acc#{clause_stack => ['case'|ClauseStack]},
    {Node, Acc1};
walk_node({'case', _Line, _Expression, _Clauses} = Node, 
          #{clause_stack := ['case'|ClauseStack]} = Acc,
          #{step := post}) ->
    Acc1 = Acc#{clause_stack => ClauseStack},
    {Node, Acc1};
walk_node({'fun', _Line, {clauses, _Clauses}} = Node, 
          #{clause_stack := ClauseStack} = Acc, 
          #{step := pre}) ->
    Acc1 = Acc#{clause_stack => [function|ClauseStack]},
    {Node, Acc1};
walk_node({'fun', _Line, {clauses, _Clauses}} = Node, 
          #{clause_stack := [function|ClauseStack]} = Acc, 
          #{step := post}) ->
    Acc1 = Acc#{clause_stack => ClauseStack},
    {Node, Acc1};
walk_node({'match', _Line, _Left, _Right} = Node, 
          #{clause_stack := ClauseStack} = Acc, 
          #{step := pre, node := expression}) ->
    Acc1 = Acc#{clause_stack => [match|ClauseStack]},
    {Node, Acc1};
walk_node({'match', _Line, _Left, _Right} = Node, 
          #{clause_stack := [match|ClauseStack]} = Acc, 
          #{step := post, node := expression}) ->
    Acc1 = Acc#{clause_stack => ClauseStack},
    {Node, Acc1};
walk_node({named_fun, _Line, _Name, _Clauses} = Node, 
          #{clause_stack := ClauseStack} = Acc, 
          #{step := pre}) ->
    Acc1 = Acc#{clause_stack => [function|ClauseStack]},
    {Node, Acc1};
walk_node({named_fun, _Line, _Name, _Clauses} = Node, 
          #{clause_stack := [function|ClauseStack]} = Acc,
          #{step := post}) ->
    Acc1 = Acc#{clause_stack => ClauseStack},
    {Node, Acc1};
walk_node({clause, _Line, _Patterns, _Match, _Body} = Node, 
          #{clause_stack := [function|_T]} = Acc, 
          #{step := pre}) ->
    Acc1 = entry_scope(Acc),
    {Node, Acc1};
walk_node({clause, _Line, _Patterns, _Match, _Body} = Node, 
          #{clause_stack := [function|_T]} = Acc, 
          #{step := post}) ->
    Acc1 = exit_scope(Acc),
    {Node, Acc1};
walk_node({clause, _Line, _Patterns, _Match, _Body} = Node, 
          #{} = Acc, 
          #{step := pre}) ->
    Acc1 = entry_rename_map_scope(Acc),
    {Node, Acc1};
walk_node({clause, _Line, _Patterns, _Match, _Body} = Node, 
          #{} = Acc, 
          #{step := post}) ->
    Acc1 = exit_rename_map_scope(Acc),
    {Node, Acc1};
%% rename var if current node is expression.
walk_node({var, _Line, '_'} = Var, #{} = Acc, #{}) ->
    {Var, Acc};
walk_node({var, _Line, _Varname} = Var, #{} = Acc, #{node := expression}) ->
    {rename_var(Var, Acc), Acc};
%% rename var if current node is guard.
walk_node({var, _Line, _Varname} = Var, #{} = Acc, #{node := guard}) ->
    {rename_var(Var, Acc), Acc};
%% rebind var if current node is function pattern.
walk_node({var, _Line, Varname} = Var, 
          #{clause_stack := [function|_T], function_vars := FunctionVars} = Acc, #{node := pattern}) ->
    case ordsets:is_element(Varname, FunctionVars) of
        true ->
            Var1 = rename_var(Var, Acc),
            {Var1, Acc};
        false ->
            FunctionVars1 = ordsets:add_element(Varname, FunctionVars),
            Acc1 = Acc#{function_vars => FunctionVars1},
            add_var(Var, Acc1)
    end;
%% rebind var if current node is match pattern.
walk_node({var, _Line, _Varname} = Var, #{clause_stack := [match|_T]} = Acc, #{node := pattern}) ->
    add_var(Var, Acc);
walk_node({var, _Line, _Varname} = Var, #{} = Acc, #{}) ->
    {rename_var(Var, Acc), Acc};
walk_node(Node, Acc, #{}) ->
    {Node, Acc}.

add_var({var, Line, Varname} = Var, 
        #{global_varnames := GlobalVarnames,
          local_varnames := LocalVarnames,
          global_rename_map := GlobalRenameMap,
          local_rename_map := LocalRenameMap
         } = Context) ->
    case ordsets:is_element(Varname, GlobalVarnames) of
        true ->
            Varname1 = new_variable_name(Varname, GlobalVarnames),
            Var1 = {var, Line, Varname1},
            GlobalVarnames1 = ordsets:add_element(Varname1, GlobalVarnames),
            LocalVarnames1 = ordsets:add_element(Varname1, LocalVarnames),
            GlobalRenameMap1 = maps:put(Varname, Varname1, GlobalRenameMap),
            LocalRenameMap1 = maps:put(Varname, Varname1, LocalRenameMap),

            Context1 = Context#{global_varnames => GlobalVarnames1,
                                local_varnames => LocalVarnames1,
                                global_rename_map := GlobalRenameMap1,
                                local_rename_map := LocalRenameMap1},
            {Var1, Context1};
        false ->
            GlobalVarnames1 = ordsets:add_element(Varname, GlobalVarnames),
            LocalVarnames1 = ordsets:add_element(Varname, LocalVarnames),
            Context1 = Context#{global_varnames => GlobalVarnames1, local_varnames => LocalVarnames1},
            {Var, Context1}
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

entry_scope(Context) ->
    Context1 = entry_varname_scope(Context),
    Context2 = entry_rename_map_scope(Context1),
    Context2#{function_names => ordsets:new()}.

exit_scope(Context) ->
    Context1 = exit_varname_scope(Context),
    exit_rename_map_scope(Context1).

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

add_suffix(Variable, N) ->
    list_to_atom(atom_to_list(Variable) ++ "_" ++ integer_to_list(N)).
