%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2019, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 25 Sep 2019 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_rebinding_scope).

-type scope_context() :: #{local_varnames    => ordsets:ordsets(),
                           local_renames     => maps:maps(),
                           global_varnames   => ordsets:ordsets(),  
                           global_renames    => maps:maps(),
                           pattern_varnames  => ordsets:ordsets(),
                           varnames_stack    => [],
                           renames_stack     => [],
                           scope_group_stack => []
                          }.

-export_type([scope_context/0]).

%% API
-export([new_context/0]).
-export([scope_type_pattern/1]).
-export([rebind_var/2, rename_var/2]).
-export([with_scope_type/2, entry_scope_type/2, exit_scope_type/2]).
-export([with_scope_group/1, entry_scope_group/1, exit_scope_group/1]).
-export([with_nonfun_clause/1, entry_nonfun_clause/1, exit_nonfun_clause/1]).
-export([with_funcall_argument/1, entry_funcall_argument/1, exit_funcall_argument/1]).
-export([with_shadowed/1, entry_shadowed/1, exit_shadowed/1]).
-export([with_pattern/2, entry_pattern/2, exit_pattern/2]).
-export([with_match_left_pattern/1, with_function_clause_pattern/1, 
         with_comprehension_generate_pattern/1, with_clause_match_pattern/1]).
%%%===================================================================
%%% API
%%%===================================================================
new_context() ->
    #{local_varnames   => ordsets:new(),
      local_renames     => maps:new(),
      global_varnames   => ordsets:new(),  
      global_renames    => maps:new(),
      pattern_varnames  => ordsets:new(),
      varnames_stack    => [],
      renames_stack     => [],
      scope_group_stack => []}.

with_scope_group(NodeM) ->
    with_scope_type(scope_group, NodeM).

with_nonfun_clause(NodeM) ->
    with_scope_type(nonfun_clause, NodeM).

with_funcall_argument(NodeM) ->
    with_scope_type(funcall_argument, NodeM).

with_shadowed(NodeM) ->
    with_scope_type(shadowed, NodeM).

with_match_left_pattern(NodeM) ->
    with_pattern(match_left, NodeM).

with_function_clause_pattern(NodeM) ->
    with_pattern(function_clause, NodeM).

with_comprehension_generate_pattern(NodeM) ->
    with_pattern(comprehension_generate, NodeM).

with_clause_match_pattern(NodeM) ->
    with_pattern(clause_match, NodeM).

with_pattern(PatternType, NodeM) ->
    ScopeType = pattern_to_scope_type(PatternType),
    with_scope_type(ScopeType, NodeM).

with_scope_type(ScopeType, NodeMs) when is_list(NodeMs) ->
    NodesM = astranaut_traverse_monad:deep_sequence_m(NodeMs),
    with_scope_type(ScopeType, NodesM);
with_scope_type(ScopeType, NodeM) ->
    astranaut_traverse_monad:then(
      astranaut_traverse_monad:modify(fun(Context) -> entry_scope_type(ScopeType, Context) end),
      astranaut_traverse_monad:bind(
        NodeM,
        fun(Node) ->
                case ScopeType of
                    function_clause_pattern ->
                        %% io:format("node is ~p~n", [Node]);
                        ok;
                    _ ->
                        ok
                end,
                astranaut_traverse_monad:then(
                  astranaut_traverse_monad:modify(fun(Context) -> exit_scope_type(ScopeType, Context) end),
                  astranaut_traverse_monad:return(Node))
        end)).

rebind_var({var, Line, Varname} = Var, 
           #{global_varnames   := GlobalVarnames,
             local_varnames    := LocalVarnames,
             global_renames    := GlobalRenameMap,
             local_renames     := LocalRenameMap,
             pattern_varnames  := PatternVarnames
            } = Context) ->
    case ordsets:is_element(Varname, PatternVarnames) of
        true ->
            Var1 = rename_var(Var, Context),
            {Var1, Context};
        false ->
            PatternVarnames1 = ordsets:add_element(Varname, PatternVarnames),
            Context1 = Context#{pattern_varnames => PatternVarnames1},

            case ordsets:is_element(Varname, GlobalVarnames) of
                true ->
                    Varname1 = new_variable_name(Varname, GlobalVarnames),
                    Var1 = {var, Line, Varname1},
                    GlobalVarnames1 = ordsets:add_element(Varname1, GlobalVarnames),
                    LocalVarnames1 = ordsets:add_element(Varname1, LocalVarnames),
                    GlobalRenameMap1 = maps:put(Varname, Varname1, GlobalRenameMap),
                    LocalRenameMap1 = maps:put(Varname, Varname1, LocalRenameMap),

                    Context2 = Context1#{global_varnames  => GlobalVarnames1,
                                         local_varnames   => LocalVarnames1,
                                         global_renames   => GlobalRenameMap1,
                                         local_renames    => LocalRenameMap1,
                                         pattern_varnames => PatternVarnames1
                                        },
                    {Var1, Context2};
                false ->
                    GlobalVarnames1 = ordsets:add_element(Varname, GlobalVarnames),
                    LocalVarnames1 = ordsets:add_element(Varname, LocalVarnames),
                    Context2 = Context1#{global_varnames => GlobalVarnames1, 
                                         local_varnames => LocalVarnames1
                                        },
                    {Var, Context2}
            end
    end.

rename_var({var, Line, Varname} = Var, #{global_renames := Renames}) ->
    case maps:find(Varname, Renames) of
        {ok, Varname1} ->
            {var, Line, Varname1};
        error ->
            Var
    end.

entry_scope_type(scope_group, Context) ->
    entry_scope_group(Context);
entry_scope_type(nonfun_clause, Context) ->
    entry_nonfun_clause(Context);
entry_scope_type(funcall_argument, Context) ->
    entry_funcall_argument(Context);
entry_scope_type(shadowed, Context) ->
    entry_shadowed(Context);
entry_scope_type(ScopeType, Context) ->
    PatternType = scope_to_pattern_type(ScopeType),
    entry_pattern(PatternType, Context).

exit_scope_type(scope_group, Context) ->
    exit_scope_group(Context);
exit_scope_type(nonfun_clause, Context) ->
    exit_nonfun_clause(Context);
exit_scope_type(funcall_argument, Context) ->
    exit_funcall_argument(Context);
exit_scope_type(shadowed, Context) ->
    exit_shadowed(Context);
exit_scope_type(ScopeType, Context) ->
    PatternType = scope_to_pattern_type(ScopeType),
    exit_pattern(PatternType, Context).

entry_scope_group(#{scope_group_stack := ScopeStack} = Context) ->
    ScokeStack1 = [{ordsets:new(), maps:new()}|ScopeStack],
    Context#{scope_group_stack => ScokeStack1}.

exit_scope_group(#{local_varnames    := LocalVarnames,
                   local_renames     := LocalRenames,
                   global_varnames   := GlobalVarnames,
                   global_renames    := GlobalRenames,
                   scope_group_stack := [{ScopeVarnames, ScopeRenames}|ScopeStack]} = Context) ->
    LocalVarnames1 = ordsets:union(LocalVarnames, ScopeVarnames),
    LocalRenames1 = maps:merge(LocalRenames, ScopeRenames),
    GlobalVarnames1 = ordsets:union(GlobalVarnames, ScopeVarnames),
    GlobalRenames1 = maps:merge(GlobalRenames, ScopeRenames),
    Context#{local_varnames    => LocalVarnames1,
             local_renames     => LocalRenames1,
             global_varnames   => GlobalVarnames1, 
             global_renames    => GlobalRenames1,
             scope_group_stack => ScopeStack}.

entry_nonfun_clause(Context) ->
    Context1 = push_varname_stack(Context),
    push_rename_stack(Context1).

exit_nonfun_clause(#{local_varnames    := LocalVarnames,
                     scope_group_stack := [{ScopeVarnames, ScopeRenames}|ScopeGroupStack]} = Context) ->
    Context1 = pop_varname_stack(Context),
    Context2 = pop_rename_stack(Context1),
    ScopeVarnames1 = ordsets:union(ScopeVarnames, LocalVarnames),
    ScopeGroupStack1 = [{ScopeVarnames1, ScopeRenames}|ScopeGroupStack],
    Context2#{scope_group_stack => ScopeGroupStack1}.

entry_funcall_argument(Context) ->
    push_rename_stack(Context).

exit_funcall_argument(#{local_varnames    := LocalVarnames,
                        local_renames     := LocalRenames,
                        scope_group_stack := [{ScopeVarnames, ScopeRenames}|ScopeGroupStack]} = Context) -> 
    Context1 = pop_rename_stack(Context),
    ScopeVarnames1 = ordsets:union(ScopeVarnames, LocalVarnames),
    ScopeRenames1 = maps:merge(ScopeRenames, LocalRenames),
    ScopeGroupStack1 = [{ScopeVarnames1, ScopeRenames1}|ScopeGroupStack],
    Context1#{scope_group_stack => ScopeGroupStack1}.

entry_shadowed(Context) ->
    Context1 = push_varname_stack(Context),
    push_rename_stack(Context1).

exit_shadowed(Context) ->
    Context1 = pop_varname_stack(Context),
    pop_rename_stack(Context1).

entry_pattern(PatternType, #{} = Context) ->
    %% io:format("entry pattern ~p ~p~n", [PatternType, Context]),
    Context#{pattern => PatternType, pattern_varnames => ordsets:new()}.

exit_pattern(PatternType, #{pattern := PatternType} = Context) ->
    %% io:format("exit pattern ~p ~p~n", [PatternType, Context]),
    Context1 = maps:remove(pattern, Context),
    Context1#{pattern_varnames => ordsets:new()}.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
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

add_suffix(Variable, N) ->
    list_to_atom(atom_to_list(Variable) ++ "_" ++ integer_to_list(N)).

push_varname_stack(#{local_varnames := LocalVarnames, 
                     varnames_stack := VarnamesStack} = Context) ->
    LocalVarnames1 = ordsets:new(),
    VarnamesStack1 = [LocalVarnames|VarnamesStack],
    Context#{local_varnames => LocalVarnames1, 
             varnames_stack => VarnamesStack1}.

pop_varname_stack(#{varnames_stack := [LocalVarnames|ParentVarnameStack] = VarnameStack} = Context) ->
    GlobalVarnames = ordsets:union(VarnameStack),
    Context#{local_varnames  => LocalVarnames,
             global_varnames => GlobalVarnames, 
             varnames_stack  => ParentVarnameStack}.

push_rename_stack(#{local_renames := LocalRenames,
                    renames_stack := RenameStack
                   } = Context) ->
    LocalRenames1 = maps:new(),
    RenameStack1 = [LocalRenames|RenameStack],
    Context#{local_renames => LocalRenames1,
             renames_stack => RenameStack1}.

pop_rename_stack(#{renames_stack := [LocalRenames|ParentRenamesStack]} = Context) ->
    GlobalRenames = 
        lists:foldl(
          fun(Renames, Acc) ->
                  maps:merge(Renames, Acc)
          end, LocalRenames, ParentRenamesStack),
    Context#{local_renames  => LocalRenames,
             global_renames => GlobalRenames,
             renames_stack  => ParentRenamesStack}.

scope_type_pattern(shadowed) ->
    function_clause;
scope_type_pattern(nonfun_clause) ->
    clause_match.

pattern_to_scope_type(function_clause) ->
    function_clause_pattern;
pattern_to_scope_type(clause_match) ->
    clause_match_pattern;
pattern_to_scope_type(comprehension_generate) ->
    comprehension_generate_pattern;
pattern_to_scope_type(match_left) ->
    match_left_pattern.

scope_to_pattern_type(function_clause_pattern) ->
    function_clause;
scope_to_pattern_type(clause_match_pattern) ->
    clause_match;
scope_to_pattern_type(comprehension_generate_pattern) ->
    comprehension_generate;
scope_to_pattern_type(match_left_pattern) ->
    match_left.





