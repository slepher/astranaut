%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2019, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 25 Sep 2019 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_rebinding_scope).

-record(scope_context, {local_varnames    = ordsets:new(),
                        local_renames     = maps:new(),
                        global_varnames   = ordsets:new(),  
                        global_renames    = maps:new(),
                        pattern_varnames  = ordsets:new(),
                        varnames_stack    = [],
                        renames_stack     = [],
                        scope_group_stack = []
                       }).
%% API
-export([with_scope_type/2, entry_scope_type/2, exit_scope_type/2]).
-export([entry_scope_group/1, exit_scope_group/1]).
-export([entry_nonfun_clause/1, exit_nonfun_clause/1]).
-export([entry_funcall_argument/1, exit_funcall_argument/1]).
-export([entry_shadowed/1, exit_shadowed/1]).
-export([entry_pattern_group/1, exit_pattern_group/1]).
%%%===================================================================
%%% API
%%%===================================================================
with_scope_type(ScopeType, NodeM) ->
    astranaut_traverse_monad:then(
      astranaut_traverse_monad:modify(fun(Context) -> entry_scope_type(ScopeType, Context) end),
      astranaut_traverse_monad:bind(
        NodeM,
        fun(Node) ->
                astranaut_traverse_monad:then(
                  astranaut_traverse_monad:modify(fun(Context) -> exit_scope_type(ScopeType, Context) end),
                  astranaut_traverse_monad:return(Node))
        end)).

entry_scope_type(scope_group, Context) ->
    entry_scope_group(Context);
entry_scope_type(nonfun_clause, Context) ->
    entry_nonfun_clause(Context);
entry_scope_type(funcall_argument, Context) ->
    entry_funcall_argument(Context);
entry_scope_type(shadowed, Context) ->
    entry_shadowed(Context);
entry_scope_type(pattern_group, Context) ->
    entry_pattern_group(Context).

exit_scope_type(scope_group, Context) ->
    exit_scope_group(Context);
exit_scope_type(nonfun_clause, Context) ->
    exit_nonfun_clause(Context);
exit_scope_type(funcall_argument, Context) ->
    exit_funcall_argument(Context);
exit_scope_type(shadowed, Context) ->
    exit_shadowed(Context);
exit_scope_type(pattern_group, Context) ->
    exit_pattern_group(Context).

entry_scope_group(#scope_context{scope_group_stack = ScopeStack} = Context) ->
    ScokeStack1 = [{ordsets:new(), maps:new()}|ScopeStack],
    Context#scope_context{scope_group_stack = ScokeStack1}.

exit_scope_group(#scope_context{local_varnames    = LocalVarnames,
                                local_renames     = LocalRenames,
                                global_varnames   = GlobalVarnames,
                                global_renames    = GlobalRenames,
                                scope_group_stack = [{ScopeVarnames, ScopeRenames}|ScopeStack]} = Context) ->
    LocalVarnames1 = ordsets:union(LocalVarnames, ScopeVarnames),
    LocalRenames1 = maps:merge(LocalRenames, ScopeRenames),
    GlobalVarnames1 = ordsets:union(GlobalVarnames, ScopeVarnames),
    GlobalRenames1 = maps:merge(GlobalRenames, ScopeRenames),
    Context#scope_context{local_varnames    = LocalVarnames1,
                          local_renames     = LocalRenames1,
                          global_varnames   = GlobalVarnames1, 
                          global_renames    = GlobalRenames1,
                          scope_group_stack = ScopeStack}.

entry_nonfun_clause(Context) ->
    Context1 = push_varname_stack(Context),
    push_rename_stack(Context1).

exit_nonfun_clause(#scope_context{local_varnames    = LocalVarnames,
                                  scope_group_stack = [{ScopeVarnames, ScopeRenames}|ScopeGroupStack]} = Context) ->
    Context1 = pop_varname_stack(Context),
    Context2 = pop_rename_stack(Context1),
    ScopeVarnames1 = ordsets:union(LocalVarnames, ScopeVarnames),
    ScopeGroupStack1 = [{ScopeVarnames1, ScopeRenames}|ScopeGroupStack],
    Context2#scope_context{scope_group_stack = ScopeGroupStack1}.

entry_funcall_argument(Context) ->
    push_rename_stack(Context).

exit_funcall_argument(#scope_context{local_varnames    = LocalVarnames,
                                     local_renames     = LocalRenames,
                                     scope_group_stack = [{ScopeVarnames, ScopeRenames}|ScopeGroupStack]} = Context) -> 
    Context1 = pop_rename_stack(Context),
    ScopeVarnames1 = ordsets:union(LocalVarnames, ScopeVarnames),
    ScopeRenames1 = maps:merge(LocalRenames, ScopeRenames),
    ScopeGroupStack1 = [{ScopeVarnames1, ScopeRenames1}|ScopeGroupStack],
    Context1#scope_context{scope_group_stack = ScopeGroupStack1}.

entry_shadowed(Context) ->
    Context1 = push_varname_stack(Context),
    push_rename_stack(Context1).

exit_shadowed(Context) ->
    Context1 = pop_varname_stack(Context),
    pop_rename_stack(Context1).

entry_pattern_group(#scope_context{} = Context) ->
    Context#scope_context{pattern_varnames = ordsets:new()}.

exit_pattern_group(#scope_context{} = Context) ->
    Context#scope_context{pattern_varnames = ordsets:new()}.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
push_varname_stack(#scope_context{local_varnames = LocalVarnames, 
                                  varnames_stack = VarnamesStack} = Context) ->
    LocalVarnames1 = ordsets:new(),
    VarnamesStack1 = [LocalVarnames|VarnamesStack],
    Context#scope_context{local_varnames = LocalVarnames1, 
                          varnames_stack = VarnamesStack1}.

pop_varname_stack(#scope_context{varnames_stack = [LocalVarnames|ParentVarnameStack] = VarnameStack} = Context) ->
    GlobalVarnames = ordsets:union(VarnameStack),
    Context#scope_context{local_varnames  = LocalVarnames,
                          global_varnames = GlobalVarnames, 
                          varnames_stack  = ParentVarnameStack}.

push_rename_stack(#scope_context{local_renames = LocalRenames,
                                 renames_stack = RenameStack
                    } = Context) ->
    LocalRenames1 = maps:new(),
    RenameStack1 = [LocalRenames|RenameStack],
    Context#{local_renames => LocalRenames1,
             renames_stack => RenameStack1}.

pop_rename_stack(#scope_context{renames_stack = [LocalRenames|ParentRenamesStack]} = Context) ->
    GlobalRenames = 
        lists:foldl(
          fun(Renames, Acc) ->
                  maps:merge(Renames, Acc)
          end, LocalRenames, ParentRenamesStack),
    Context#scope_context{local_renames  = LocalRenames,
                          global_renames = GlobalRenames,
                          renames_stack  = ParentRenamesStack}.
