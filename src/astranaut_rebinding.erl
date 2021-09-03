%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2019, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 20 Sep 2019 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_rebinding).

-include("do.hrl").

%% API
-export([parse_transform/2, format_error/1]).

-record(rebinding_options, {global_options, fun_options}).
%%%===================================================================
%%% API
%%%===================================================================
-spec parse_transform([erl_parse:abstract_form()], compile:option()) -> [erl_parse:abstract_form()].
parse_transform(Forms, _Options) ->
    Return =
        do([ return ||
               RebindingOptions <- load_attributes(Forms),
               astranaut_traverse:eval(
                 astranaut_traverse:map_m(
                   fun(Form) ->
                           walk_form(Form, RebindingOptions)
                   end, Forms), ?MODULE, #{}, #{})
           ]),
    astranaut_return:to_compiler(Return).

format_error(Message) ->
    case io_lib:deep_char_list(Message) of
        true -> Message;
        _    -> io_lib:write(Message)
    end.
%%%===================================================================
%%% load options
%%%===================================================================
load_attributes(Forms) ->
    do([return ||
           GlobalOptions <-
               astranaut_lib:validate_attribute_option(rebinding_validator(), ?MODULE, rebinding_all, Forms),
           FunOptions <-
               astranaut_lib:with_attribute(
                 fun(Attr, Acc) ->
                         add_fun_options(Attr, Acc)
                 end, #{}, Forms, rebinding_fun,
                 #{formatter => ?MODULE, simplify_return => false}),
           return(#rebinding_options{fun_options = FunOptions, global_options = GlobalOptions})
       ]).

add_fun_options(FName, Acc) when is_atom(FName) ->
    add_fun_options(FName, #{}, Acc);
add_fun_options({FName, Arity}, Acc) when is_atom(FName), is_integer(Arity) ->
    add_fun_options({FName, Arity}, #{}, Acc);
add_fun_options({Function, Options}, Acc) ->
    do([ return ||
           Options1 <- astranaut_lib:validate(rebinding_validator(), Options),
           add_fun_options(Function, Options1, Acc)
       ]);
add_fun_options(Function, Acc) ->
    add_fun_options(Function, #{}, Acc).

add_fun_options(Functions, Options, Acc) when is_list(Functions) ->
    astranaut_return:foldl_m(
      fun(Function, Acc1) ->
              add_fun_options(Function, Options, Acc1)
      end, Acc, Functions);
add_fun_options({FName, Arity}, Options, Acc) when is_atom(FName), is_integer(Arity) ->
    merge_fun_options({FName, Arity}, Options, Acc);
add_fun_options(FName, Options, Acc) when is_atom(FName) ->
    merge_fun_options(FName, Options, Acc);
add_fun_options(Other, _Options, Acc) ->
    astranaut_return:warning_ok({invalid_rebinding_fun, Other}, Acc).

merge_fun_options(Function, Options, Acc) ->
    FAcc = maps:get(Function, Acc, #{}),
    astranaut_return:return(maps:put(Function, maps:merge(FAcc, Options), Acc)).

rebinding_validator() ->
    #{clause_pinned => boolean, strict => boolean, debug => boolean, rebinding => [paired, {default, true}]}.

rebinding_keys() ->
    [clause_pinned, strict].
%%%===================================================================
%%% walk form
%%%===================================================================
walk_form({function, Pos, Name, Arity, Clauses} = Function, RebindingOptionsRec) ->
    case match_rebinding(Name, Arity, RebindingOptionsRec) of
        {ok, RebindingOptions} ->
            ClausesM =
                astranaut_traverse:map_m(
                  fun(Clause) ->
                          walk_function_clause(Clause, RebindingOptions)
                  end, Clauses),
            astranaut_traverse:lift_m(
              fun(Clauses1) ->
                      Function1 = {function, Pos, Name, Arity, Clauses1},
                      case maps:get(debug, RebindingOptions, false) of
                          true ->
                              io:format("~s~n", [astranaut_lib:ast_safe_to_string(Function1)]);
                          false ->
                              ok
                      end,
                      Function1
              end, ClausesM);
        error ->
            astranaut_traverse:return(Function)
    end;
walk_form(Form, _RebindingOptionsRec) ->
    astranaut_traverse:return(Form).

match_rebinding(Name, Arity, RebindingOptionsRec) ->
    RebindingOptions = find_rebinding_options(Name, Arity, RebindingOptionsRec),
    case maps:get(rebinding, RebindingOptions, true) of
        false ->
            error;
        true ->
            {ok, RebindingOptions}
    end.

find_rebinding_options(Name, Arity, #rebinding_options{fun_options = FunOptions, global_options = GlobalOptions}) ->
    case maps:find({Name, Arity}, FunOptions) of
        {ok, Options} ->
            Options;
        error ->
            case maps:find(Name, FunOptions) of
                {ok, Options} ->
                    Options;
                error ->
                    GlobalOptions
            end
    end.

walk_function_clause(Clause, RebindingOptions) ->
    astranaut_traverse:local(
      fun(Attr) -> Attr#{node => form, parent => fun_expr} end,
      do([ traverse ||
             astranaut_traverse:put(new_context()),
             astranaut:map_m(
               fun(Node) ->
                       do([traverse ||
                              Attr <- astranaut_traverse:ask(),
                              Context <- astranaut_traverse:get(),
                              NodeType = astranaut_syntax:type(Node),
                              Attr1 = maps:merge(Attr, maps:with(rebinding_keys(), RebindingOptions)),
                              astranaut_traverse:astranaut_traverse(walk_node(NodeType, Node, Context, Attr1))
                          ])
             end, Clause, #{traverse => pre})
         ])).

%% the + pin operator will be replaced with ^ pin operator after this pull request merged.
%% https://github.com/erlang/otp/pull/2951
walk_node(prefix_expr, {op, _Pos1, '+', {var, _Pos3, _Varname} = Var},
          #{pattern := PatternType} = Context, #{node := pattern})
  when PatternType == match_left; PatternType == clause_match ->
    Var1 = rename_var(Var, Context),
    astranaut:walk_return(#{return => Var1, continue => true});

%% rename var if current node is expression.
walk_node(variable, Var, #{} = Context, #{node := expression}) ->
    to_walk_return(Var, rename_var(Var, Context));

%% rename var if current node is guard.
walk_node(variable, Var, #{} = Context, #{node := guard}) ->
    to_walk_return(Var, rename_var(Var, Context));

%% rename var if current node is clause match pattern.
walk_node(variable, Var, #{pattern := clause_match} = Context, #{clause_pinned := true}) ->
    to_walk_return(Var, rename_var(Var, Context));

%% rename var if current node is clause match pattern.
walk_node(variable, Var, #{pattern := clause_match} = Context, #{}) ->
    to_walk_return(Var, rebind_var(Var, Context));

%% rebind var if current node is function pattern.
walk_node(variable, Var, #{pattern := function_clause} = Context, #{node := pattern}) ->
    to_walk_return(Var, rebind_var(Var, Context));

%% rebind var if current node is match pattern.
walk_node(variable, Var, #{pattern := match_left} = Context, #{node := pattern}) ->
    to_walk_return(Var, rebind_var(Var, Context));

%% rebind var if current node is comprehension_generate pattern.
walk_node(variable, Var, #{pattern := comprehension_generate} = Context, #{node := pattern}) ->
    to_walk_return(Var, rebind_var(Var, Context));

walk_node(Type, Node, _Context, Attr) ->
    Node1 = walk_node_1(Type, Node, Attr),
    astranaut:walk_return(
      astranaut_uniplate:up_attr(
        #{parent => Type},
        astranaut_uniplate:with_subtrees(
        fun(Subtrees) ->
                astranaut_syntax:subtrees_pge(Type, Subtrees, Attr)
        end, Node1))).

walk_node_1(infix_expr, _Expr, #{node := expression, strict := true}) ->
    walk_scope_group_expression();

%% walk function call
walk_node_1(application, _Expr, #{node := expression, strict := true}) ->
    walk_scope_group_expression();

walk_node_1(tuple, _Expr, #{node := expression, strict := true}) ->
    walk_scope_group_expression();

walk_node_1(list, _Expr, #{node := expression, strict := true}) ->
    walk_scope_group_expression();

walk_node_1(map_expr, _Expr, #{node := expression, strict := true}) ->
    walk_scope_group_expression();

walk_node_1(record_expr, _Expr, #{node := expression, strict := true}) ->
    walk_scope_group_expression();

%% walk comprehension
walk_node_1(list_comp, _ListComp, #{}) ->
    walk_comprehension();

walk_node_1(binary_comp, _BinaryComp, #{}) ->
    walk_comprehension();

%% walk comprehension generate
walk_node_1(generator, _ListGenerator, #{}) ->
    walk_generate();

walk_node_1(binary_generator, _BinaryGenerator, #{}) ->
    walk_generate();

%% walk match
walk_node_1(match_expr, _Match,  #{node := expression}) ->
    walk_match();

%% walk function clause and other clauses
walk_node_1(clause, _Clause, #{} = Attr) ->
    walk_clause(Attr);

%% walk named fun
walk_node_1(named_fun_expr, _NamedFun, #{}) ->
    walk_named_fun();

walk_node_1(case_expr, _Case, #{}) ->
    walk_clause_parent_expression();

walk_node_1(if_expr, _If, #{}) ->
    walk_clause_parent_expression();

walk_node_1(receive_expr, _Receive, #{}) ->
    walk_clause_parent_expression();

walk_node_1(try_expr, _Try, #{}) ->
    walk_clause_parent_expression();

walk_node_1(catch_expr, _Catch, #{}) ->
    walk_clause_parent_expression();

walk_node_1(_NodeType, _Node, #{}) ->
    keep.

to_walk_return(Var, {Var, Context1}) ->
    astranaut:walk_return(#{return => keep, state => Context1});
to_walk_return(_Var, {Var1, Context1}) ->
    astranaut:walk_return(#{return => Var1, state => Context1});
to_walk_return(Var, Var) ->
    astranaut:walk_return(#{return => keep});
to_walk_return(_Var, Var1) ->
    astranaut:walk_return(#{return => Var1}).

clause_scope_type(fun_expr) ->
    shadowed;
clause_scope_type(named_fun_expr) ->
    shadowed;
clause_scope_type(_Other) ->
    nonfun_clause.

walk_comprehension() ->
    Sequence =
        fun(Subtrees) ->
                with_shadowed(lists:reverse(Subtrees))
        end,
    Reduce = fun lists:reverse/1,
    astranaut_uniplate:with_subtrees(Sequence, Reduce).

walk_generate() ->
    Sequence =
        fun([Patterns, Expressions]) ->
                Patterns1 = with_comprehension_generate_pattern(Patterns),
                Expressions1 = with_shadowed(Expressions),
                %% walk expression first
                [Expressions1, Patterns1]
        end,
    Reduce = fun lists:reverse/1,
    astranaut_uniplate:with_subtrees(Sequence, Reduce).

walk_match() ->
    Sequence =
        fun([Patterns, Expressions]) ->
                Patterns1 = with_match_left_pattern(Patterns),
                %% walk expression first
                [Expressions, Patterns1]
        end,
    Reduce = fun lists:reverse/1,
    astranaut_uniplate:with_subtrees(Sequence, Reduce).

walk_clause(#{parent := Parent}) ->
    ScopeType = clause_scope_type(Parent),
    PatternType = scope_type_pattern(ScopeType),
    Sequence =
        fun([Patterns, Guards, Expressions]) ->
                Patterns1 = with_scope_type(PatternType, Patterns),
                with_scope_type(ScopeType, [Patterns1, Guards, Expressions]);
           ([Patterns, Expressions]) ->
                Patterns1 = with_scope_type(PatternType, Patterns),
                with_scope_type(ScopeType, [Patterns1, Expressions])
        end,
    astranaut_uniplate:with_subtrees(Sequence).

%% Function Name in named fun should also be pattern and whole scope is shadowed.
walk_named_fun() ->
    Sequence =
        fun([NameTree|RestTrees]) ->
                NameTree1 = with_function_clause_pattern(NameTree),
                with_shadowed([NameTree1|RestTrees])
        end,
    astranaut_uniplate:with_subtrees(Sequence).

walk_clause_parent_expression() ->
    Sequence = fun with_scope_group/1,
    astranaut_uniplate:with_subtrees(Sequence).

%% hello(A = 1, A1 = 2)
%% [A = 1|[A1 = 2]]
%% {A = 1, A1 = 2}
%% #hello{world1 = (A = 1), world2 = (A1 = 2)}
%% #{world1 => (A = 1), world2 => (A1 = 2)}
%% (A = 1) + (A1 = 2)
%% these expression has it's scope group
%% for this scope, we name it argument scope
%% every variable binded in scope could not used in neighbour scope, but avaliable outside scope group
%% for example
%% 1> (A = A1) + (A1 = 2).
%% * 1: variable 'A1' is unbound
%% 1> (A = 1) + (A1 = A).
%% * 1: variable 'A' is unbound
%% 1> ((A = 1) + (A1 = 2)), A1.
%% 2
%% usually user dont write code this style, so variable rebinding only works in strict mode.
walk_scope_group_expression() ->
    Sequence = fun sequence_scope_group_with_argument/1,
    astranaut_uniplate:with_subtrees(Sequence).

sequence_scope_group_subtrees(Subtrees) ->
    with_scope_group(Subtrees).

sequence_scope_group_with_argument(Subtreess) ->
    sequence_scope_group_subtrees(lists:map(fun(Subtrees) -> lists:map(fun with_argument/1, Subtrees) end, Subtreess)).

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

with_argument(Trees) ->
    with_scope_type(argument, Trees).

with_shadowed(NodeM) ->
    with_scope_type(shadowed, NodeM).

with_match_left_pattern(NodeM) ->
    with_scope_type(match_left, NodeM).

with_function_clause_pattern(NodeM) ->
    with_scope_type(function_clause, NodeM).

with_comprehension_generate_pattern(NodeM) ->
    with_scope_type(comprehension_generate, NodeM).

with_scope_type(ScopeType, Trees) ->
    astranaut_uniplate:with(fun(Context) -> entry_scope_type(ScopeType, Context) end,
                            fun(Context) -> exit_scope_type(ScopeType, Context) end,
                            Trees).

rebind_var(Var,
           #{global_varnames   := GlobalVarnames,
             local_varnames    := LocalVarnames,
             global_renames    := GlobalRenameMap,
             local_renames     := LocalRenameMap,
             pattern_varnames  := PatternVarnames
            } = Context) ->
    Varname = erl_syntax:variable_name(Var),
    Pos = erl_syntax:get_pos(Var),
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
                    Var1 = {var, Pos, Varname1},
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

rename_var(Var, #{global_renames := Renames}) ->
    Varname = erl_syntax:variable_name(Var),
    Pos = erl_syntax:get_pos(Var),
    case maps:find(Varname, Renames) of
        {ok, Varname1} ->
            {var, Pos, Varname1};
        error ->
            Var
    end.

entry_scope_type(scope_group, Context) ->
    entry_scope_group(Context);
entry_scope_type(nonfun_clause, Context) ->
    entry_nonfun_clause(Context);
entry_scope_type(argument, Context) ->
    entry_argument(Context);
entry_scope_type(shadowed, Context) ->
    entry_shadowed(Context);
entry_scope_type(ScopeType, Context) ->
    entry_pattern(ScopeType, Context).

exit_scope_type(scope_group, Context) ->
    exit_scope_group(Context);
exit_scope_type(nonfun_clause, Context) ->
    exit_nonfun_clause(Context);
exit_scope_type(argument, Context) ->
    exit_argument(Context);
exit_scope_type(shadowed, Context) ->
    exit_shadowed(Context);
exit_scope_type(ScopeType, Context) ->
    exit_pattern(ScopeType, Context).

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

entry_argument(Context) ->
    push_rename_stack(Context).

exit_argument(#{local_varnames    := LocalVarnames,
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
    Context#{pattern => PatternType, pattern_varnames => ordsets:new()}.

exit_pattern(PatternType, #{pattern := PatternType} = Context) ->
    Context1 = maps:remove(pattern, Context),
    Context1#{pattern_varnames => ordsets:new()}.

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
