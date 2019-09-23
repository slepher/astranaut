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
    {BFunOptions, BAllOptions, Warnings} = 
        binding_options(Forms),
    %dbg:tracer(),
    %dbg:tpl(astranaut_rebinding, add_var, cx),
    %dbg:p(self(), [c]),
    FormsMonad = 
        astranaut_traverse_monad:then(
          astranaut_traverse_monad:warnings(Warnings),
          astranaut_traverse_monad:map_m(
            fun(Form) ->
                    walk_form(Form, BFunOptions, BAllOptions)
            end, Forms)),
    File = astranaut:file(Forms),
    Return = astranaut_traverse_monad:eval(FormsMonad, #{}),
    %% astranaut_traverse:map_traverse_return(
    %%  fun(Forms1) ->
    %%          io:format("~s~n", [astranaut:safe_to_string(Forms1)])
    %%  end, Return),
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
walk_form({function, LINE, Name, Arity, Clauses} = Function, BFunOptions, BAllOptions) ->
    case match_rebinding(Name, Arity, BFunOptions, BAllOptions) of
        {ok, BindingOpts} ->
            ClausesM = 
                astranaut_traverse_monad:map_m(
                  fun(Clause) ->
                          Return = walk_function_clause(Clause),
                          astranaut_traverse:fun_return_to_monad(Return, Clause, #{with_state => true})
                  end, Clauses),
            astranaut_traverse_monad:lift_m(
              fun(Clauses1) ->
                      Function1 = {function, LINE, Name, Arity, Clauses1},
                      case maps:get(debug, BindingOpts, false) of
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
walk_form(Form, _BidingAttributes, _BidingAllAttributes) ->
    astranaut_traverse_monad:return(Form).

binding_options(Forms) ->
    BFunAttrs = astranaut:attributes_with_line(rebinding_fun, Forms),
    BAllAttrs = astranaut:attributes_with_line(rebinding_all, Forms),
    case {BFunAttrs, BAllAttrs} of
        {[], []} ->
            {#{}, #{}, []};
        _ ->
            {BFunOptions, Warnings2} = binding_fun_options(BFunAttrs),
            {BAllOptions, Warnings1} = binding_all_options(BAllAttrs),
            {BFunOptions, BAllOptions, Warnings1 ++ Warnings2}
    end.

binding_all_options([]) ->
    {none, []};
binding_all_options([{Line, Options}|_T]) ->
    {Options1, Warnings1} = astranaut:validate_options(fun validate_option_key/2, Options),
    Warnings2 = 
        lists:map(
          fun(Warning) ->
                  {Line, astranaut_macro, Warning}
          end, Warnings1),
    {Options1, Warnings2}.

binding_fun_options(BindingAttributes) ->
    lists:foldl(
      fun({Line, FunName}, Acc) when is_atom(FunName) ->
              add_binding_options(FunName, #{}, Line, Acc);
         ({Line, {FunName, Arity}}, Acc) when is_atom(FunName), is_integer(Arity) ->
              add_binding_options({FunName, Arity}, #{}, Line, Acc);
         ({Line, {FunName, Opts}}, Acc) ->
              add_binding_options(FunName, Opts, Line, Acc)
      end, {maps:new(), []}, BindingAttributes).

add_binding_options(Funs, Options, Line, {Acc, Warnings}) when is_list(Funs) ->
    lists:foldl(
      fun(Fun, {Acc1, Warnings1}) ->
              add_binding_options(Fun, Options, Line, {Acc1, Warnings1})
      end, {Acc, Warnings}, Funs);

add_binding_options(Fun, Options, Line, {Acc, Warnings}) ->
    {Options1, Warnings1} = astranaut:validate_options(fun validate_option_key/2, Options),
    Warnings2 = 
        lists:map(
          fun(Warning) ->
                  {Line, astranaut_rebinding, Warning}
          end, Warnings1),
    {maps:put(Fun, Options1, Acc), Warnings ++ Warnings2}.

validate_option_key(debug, true) ->
    ok;
validate_option_key(debug, false) ->
    ok;
validate_option_key(debug, _Debug) ->
    error;
validate_option_key(_Key, _Value) ->
    error.

match_rebinding(Name, Arity, BFunOptions, BAllOptions) ->
    case maps:find(Name, BFunOptions) of
        {ok, Options} ->
            {ok, Options};
        error ->
            case maps:find({Name, Arity}, BFunOptions) of
                {ok, Options} ->
                    {ok, Options};
                error ->
                    case BAllOptions of
                        none ->
                            error;
                        #{} ->
                            {ok, BAllOptions}
                    end
            end
    end.

walk_function_clause(Clause) ->
    Context0 = #{global_varnames => ordsets:new(), 
                 local_varnames => ordsets:new(),
                 global_rename_map => maps:new(),
                 local_rename_map => maps:new(), 
                 scope_varnames_stack => [], 
                 scope_rename_map_stack => [],
                 clause_stack => [{fun_expr, ordsets:new()}]},
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
            ClauseStack1 = [{ClauseParentType, ordsets:new()}|ClauseStack],
            Context1 = Context#{clause_stack => ClauseStack1},
            walk_node_1(Node, Context1, Attr)
    end;
walk_node(Node, #{clause_stack := [{BlockType, BlockVarnames}|ClauseStack]} = Context,
          #{step := post} = Attr) ->
    NodeType = erl_syntax:type(Node),
    case clause_parent_type(NodeType) of
        BlockType ->
            Context1 = Context#{clause_stack => ClauseStack},
            Context2 = exit_block(BlockType, BlockVarnames, Context1),
            %% io:format("exit block ~p ~p ~p ~n", [BlockType, Context1, Context2]),
            walk_node_1(Node, Context2, Attr);
        _ ->
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
    match_expr;
clause_parent_type(_Type) ->
    none.

walk_node_1({ComprehensionType, _Line, _Expression, _Qualifiers} = Node, #{} = Context, #{step := pre} = Attr) 
  when (ComprehensionType == lc) or (ComprehensionType == bc) ->
    NodeM = walk_comprehension(Node), 
    {Node1, Context1} = 
        astranaut_traverse:monad_to_traverse_fun_return(NodeM, #{init => Context, with_state => true}),
    {Node2, Context2} = walk_node(Node1, Context1, Attr#{step => post}),
    astranaut_traverse:traverse_fun_return(#{node => Node2, state => Context2, continue => true});
walk_node_1({'match', Line, Patterns, Expressions}, 
          #{} = Context, 
          #{step := pre, node := expression} = Attr) ->
    Opts = #{node => expression, traverse => all, match_right_first => true},
    F = astranaut_traverse:transform_mapfold_f(fun walk_node/3, Opts),
    NodeM =
        astranaut_traverse_monad:bind(
          astranaut_traverse:map_m(F, Expressions, Opts#{node => expression}),
          fun(NExpressions) ->
                  astranaut_traverse_monad:then(
                    astranaut_traverse_monad:modify(fun entry_pattern_scope/1),
                    astranaut_traverse_monad:bind(
                      astranaut_traverse:map_m(F, Patterns, Opts#{node => pattern}),
                      fun(NPatterns) ->
                              astranaut_traverse_monad:return({'match', Line, NPatterns, NExpressions})
                      end))
          end),
    {Node1, Context1} = 
        astranaut_traverse:monad_to_traverse_fun_return(NodeM, #{init => Context, with_state => true}),
    {Node2, Context2} = walk_node(Node1, Context1, Attr#{step => post}),
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
          #{clause_stack := [{fun_expr, _}|_T]} = Context, 
          #{step := pre}) ->
    Context1 = entry_function_scope(Context),
    %% io:format("entry function scope ~p~n ~p~n", [Context, Context1]),
    {Node, Context1};
walk_node_1({clause, _Line, _Patterns, _Match, _Body} = Node, 
          #{clause_stack := [{fun_expr, _}|_T]} = Context, 
          #{step := post}) ->
    Context1 = exit_function_scope(Context),
    %% io:format("exit function scope ~p~n ~p~n", [Context, Context1]),
    {Node, Context1};
walk_node_1({clause, _Line, _Patterns, _Match, _Body} = Node, 
          #{} = Context, 
          #{step := pre}) ->
    Context1 = entry_scope(Context),
    %% io:format("entry scope ~p~n ~p~n", [Context, Context1]),
    {Node, Context1};
walk_node_1({clause, _Line, _Patterns, _Match, _Body} = Node, 
          #{} = Context, 
          #{step := post}) ->
    Context1 = exit_scope(Context),
    %% io:format("exit scope ~p~n ~p~n", [Context, Context1]),
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
          #{clause_stack := [{fun_expr, _}|_T]} = Context, #{node := pattern}) ->
    add_var(Var, Context);
%% rebind var if current node is match pattern.
walk_node_1({var, _Line, _Varname} = Var, 
            #{clause_stack := [{match_expr, _}|_T]} = Context, #{node := pattern}) ->
    {Var1, Context1} = add_var(Var, Context),
    %% io:format("var ~p ~p~n context 1 ~p~n context 2 ~p~n", [Var, Var1, Context, Context1]),
    {Var1, Context1};
walk_node_1({var, _Line, _Varname} = Var, 
            #{clause_stack := [{lc_expr, _}|_T]} = Context, #{node := pattern}) ->
    {Var1, Context1} = add_var(Var, Context),
    %% io:format("lc ~p ~p~n context 1 ~p~n context 2 ~p~n", [Var, Var1, Context, Context1]),
    {Var1, Context1};

walk_node_1({var, _Line, _Varname} = Var, #{} = Context, #{}) ->
    {rename_var(Var, Context), Context};
walk_node_1(Node, Acc, #{}) ->
    {Node, Acc}.

walk_comprehension({ComprehensionType, Line, Expression, Qualifiers}) ->
    Opts = #{node => expression, traverse => all},
    F = astranaut_traverse:transform_mapfold_f(fun walk_node/3, Opts),
    astranaut_traverse_monad:then(
      astranaut_traverse_monad:modify(
        fun(#{clause_stack := ClauseStack} = Context) ->
                ClauseStack1 = [{lc_expr, ordsets:new()}|ClauseStack],
                entry_function_scope(Context#{clause_stack => ClauseStack1})
        end),
      astranaut_traverse_monad:bind(
        map_m_qualifiers(F, Qualifiers, Opts),
        fun(Qualifiers1) ->
                astranaut_traverse_monad:bind(
                  astranaut_traverse:map_m(F, Expression, Opts#{node => expression}),
                  fun(Expression1) ->
                          astranaut_traverse_monad:then(
                            astranaut_traverse_monad:modify(
                              fun(#{clause_stack := [{lc_expr, _}|ClauseStack]} = Context) ->
                                      exit_function_scope(Context#{clause_stack => ClauseStack})
                              end),
                            astranaut_traverse_monad:return(
                              {ComprehensionType, Line, Expression1, Qualifiers1}))
                  end)
        end)).

map_m_qualifiers(F, Qualifiers, Opts) ->
    map_m_qualifiers(F, Qualifiers, Opts, []).

map_m_qualifiers(F, [{GenerateType, Line, Pattern, Expression}|T], Opts, Acc)
  when (GenerateType == generate) or (GenerateType == b_generate) ->
    astranaut_traverse_monad:bind(
      astranaut_traverse:map_m(F, Expression, Opts),
      fun(Expression1) ->
              Generate1 = {GenerateType, Line, Pattern, Expression1},
              map_m_qualifiers(F, T, Opts, [Generate1|Acc])
      end);
map_m_qualifiers(F, [Expression|T], Opts, Acc) ->
    astranaut_traverse_monad:bind(
      astranaut_traverse:map_m(F, Expression, Opts),
      fun(Expression1) ->
              map_m_qualifiers(F, T, Opts, [Expression1|Acc])
      end);
map_m_qualifiers(F, [], Opts, Acc) ->
    astranaut_traverse_monad:then(
      astranaut_traverse_monad:modify(fun entry_pattern_scope/1),
      map_m_qualifiers_1(F, lists:reverse(Acc), Opts, [])).

map_m_qualifiers_1(F, [{GenerateType, Line, Pattern, Expression}|T], Opts, Acc)
  when (GenerateType == generate) or (GenerateType == b_generate) ->
    astranaut_traverse_monad:bind(
      astranaut_traverse:map_m(F, Pattern, Opts#{node => pattern}),
      fun(Pattern1) ->
              Generate1 = {GenerateType, Line, Pattern1, Expression},
              map_m_qualifiers_1(F, T, Opts, [Generate1|Acc])
      end);
map_m_qualifiers_1(F, [Expression|T], Opts, Acc) ->
    map_m_qualifiers_1(F, T, Opts, [Expression|Acc]);
map_m_qualifiers_1(_F, [], _Opts, Acc) ->
    astranaut_traverse_monad:return(lists:reverse(Acc)).

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

exit_block(_BlockType, BlockVarnames, 
           #{global_varnames := GlobalVarnames, 
             local_varnames := LocalVarnames} = Context) ->
    GlobalVarnames1 = ordsets:union(BlockVarnames, GlobalVarnames),
    LocalVarnames1 = ordsets:union(BlockVarnames, LocalVarnames),
        Context#{global_varnames => GlobalVarnames1, 
                 local_varnames => LocalVarnames1}.

entry_function_scope(Context) ->
    Context1 = entry_varname_scope(Context),
    Context2 = entry_rename_map_scope(Context1),
    entry_pattern_scope(Context2).

exit_function_scope(Context) ->
    Context1 = exit_varname_scope(Context),
    exit_rename_map_scope(Context1).

entry_scope(Context) ->
    Context1 = entry_rename_map_scope(Context),
    Context2 = entry_varname_scope(Context1),
    entry_pattern_scope(Context2).

exit_scope(#{local_varnames := LocalVarnames, clause_stack := [{BlockType, BlockVarnames}|T]} = Context) ->
    BlockVarnames1 = ordsets:union(LocalVarnames, BlockVarnames),
    ClauseStack1 = [{BlockType, BlockVarnames1}|T],
    Context1 = Context#{clause_stack => ClauseStack1},
    Context2 = exit_varname_scope(Context1),
    exit_rename_map_scope(Context2).

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
