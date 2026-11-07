%% Standard Erlang lexical scopes with caller-defined variable actions.
-module(astranaut_vars).
-include("astranaut_struct_name.hrl").
-export([
    analyze/1, analyze/2,
    rewrite/2,
    map_m/2, map_m/3,
    mapfold_m/2, mapfold_m/3,
    all_vars/1,
    fresh_var/2, fresh_var/3,
    fresh_var_asts/4,
    alloc_var_asts/3
]).

-export_type([
    var_name/0,
    orig_var/0,
    final_var/0,
    var_pair/0,
    var_metric/0,
    scope_view/0,
    var_action/0,
    transform_var_fun/0,
    scope_opts/0,
    analysis_result/0,
    transform_var_return/1,
    scope_walk/1
]).

-type var_name() :: atom().
%% 重命名前的变量名（源码 AST 中的名字，如 'A'）
-type orig_var() :: var_name().
%% 重命名后的变量名（改写后 AST 中的名字，如 'A_1'；未改名则等于 OrigVar）
-type final_var() :: var_name().
-type var_pair() :: {orig_var(), final_var()}.

-type var_metric() ::
    %% 命名空间内出现的所有变量名
    all
    %% 处于绑定位置的变量
    | bound
    %% 处于读取引用位置的变量
    | used
    %% 在引用点之前尚未在当前局部作用域绑定的自由变量
    | free
    %% 按 Erlang 作用域规则向后续同级语句导出的绑定变量
    | exported
    %% 每个变量的被引用（读取/二次匹配）次数 #{final_var() => non_neg_integer()}
    | counts
    %% 当前生效的重命名映射 #{orig_var() => final_var()}
    | rename_map
    %% 每个原变量名对应的历次绑定重命名列表 #{orig_var() => [final_var()]}
    | rename_history
    | bindings.

%% 回调可见的当前作用域快照
-type scope_view() :: #{
    role := astranaut_syntax:node_role() | binary_size,
    pattern_kind := function_clause | clause_match | match_left | comprehension_generate | none,
    binding_id := pos_integer(),
    event := bind | use | node,
    is_bound => boolean(),
    current_var => final_var(),
    %% 是否在同一个 Pattern 内重复出现（如 {A, A}）
    same_pattern := boolean(),
    %% 当前正在展开的外层变量集合（防同名自引用死循环）
    expanding := ordsets:ordset(orig_var()),
    %% 当前命名空间已占用的所有变量名（global_varnames）
    allocated := ordsets:ordset(final_var()),
    %% 当前求值点可见的 [重命名前 => 重命名后] 映射表（global_renames）
    active_map := #{orig_var() => final_var()}
}.

%% 变量节点的变换动作：
%% - {rename, FinalVar} 或直接返回 FinalVar :: atom()：
%%   将当前变量节点改写为 {var, Pos, FinalVar}，并在状态机中记录 OrigVar -> FinalVar 及变量集合统计
%%   直接返回原变量名等同 keep；显式 {rename, FinalVar} 不受此简写规则影响
%% - {expand, NewAst} 或直接返回 NewAst :: astranaut:tree()：
%%   将当前 {var, Pos, OrigVar} 展开为 NewAst，且【不 skip】，立即通过 walk_node 让 NewAst 继续向下参与 vars 分析与重命名
%% - keep：
%%   保持引擎默认行为（bind 时不改名，use 时跟随 active_map 中已有的重命名映射）
-type var_action() ::
    {bind, final_var()}
    | {bind, astranaut:tree(), final_var()}
    | {use, astranaut:tree()}
    | {use, astranaut:tree(), final_var()}
    | {rename, final_var()}
    | {expand, astranaut:tree()}
    | keep
    | final_var()
    | astranaut:tree().

-type transform_var_fun() ::
    fun((astranaut:tree(), scope_view()) -> transform_var_return(term())).

%% Callbacks run before children and return actions, diagnostics or traverse
%% computations. Synchronous wrappers require a diagnostic-free result.
-type transform_var_return(S) ::
    var_action()
    | astranaut:common_walk_return(S, var_action())
    | astranaut_traverse:struct(S, var_action()).
-type scope_walk(S) ::
    fun((astranaut:tree(), scope_view()) -> transform_var_return(S))
    | fun((astranaut:tree(), S, scope_view()) -> transform_var_return(S)).

-type scope_opts() :: #{
    traverse_opts => map(),
    %% 需要一次性搜集的指标（默认 all）
    collect => [var_metric()] | all,
    %% bound/used/exported 的元素形态（默认 final，pair 为 {OrigVar, FinalVar}）
    collect_as => orig | final | pair,
    %% 变量重命名或展开回调
    transform =>
        transform_var_fun(),
    %% Standard Erlang isolates sibling arguments; sequential is an explicit dialect override.
    argument_scope => isolated | sequential,
    %% 初始已占用变量集合
    init_allocated => ordsets:ordset(final_var()),
    %% 初始重命名映射
    init_renames => #{orig_var() => final_var()}
}.

-type analysis_result() :: #{
    all => ordsets:ordset(final_var()),
    bound => ordsets:ordset(var_name() | var_pair()),
    used => ordsets:ordset(var_name() | var_pair()),
    free => ordsets:ordset(orig_var()),
    exported => ordsets:ordset(var_name() | var_pair()),
    counts => #{final_var() => non_neg_integer()},
    rename_map => #{orig_var() => final_var()},
    rename_history => #{orig_var() => [final_var()]},
    bindings => #{
        pos_integer() => #{orig := orig_var(), final := final_var(), count := non_neg_integer()}
    }
}.

-type bind_id() :: pos_integer().
-type binding_env() :: #{orig_var() => ordsets:ordset(bind_id())}.
-type var_base() :: atom() | string().
-type var_scope() :: ordsets:ordset(var_name()) | astranaut:trees().
-record(bind_entry, {
    id :: bind_id(),
    orig :: orig_var(),
    final :: final_var(),
    count = 0 :: non_neg_integer(),
    deferred = false :: boolean()
}).
-record(scope_frame, {kind, allocated, renames, env, pattern, pattern_vars}).
-record(group_frame, {saved_env, allocated = [], renames = #{}, branch_envs = none}).
-record(scope_state, {
    opts = #{},
    collect_keys = [],
    count_bindings = false,
    collect_exported = false,
    allocated = [],
    renames = #{},
    env = #{} :: binding_env(),
    pattern = none,
    pattern_vars = [],
    frames = [],
    groups = [],
    bindings = #{},
    events = [],
    annotated = false,
    annotation_prefix = [],
    next_id = 1,
    metrics = #{}
}).

-spec analyze(astranaut:trees()) -> analysis_result().
analyze(Nodes) -> analyze(Nodes, #{}).
-spec analyze(astranaut:trees(), scope_opts() | [var_metric()]) -> analysis_result().
analyze(Nodes, Keys) when is_list(Keys) -> analyze(Nodes, #{collect => Keys});
analyze(Nodes, Opts) ->
    {_, Analysis} = rewrite(Nodes, Opts),
    Analysis.

-spec rewrite(astranaut:trees(), scope_opts()) -> {astranaut:trees(), analysis_result()}.
rewrite(Nodes, Opts) ->
    astranaut_return:simplify(astranaut_traverse:eval(mapfold_m(Nodes, Opts), ?MODULE, #{}, ok)).

traversal_options(Nodes, Opts, Defaults) ->
    Extra = maps:with(
        [role, validator, attr, normalize, validate, validate_opts, uniplate],
        maps:get(traverse_opts, Opts, #{})
    ),
    Root =
        case maps:is_key(role, Extra) orelse maps:is_key(validator, Extra) of
            true -> #{};
            false -> #{role => root_role(Nodes)}
        end,
    maps:merge(maps:merge(Defaults#{attr => #{parent => fun_expr}}, Root), Extra).

%% The caller state and lexical state are independent. Scope entry/exit hooks
%% change only the latter; callback diagnostics stay on the enclosing pipeline.
-spec map_m(astranaut:trees(), scope_opts()) ->
    astranaut_traverse:struct(term(), astranaut:trees()).
map_m(Nodes, Opts) ->
    astranaut_traverse:lift_m(fun({New, _}) -> New end, mapfold_m(Nodes, Opts)).
-spec map_m(scope_walk(term()), astranaut:trees(), scope_opts()) ->
    astranaut_traverse:struct(term(), astranaut:trees()).
map_m(WalkFun, Nodes, Opts) ->
    astranaut_traverse:lift_m(fun({New, _}) -> New end, mapfold_m(WalkFun, Nodes, Opts)).
-spec mapfold_m(astranaut:trees(), scope_opts()) ->
    astranaut_traverse:struct(term(), {astranaut:trees(), analysis_result()}).
mapfold_m(Nodes, Opts) -> mapfold_m(none, Nodes, Opts).
-spec mapfold_m(scope_walk(term()) | none, astranaut:trees(), scope_opts()) ->
    astranaut_traverse:struct(term(), {astranaut:trees(), analysis_result()}).
mapfold_m(WalkFun, Nodes, Opts) ->
    astranaut_traverse:lift_m(
        fun({New, State}) -> {resolve_bindings(New, State), result(State)} end,
        mapfold_state_m(WalkFun, Nodes, Opts)
    ).

%% All public traversals and binding-site analysis share this one driver.
mapfold_state_m(WalkFun0, Nodes, Opts) ->
    WalkFun = normalize_callback(WalkFun0, Opts),
    astranaut_traverse:bind(astranaut_traverse:get(), fun(Caller) ->
        Walk = astranaut:map_m(
            fun(Node) ->
                astranaut_traverse:bind(astranaut_traverse:ask(), fun(Attr) ->
                    astranaut_traverse:bind(astranaut_traverse:get(), fun({_Caller, State}) ->
                        astranaut_traverse:bind(walk(Node, State, Attr, WalkFun), fun(
                            {New, State1}
                        ) ->
                            astranaut_traverse:then(
                                astranaut_traverse:modify(fun({Caller1, _}) -> {Caller1, State1} end),
                                astranaut_traverse:return(New)
                            )
                        end)
                    end)
                end)
            end,
            Nodes,
            traversal_options(Nodes, Opts, #{traverse => pre, normalize => true})
        ),
        astranaut_traverse:bind(
            astranaut_traverse:scoped_state_run({Caller, context(Opts)}, Walk),
            fun({New, {CallerFinal, StateFinal}}) ->
                astranaut_traverse:then(
                    astranaut_traverse:put(CallerFinal),
                    astranaut_traverse:return({New, StateFinal})
                )
            end
        )
    end).

caller_callback(Default, F) ->
    astranaut_traverse:bind(astranaut_traverse:get(), fun({Caller, _Scope}) ->
        astranaut_traverse:bind(
            astranaut_traverse:scoped_state_run(Caller, callback_return(Default, F(Caller))),
            fun({Value, Caller1}) ->
                astranaut_traverse:then(
                    astranaut_traverse:modify(fun({_, Scope1}) -> {Caller1, Scope1} end),
                    astranaut_traverse:return(Value)
                )
            end
        )
    end).

%% with_return/3 is private to astranaut and rejects traverse monads. Use its
%% public walk_return/traverse_return adapters, supplying diagnostic-only defaults.
callback_return(_Default, #{?STRUCT_KEY := Kind} = M) when
    Kind =:= ?TRAVERSE_M; Kind =:= ?RETURN_OK; Kind =:= ?RETURN_FAIL
->
    astranaut:traverse_return(M);
callback_return(Default, Value) ->
    Return = astranaut:walk_return(Value),
    astranaut:traverse_return(maps:merge(#{return => Default}, Return)).

done(Node, C) -> astranaut_traverse:return({Node, C}).

scope_view(C, Attr) ->
    Role =
        case maps:get(validator, Attr, none) of
            {slot, size_qualifier, size, binary_size} -> binary_size;
            _ -> maps:get(node, Attr, expression)
        end,
    #{
        role => Role,
        pattern_kind => C#scope_state.pattern,
        binding_id => C#scope_state.next_id,
        event => node,
        same_pattern => false,
        expanding => maps:get(expanding, Attr, []),
        allocated => C#scope_state.allocated,
        active_map => C#scope_state.renames
    }.

root_role([Node | _]) ->
    root_role(Node);
root_role([]) ->
    expression;
root_role(Node) ->
    case astranaut_syntax:type(Node) of
        clause -> clause;
        function -> form;
        attribute -> form;
        eof_marker -> form;
        _ -> expression
    end.

context(Opts) ->
    Keys = keys(Opts),
    Renames = maps:get(init_renames, Opts, #{}),
    #scope_state{
        opts = Opts,
        annotation_prefix = "$astranaut_binding:" ++ erlang:ref_to_list(make_ref()) ++ ":",
        collect_keys = Keys,
        count_bindings = lists:member(bindings, Keys),
        collect_exported = lists:member(exported, Keys),
        allocated = maps:get(init_allocated, Opts, []),
        renames = Renames,
        env = maps:map(fun(_, _) -> [] end, Renames),
        metrics = maps:with(Keys, #{
            all => [],
            bound => [],
            used => [],
            free => [],
            counts => #{},
            rename_history => #{}
        })
    }.

keys(Opts) ->
    case maps:get(collect, Opts, all) of
        all -> [all, bound, used, free, exported, counts, rename_map, rename_history, bindings];
        Keys -> Keys
    end.

result(#scope_state{
    opts = Opts,
    collect_keys = Keys,
    collect_exported = CollectExported,
    metrics = EmptyMetrics,
    events = Events,
    renames = Renames,
    env = Env,
    bindings = Bindings
}) ->
    Metrics0 = lists:foldl(
        fun(Event, Acc) -> event_metrics(Event, Bindings, Opts, Acc) end,
        EmptyMetrics,
        lists:reverse(Events)
    ),
    Metrics =
        case maps:find(rename_history, Metrics0) of
            {ok, History} ->
                Metrics0#{
                    rename_history => maps:map(fun(_, Names) -> lists:reverse(Names) end, History)
                };
            error ->
                Metrics0
        end,
    Exported =
        case CollectExported of
            false ->
                [];
            true ->
                ordsets:from_list([
                    collected(Orig, Final, Opts)
                 || Ids <- maps:values(Env),
                    Id <- Ids,
                    #bind_entry{orig = Orig, final = Final} <- [maps:get(Id, Bindings)]
                ])
        end,
    FinalRenames = maps:fold(
        fun(Orig, Ids, Acc) ->
            case ordsets:from_list([(maps:get(Id, Bindings))#bind_entry.final || Id <- Ids]) of
                [] -> Acc;
                [Name] -> maps:put(Orig, Name, Acc);
                _ -> maps:remove(Orig, Acc)
            end
        end,
        Renames,
        Env
    ),
    Base = Metrics#{rename_map => FinalRenames, exported => Exported},
    case lists:member(bindings, Keys) of
        true ->
            Sites = maps:map(
                fun(_, #bind_entry{orig = O, final = F, count = N}) ->
                    #{orig => O, final => F, count => N}
                end,
                Bindings
            ),
            maps:with(Keys, Base#{bindings => Sites});
        false ->
            maps:with(Keys, Base)
    end.

all_vars(Nodes) ->
    astranaut:sreduce(
        fun
            ({var, _, '_'}, Acc) -> Acc;
            ({var, _, V}, Acc) -> ordsets:add_element(V, Acc);
            (_, Acc) -> Acc
        end,
        [],
        Nodes,
        #{traverse => pre}
    ).
-spec fresh_var(var_base(), var_scope()) -> var_name().
fresh_var(Base, Scope) -> fresh_var(Base, Scope, #{}).
-spec fresh_var(var_base(), var_scope(), map()) -> var_name().
fresh_var(Base0, Scope0, Opts) ->
    Base = base_name(Base0),
    Scope = scope_vars(Scope0),
    case maps:get(always_number, Opts, false) orelse ordsets:is_element(Base, Scope) of
        false -> Base;
        true -> fresh_number(Base, Scope, maps:get(sep, Opts, "@"), 1)
    end.
fresh_number(Base, Scope, Sep, N) ->
    Var = list_to_atom(atom_to_list(Base) ++ Sep ++ integer_to_list(N)),
    case ordsets:is_element(Var, Scope) of
        true -> fresh_number(Base, Scope, Sep, N + 1);
        false -> Var
    end.
%% AST inputs are scanned once per batch; repeated bases share allocations.
fresh_vars(Bases, Scope, Opts) when is_list(Bases), is_map(Opts) ->
    {Names, _} = lists:mapfoldl(
        fun(Base, Allocated) ->
            V = fresh_var(Base, Allocated, Opts),
            {V, ordsets:add_element(V, Allocated)}
        end,
        scope_vars(Scope),
        Bases
    ),
    Names.

base_name(Name) when is_atom(Name) -> Name;
base_name(Name) when is_list(Name) -> list_to_atom(Name).
scope_vars([]) -> [];
scope_vars([Name | _] = Names) when is_atom(Name) -> ordsets:from_list(Names);
scope_vars(Nodes) -> all_vars(Nodes).

walk(Node, Context, Attr0, WalkFun) ->
    Attr = Attr0#{argument_scope => maps:get(argument_scope, Context#scope_state.opts, isolated)},
    Scope = node_scope(Node, Context, Attr),
    Callback = fun(Caller) -> WalkFun(Node, Caller, Scope) end,
    astranaut_traverse:bind(caller_callback(keep, Callback), fun(Action) ->
        apply_action(Action, Node, Scope, Context, Attr, WalkFun)
    end).

%% Normalize once; the traversal always invokes a three-argument callback.
normalize_callback(none, Opts) ->
    F = maps:get(transform, Opts, fun(_, _) -> keep end),
    fun
        ({var, _, Orig} = Node, _, Scope) when Orig =/= '_' ->
            case ordsets:is_element(Orig, maps:get(expanding, Scope)) of
                true -> keep;
                false -> F(Node, Scope)
            end;
        (Node, _, Scope) ->
            F(Node, Scope)
    end;
normalize_callback(F, _) when is_function(F, 2) ->
    fun(Node, _, Scope) -> F(Node, Scope) end;
normalize_callback(F, _) when is_function(F, 3) ->
    F.

node_scope({var, _, Orig}, Context, Attr) when Orig =/= '_' ->
    Base = scope_view(Context, Attr),
    Role = maps:get(role, Base),
    Kind = Context#scope_state.pattern,
    Same = Role =:= pattern andalso ordsets:is_element(Orig, Context#scope_state.pattern_vars),
    Bound = maps:is_key(Orig, Context#scope_state.env),
    ExistingMatch = Bound andalso (Kind =:= match_left orelse Kind =:= clause_match),
    Event =
        case Role =:= pattern andalso not (Same orelse ExistingMatch) of
            true -> bind;
            false -> use
        end,
    Current =
        case Event of
            bind -> Orig;
            use -> current_name(Orig, Context)
        end,
    Active =
        case Event =:= use andalso Bound of
            true -> maps:put(Orig, Current, maps:get(active_map, Base));
            false -> maps:get(active_map, Base)
        end,
    Base#{
        event => Event,
        is_bound => Bound,
        same_pattern => Same,
        current_var => Current,
        active_map => Active
    };
node_scope(_, Context, Attr) ->
    scope_view(Context, Attr).

apply_action(keep, {var, _, '_'} = Node, _, Context, _, _) ->
    done(Node, Context);
apply_action(keep, {var, _, _} = Node, Scope, Context, _, _) ->
    variable_result(maps:get(event, Scope), Node, maps:get(current_var, Scope), Context);
apply_action(keep, Node, _, Context, Attr, _) ->
    walk_node(astranaut_syntax:type(Node), Node, Context, Attr);
apply_action({bind, Final}, {var, _, _} = Node, _, Context, _, _) ->
    variable_result(bind, Node, Final, Context);
apply_action({bind, Var, Final}, _, _, Context, _, _) ->
    variable_result(bind, Var, Final, Context);
apply_action({use, {var, _, Orig} = Var}, _, _, Context, _, _) ->
    variable_result(use, Var, current_name(Orig, Context), Context);
apply_action({use, Var, Final}, _, _, Context, _, _) ->
    variable_result(use, Var, Final, Context);
apply_action({rename, Final}, {var, _, _} = Node, Scope, Context, _, _) ->
    variable_result(maps:get(event, Scope), Node, Final, Context);
apply_action({expand, New}, Node, _, Context, Attr, WalkFun) ->
    Expanding = maps:get(expanding, Attr, []),
    case Node of
        {var, _, Orig} ->
            case ordsets:is_element(Orig, Expanding) of
                true ->
                    apply_action(
                        keep, Node, node_scope(Node, Context, Attr), Context, Attr, WalkFun
                    );
                false ->
                    expand(
                        New,
                        Context,
                        Attr#{expanding => ordsets:add_element(Orig, Expanding)},
                        WalkFun
                    )
            end;
        _ ->
            expand(New, Context, Attr, WalkFun)
    end;
apply_action(Node, Node, Scope, Context, Attr, WalkFun) ->
    apply_action(keep, Node, Scope, Context, Attr, WalkFun);
apply_action(Orig, {var, _, Orig} = Node, Scope, Context, Attr, WalkFun) ->
    apply_action(keep, Node, Scope, Context, Attr, WalkFun);
apply_action(Final, {var, _, _} = Node, Scope, Context, Attr, WalkFun) when is_atom(Final) ->
    apply_action({rename, Final}, Node, Scope, Context, Attr, WalkFun);
apply_action(New, Node, Scope, Context, Attr, WalkFun) ->
    apply_action({expand, New}, Node, Scope, Context, Attr, WalkFun).

variable_result(_, {var, _, '_'} = Var, _, Context) ->
    done(Var, Context);
variable_result(Event, {var, Pos, Orig}, Final, Context) ->
    Context1 = use_joined_binding(Event, Orig, Final, Context),
    Ref = binding_ref(Event, Orig, Final, Context1),
    Context2 = record_var(Event, Orig, Final, Ref, Context1),
    %% Keep binding identity until branch joins can coordinate their final names.
    case Ref of
        {binding, Id} ->
            case (maps:get(Id, Context2#scope_state.bindings))#bind_entry.deferred of
                true ->
                    Text =
                        Context2#scope_state.annotation_prefix ++
                            base64:encode_to_string(term_to_binary({Id, Pos})),
                    Pos1 = erl_anno:set_text(Text, Pos),
                    done({var, Pos1, Final}, Context2#scope_state{annotated = true});
                false ->
                    done({var, Pos, Final}, Context2)
            end;
        {name, _} ->
            done({var, Pos, Final}, Context2)
    end.

%% Unused branch bindings may be renamed independently (for warning suppression).
use_joined_binding(
    use, Orig, Final, #scope_state{env = Env, bindings = Table, renames = Renames} = C
) ->
    case maps:is_key(Orig, Env) andalso Final =:= current_name(Orig, C) of
        true ->
            Table1 = lists:foldl(
                fun(Id, T) ->
                    Entry = maps:get(Id, T),
                    maps:put(Id, Entry#bind_entry{final = Final}, T)
                end,
                Table,
                maps:get(Orig, Env, [])
            ),
            C#scope_state{
                bindings = Table1,
                renames = maps:put(Orig, Final, Renames),
                allocated = ordsets:add_element(Final, C#scope_state.allocated)
            };
        false ->
            C
    end;
use_joined_binding(_, _, _, C) ->
    C.

binding_ref(bind, _, _, #scope_state{next_id = Id}) ->
    {binding, Id};
binding_ref(use, Orig, Final, #scope_state{env = Env, bindings = Table}) ->
    case maps:get(Orig, Env, []) of
        [Id | _] ->
            case (maps:get(Id, Table))#bind_entry.final of
                Final -> {binding, Id};
                _ -> {name, Final}
            end;
        [] ->
            {name, Final}
    end.

resolve_bindings(Nodes, #scope_state{annotated = false}) ->
    Nodes;
resolve_bindings(Nodes, #scope_state{bindings = Table, annotation_prefix = Prefix}) ->
    astranaut:smap(
        fun
            ({var, Pos, _} = Node) ->
                case erl_anno:text(Pos) of
                    Text when is_list(Text) ->
                        case lists:prefix(Prefix, Text) of
                            true ->
                                Encoded = lists:nthtail(length(Prefix), Text),
                                {Id, OriginalPos} = binary_to_term(base64:decode(Encoded), [safe]),
                                {var, OriginalPos, (maps:get(Id, Table))#bind_entry.final};
                            false ->
                                Node
                        end;
                    _ ->
                        Node
                end;
            (Node) ->
                Node
        end,
        Nodes,
        #{}
    ).

event_name({binding, Id}, Table) -> (maps:get(Id, Table))#bind_entry.final;
event_name({name, Name}, _) -> Name.

event_metrics({Event, Orig, Ref, Free}, Table, Opts, M) ->
    Final = event_name(Ref, Table),
    M1 = metric(all, fun(V) -> ordsets:add_element(Final, V) end, M),
    case Event of
        bind ->
            M2 = metric(
                bound, fun(V) -> ordsets:add_element(collected(Orig, Final, Opts), V) end, M1
            ),
            M3 = metric(counts, fun(V) -> maps:put(Final, maps:get(Final, V, 0), V) end, M2),
            metric(
                rename_history, fun(V) -> maps:put(Orig, [Final | maps:get(Orig, V, [])], V) end, M3
            );
        use ->
            M2 = metric(
                used, fun(V) -> ordsets:add_element(collected(Orig, Final, Opts), V) end, M1
            ),
            M3 = metric(counts, fun(V) -> maps:put(Final, maps:get(Final, V, 0) + 1, V) end, M2),
            case Free of
                true -> metric(free, fun(V) -> ordsets:add_element(Orig, V) end, M3);
                false -> M3
            end
    end.

expand(New, Context, Attr, WalkFun) ->
    astranaut_traverse:lift_m(
        fun({New1, Context1}) -> {astranaut_uniplate:up_attr(Attr, New1), Context1} end,
        walk(New, Context, Attr, WalkFun)
    ).

%% Unlike a map pattern lookup, a map generator binds both key and value.
walk_node(
    map_field_exact,
    Node,
    #scope_state{pattern = comprehension_generate} = Context,
    #{parent := Parent}
) when Parent =:= map_generator; Parent =:= strict_map_generator ->
    done(
        astranaut_uniplate:with_subtrees(
            fun(Subtrees) ->
                astranaut_uniplate:up_attr(
                    #{node => pattern, validator => {role, pattern}, parent => map_field_exact},
                    Subtrees
                )
            end,
            Node
        ),
        Context
    );
walk_node(Type, Node, Context, Attr) ->
    Node1 = walk_node_1(Type, Node, Attr),
    done(
        astranaut_uniplate:up_attr(
            #{parent => Type},
            astranaut_uniplate:with_subtrees(
                fun(Subtrees) ->
                    astranaut_syntax:subtrees_pge(Type, Subtrees, Attr)
                end,
                Node1
            )
        ),
        Context
    ).

current_name(Orig, #scope_state{
    renames = Renames,
    env = Env,
    bindings = Table,
    allocated = Allocated
}) ->
    case maps:find(Orig, Renames) of
        {ok, Name} ->
            Name;
        error ->
            case
                ordsets:from_list([
                    (maps:get(Id, Table))#bind_entry.final
                 || Id <- maps:get(Orig, Env, [])
                ])
            of
                [] ->
                    Orig;
                [Name] ->
                    Name;
                Names ->
                    Ids = maps:get(Orig, Env),
                    Reserved = ordsets:from_list([
                        Entry#bind_entry.final
                     || {Id, Entry} <- maps:to_list(Table), not lists:member(Id, Ids)
                    ]),
                    case [Name || Name <- Names, not ordsets:is_element(Name, Reserved)] of
                        [Name | _] -> Name;
                        [] -> fresh_var(Orig, Allocated)
                    end
            end
    end.

record_var(
    Event,
    Orig,
    Final,
    Ref,
    #scope_state{
        metrics = M,
        env = Env,
        bindings = Table,
        next_id = Id
    } = C
) ->
    Events =
        case map_size(M) of
            0 -> C#scope_state.events;
            _ -> [{Event, Orig, Ref, not maps:is_key(Orig, Env)} | C#scope_state.events]
        end,
    C0 = C#scope_state{events = Events},
    case Event of
        bind ->
            Deferred = lists:any(
                fun(#scope_frame{kind = Kind}) -> Kind =:= nonfun_clause end,
                C#scope_state.frames
            ),
            Entry = #bind_entry{id = Id, orig = Orig, final = Final, deferred = Deferred},
            C0#scope_state{
                next_id = Id + 1,
                bindings = maps:put(Id, Entry, Table),
                env = maps:put(Orig, [Id], Env),
                pattern_vars = ordsets:add_element(Orig, C#scope_state.pattern_vars),
                allocated = ordsets:add_element(Final, C#scope_state.allocated),
                renames = maps:put(Orig, Final, C#scope_state.renames)
            };
        use ->
            touch_bindings(maps:get(Orig, Env, []), C0)
    end.

touch_bindings(_, #scope_state{count_bindings = false} = C) ->
    C;
touch_bindings(Ids, #scope_state{bindings = Table} = C) ->
    Table1 = lists:foldl(
        fun(Id, Acc) ->
            E = maps:get(Id, Acc),
            maps:put(Id, E#bind_entry{count = E#bind_entry.count + 1}, Acc)
        end,
        Table,
        Ids
    ),
    C#scope_state{bindings = Table1}.

metric(Key, F, Metrics) ->
    case maps:find(Key, Metrics) of
        {ok, V} -> maps:put(Key, F(V), Metrics);
        error -> Metrics
    end.
collected(Orig, Final, Opts) ->
    case maps:get(collect_as, Opts, final) of
        orig -> Orig;
        final -> Final;
        pair -> {Orig, Final}
    end.

%% The right operand may never execute, so its bindings cannot escape.
walk_node_1(infix_expr, {op, _, Op, _, _} = Expr, #{node := expression}) when
    Op =:= 'andalso'; Op =:= 'orelse'
->
    astranaut_uniplate:with_subtrees(
        fun([Left, Operator, Right]) ->
            [Left, Operator, with_shadowed_leaky(Right)]
        end,
        Expr
    );
walk_node_1(infix_expr, Expr, #{node := expression, argument_scope := isolated}) ->
    walk_scope_group_expression(Expr);
%% walk function call
walk_node_1(application, Expr, #{node := expression, argument_scope := isolated}) ->
    walk_scope_group_expression(Expr);
walk_node_1(tuple, Expr, #{node := expression, argument_scope := isolated}) ->
    walk_scope_group_expression(Expr);
walk_node_1(list, Expr, #{node := expression, argument_scope := isolated}) ->
    walk_scope_group_expression(Expr);
walk_node_1(map_expr, Expr, #{node := expression, argument_scope := isolated}) ->
    walk_scope_group_expression(Expr);
walk_node_1(record_expr, Expr, #{node := expression, argument_scope := isolated}) ->
    walk_scope_group_expression(Expr);
%% walk comprehension
walk_node_1(list_comp, ListComp, #{}) ->
    walk_comprehension(ListComp);
walk_node_1(map_comp, MapComp, #{}) ->
    walk_comprehension(MapComp);
walk_node_1(binary_comp, BinaryComp, #{}) ->
    walk_binary_comprehension(BinaryComp);
%% walk comprehension generate
walk_node_1(generator, ListGenerator, #{}) ->
    walk_generator(ListGenerator);
walk_node_1(strict_generator, ListGenerator, #{}) ->
    walk_generator(ListGenerator);
walk_node_1(binary_generator, BinaryGenerator, #{}) ->
    walk_generator(BinaryGenerator);
walk_node_1(strict_binary_generator, BinaryGenerator, #{}) ->
    walk_generator(BinaryGenerator);
walk_node_1(map_generator, MapGenerator, #{}) ->
    walk_generator(MapGenerator);
walk_node_1(strict_map_generator, MapGenerator, #{}) ->
    walk_generator(MapGenerator);
%% walk match
walk_node_1(match_expr, Match, #{node := expression}) ->
    walk_match(Match);
walk_node_1(maybe_match_expr, MaybeMatch, #{node := expression}) ->
    walk_match(MaybeMatch);
%% walk function clause and other clauses
walk_node_1(clause, Clause, #{} = Attr) ->
    walk_clause(Clause, Attr);
%% walk named fun
walk_node_1(named_fun_expr, NamedFun, #{}) ->
    walk_named_fun(NamedFun);
walk_node_1(case_expr, Case, #{}) ->
    walk_clause_parent_expression(Case);
walk_node_1(if_expr, If, #{}) ->
    walk_clause_parent_expression(If);
walk_node_1(receive_expr, Receive, #{}) ->
    ClauseCount = length(erl_syntax:receive_expr_clauses(Receive)),
    astranaut_uniplate:with_subtrees(
        fun
            ([Clauses, Timeout, Body]) ->
                Branches = with_scope_group(Clauses ++ with_scope_type(nonfun_clause, Body)),
                with_scope_group([with_argument(Timeout), with_argument(Branches)]);
            (Subtrees) ->
                with_scope_group(Subtrees)
        end,
        fun
            ([Timeout, Branches]) ->
                {Clauses, Body} = lists:split(ClauseCount, Branches),
                [Clauses, Timeout, Body];
            (Subtrees) ->
                Subtrees
        end,
        Receive
    );
walk_node_1(try_expr, Try, #{}) ->
    %% Body bindings are visible in of-clauses, but not catch/after clauses.
    BodyLength = length(erl_syntax:try_expr_body(Try)),
    ClauseCount = length(erl_syntax:try_expr_clauses(Try)),
    Sequence = fun([Body, Clauses, Handlers, After]) ->
        [
            with_shadowed_leaky(
                with_scope_group(
                    with_shadowed_leaky(Body ++ Clauses) ++ with_shadowed_leaky(Handlers)
                )
            ),
            with_shadowed_leaky(After)
        ]
    end,
    Reduce = fun([BeforeAfter, After]) ->
        {Body, Rest} = lists:split(BodyLength, BeforeAfter),
        {Clauses, Handlers} = lists:split(ClauseCount, Rest),
        [Body, Clauses, Handlers, After]
    end,
    with_shadowed_leaky(astranaut_uniplate:with_subtrees(Sequence, Reduce, Try));
walk_node_1(catch_expr, Catch, #{}) ->
    with_shadowed_leaky(walk_clause_parent_expression(Catch));
walk_node_1(maybe_expr, Maybe, #{}) ->
    walk_maybe(Maybe);
walk_node_1(_NodeType, Node, #{}) ->
    Node.

clause_scope_type(function) ->
    shadowed;
clause_scope_type(fun_expr) ->
    shadowed;
clause_scope_type(named_fun_expr) ->
    shadowed;
clause_scope_type(_Other) ->
    nonfun_clause.

walk_comprehension(Comprehension) ->
    walk_comprehension(Comprehension, false).
walk_comprehension(Comprehension, Leaky) ->
    Sequence =
        fun(Subtrees) ->
            Subtrees1 = lists:reverse(Subtrees),
            case Leaky of
                true ->
                    with_shadowed_leaky(Subtrees1);
                false ->
                    with_shadowed(Subtrees1)
            end
        end,
    Reduce = fun lists:reverse/1,
    astranaut_uniplate:with_subtrees(Sequence, Reduce, Comprehension).

%% strange, variables in binary comprehension is not shadowed, but not usable in next expressions.
walk_binary_comprehension(BinaryComprehension) ->
    walk_comprehension(BinaryComprehension, true).

walk_generator(Generator) ->
    Sequence =
        fun([Patterns, Expressions]) ->
            Patterns1 = with_comprehension_generate_pattern(Patterns),
            Expressions1 = with_shadowed(Expressions),
            %% walk expression first
            [Expressions1, Patterns1]
        end,
    Reduce = fun lists:reverse/1,
    astranaut_uniplate:with_subtrees(Sequence, Reduce, Generator).

walk_match(Match) ->
    Sequence =
        fun([Patterns, Expressions]) ->
            Patterns1 = with_match_left_pattern(Patterns),
            %% walk expression first
            [Expressions, Patterns1]
        end,
    Reduce = fun lists:reverse/1,
    astranaut_uniplate:with_subtrees(Sequence, Reduce, Match).

walk_clause(Clause, Attr) ->
    Parent = maps:get(parent, Attr, fun_expr),
    ScopeType = clause_scope_type(Parent),
    PatternType = scope_type_pattern(ScopeType),
    Sequence =
        fun
            ([Patterns, Guards, Expressions]) ->
                Patterns1 = with_scope_type(PatternType, Patterns),
                with_scope_type(ScopeType, [Patterns1, Guards, Expressions]);
            ([Patterns, Expressions]) ->
                Patterns1 = with_scope_type(PatternType, Patterns),
                with_scope_type(ScopeType, [Patterns1, Expressions])
        end,
    astranaut_uniplate:with_subtrees(Sequence, Clause).

%% Function Name in named fun should also be pattern and whole scope is shadowed.
walk_named_fun(NamedFun) ->
    Sequence =
        fun([NameTree | RestTrees]) ->
            NameTree1 = with_function_clause_pattern(NameTree),
            with_shadowed([NameTree1 | RestTrees])
        end,
    astranaut_uniplate:with_subtrees(Sequence, NamedFun).

walk_maybe(Maybe) ->
    Sequence =
        fun([MaybeBody | Elses]) ->
            %% treat maybe body as shadowed_leaky scope like clause scope
            MaybeBody1 = with_shadowed_leaky(MaybeBody),
            %% walk expression first
            [MaybeBody1 | Elses]
        end,
    walk_clause_parent_expression(astranaut_uniplate:with_subtrees(Sequence, Maybe)).

walk_clause_parent_expression(ClauseParent) ->
    Sequence = fun with_scope_group/1,
    astranaut_uniplate:with_subtrees(Sequence, ClauseParent).

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
%% Strict argument isolation keeps sibling bindings invisible until group exit.
walk_scope_group_expression(Node) ->
    Sequence = fun sequence_scope_group_with_argument/1,
    astranaut_uniplate:with_subtrees(Sequence, Node).

sequence_scope_group_subtrees(Subtrees) ->
    with_scope_group(Subtrees).

sequence_scope_group_with_argument(Subtreess) ->
    sequence_scope_group_subtrees(
        lists:map(fun(Subtrees) -> lists:map(fun with_argument/1, Subtrees) end, Subtreess)
    ).

with_scope_group(NodeM) ->
    with_scope_type(scope_group, NodeM).

with_argument(Trees) ->
    with_scope_type(argument, Trees).

with_shadowed(NodeM) ->
    with_scope_type(shadowed, NodeM).

with_shadowed_leaky(NodeM) ->
    with_scope_type(shadowed_leaky, NodeM).

with_match_left_pattern(NodeM) ->
    with_scope_type(match_left, NodeM).

with_function_clause_pattern(NodeM) ->
    with_scope_type(function_clause, NodeM).

with_comprehension_generate_pattern(NodeM) ->
    with_scope_type(comprehension_generate, NodeM).

with_scope_type(ScopeType, Trees) ->
    astranaut_uniplate:with(
        fun(Context) -> entry_scope_type(ScopeType, Context) end,
        fun(Context) -> exit_scope_type(ScopeType, Context) end,
        Trees
    ).

%% Only lexical snapshots live on frames. Metrics and binding identities are
%% traversal-wide and never roll back when a scope exits.
entry_scope_type(Kind, {Caller, C}) ->
    {Caller, entry_scope_type(Kind, C)};
entry_scope_type(scope_group, C) ->
    G = #group_frame{saved_env = C#scope_state.env},
    C#scope_state{groups = [G | C#scope_state.groups]};
entry_scope_type(Kind, C) ->
    F = #scope_frame{
        kind = Kind,
        allocated = C#scope_state.allocated,
        renames = C#scope_state.renames,
        env = C#scope_state.env,
        pattern = C#scope_state.pattern,
        pattern_vars = C#scope_state.pattern_vars
    },
    C1 = C#scope_state{frames = [F | C#scope_state.frames]},
    case is_pattern_scope(Kind) of
        true -> C1#scope_state{pattern = Kind, pattern_vars = []};
        false -> C1
    end.

exit_scope_type(Kind, {Caller, C}) ->
    {Caller, exit_scope_type(Kind, C)};
exit_scope_type(scope_group, #scope_state{groups = [G | Rest]} = C) ->
    Exports =
        case G#group_frame.branch_envs of
            none -> #{};
            E -> E
        end,
    %% Branch exports cannot replace bindings already visible on entry.
    Env = maps:merge(Exports, C#scope_state.env),
    C1 = C#scope_state{
        groups = Rest,
        env = maps:merge(Env, maps:with(maps:keys(G#group_frame.renames), Exports)),
        allocated = ordsets:union(C#scope_state.allocated, G#group_frame.allocated),
        renames = maps:merge(C#scope_state.renames, G#group_frame.renames)
    },
    New = maps:without(maps:keys(G#group_frame.saved_env), Exports),
    join_bindings(maps:merge(New, maps:with(maps:keys(G#group_frame.renames), Exports)), C1);
exit_scope_type(Kind, #scope_state{frames = [#scope_frame{kind = Kind} = F | Rest]} = C) ->
    C1 = C#scope_state{frames = Rest},
    case is_pattern_scope(Kind) of
        true ->
            C1#scope_state{
                pattern = F#scope_frame.pattern, pattern_vars = F#scope_frame.pattern_vars
            };
        false ->
            C2 = C1#scope_state{renames = F#scope_frame.renames, env = F#scope_frame.env},
            case Kind of
                shadowed ->
                    C2#scope_state{allocated = F#scope_frame.allocated};
                shadowed_leaky ->
                    C2;
                nonfun_clause ->
                    add_branch(
                        C#scope_state.env,
                        C#scope_state.allocated,
                        C2#scope_state{allocated = F#scope_frame.allocated}
                    );
                argument ->
                    add_argument(C, F, C2)
            end
    end.

is_pattern_scope(function_clause) -> true;
is_pattern_scope(clause_match) -> true;
is_pattern_scope(match_left) -> true;
is_pattern_scope(comprehension_generate) -> true;
is_pattern_scope(_) -> false.

join_bindings(Exports, C) ->
    maps:fold(
        fun
            (_, [], Acc) ->
                Acc;
            (
                Orig,
                Ids,
                #scope_state{bindings = Table, renames = Renames} = Acc
            ) ->
                Names = ordsets:from_list([(maps:get(Id, Table))#bind_entry.final || Id <- Ids]),
                case Names of
                    [Name] -> Acc#scope_state{renames = maps:put(Orig, Name, Renames)};
                    _ -> Acc#scope_state{renames = maps:remove(Orig, Renames)}
                end
        end,
        C,
        Exports
    ).

add_branch(Env, Allocated, #scope_state{groups = [G | Rest]} = C) ->
    Common =
        case G#group_frame.branch_envs of
            none ->
                Env;
            Previous ->
                maps:map(
                    fun(K, Ids) -> ordsets:union(Ids, maps:get(K, Env)) end,
                    maps:with(maps:keys(Env), Previous)
                )
        end,
    C#scope_state{
        groups = [
            G#group_frame{
                branch_envs = Common,
                allocated = ordsets:union(G#group_frame.allocated, Allocated)
            }
            | Rest
        ]
    }.

add_argument(Inner, F, #scope_state{groups = [G | Rest]} = C) ->
    Added = maps:filter(
        fun(K, V) -> maps:find(K, F#scope_frame.renames) =/= {ok, V} end,
        Inner#scope_state.renames
    ),
    AddedEnv = maps:filter(
        fun(K, V) -> maps:find(K, G#group_frame.saved_env) =/= {ok, V} end,
        Inner#scope_state.env
    ),
    Previous =
        case G#group_frame.branch_envs of
            none -> #{};
            E -> E
        end,
    {Merged, C1} = merge_arguments(Previous, AddedEnv, C),
    C1#scope_state{
        groups = [
            G#group_frame{
                renames = maps:merge(G#group_frame.renames, Added),
                branch_envs = Merged
            }
            | Rest
        ]
    }.

merge_arguments(Previous, Added, C) ->
    maps:fold(
        fun(Orig, Ids, {Acc, Context}) ->
            Both = ordsets:union(maps:get(Orig, Acc, []), Ids),
            Names = ordsets:from_list([
                (maps:get(Id, Context#scope_state.bindings))#bind_entry.final
             || Id <- Both
            ]),
            case maps:is_key(Orig, Acc) andalso length(Names) =:= 1 of
                true ->
                    Context1 = touch_bindings(Both, Context),
                    Events =
                        case map_size(Context#scope_state.metrics) of
                            0 ->
                                Context1#scope_state.events;
                            _ ->
                                [
                                    {use, Orig, {binding, hd(Both)}, false}
                                    | Context1#scope_state.events
                                ]
                        end,
                    {maps:put(Orig, Both, Acc), Context1#scope_state{events = Events}};
                false ->
                    {maps:put(Orig, Ids, Acc), Context}
            end
        end,
        {Previous, C},
        Added
    ).

scope_type_pattern(shadowed) -> function_clause;
scope_type_pattern(nonfun_clause) -> clause_match.

%% Batch helpers used by AST-producing passes.
-spec fresh_var_asts(var_base(), non_neg_integer(), erl_anno:location(), var_scope()) ->
    [astranaut:tree()].
fresh_var_asts(Base, Count, Pos, Scope) when is_integer(Count), Count >= 0 ->
    [{var, Pos, Name} || Name <- fresh_vars(lists:duplicate(Count, Base), Scope, #{})].

-spec alloc_var_asts([var_base()], erl_anno:location(), var_scope()) ->
    {[astranaut:tree()], ordsets:ordset(var_name())}.
alloc_var_asts(Bases, Pos, Scope) ->
    Allocated = scope_vars(Scope),
    Names = fresh_vars(Bases, Allocated, #{}),
    {[{var, Pos, Name} || Name <- Names], ordsets:union(ordsets:from_list(Names), Allocated)}.
