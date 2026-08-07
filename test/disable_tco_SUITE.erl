%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2018, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 10 Dec 2018 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(disable_tco_SUITE).

-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").
%%--------------------------------------------------------------------
%% @spec suite() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap, {seconds, 30}}].

%%--------------------------------------------------------------------
%% @spec init_per_suite(Config0) ->
%%     Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_suite(Config0) -> term() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_group(GroupName, Config0) ->
%%               term() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_testcase(TestCase, Config0) ->
%%               term() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec groups() -> [Group]
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%% Shuffle = shuffle | {shuffle,{integer(),integer(),integer()}}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%% N = integer() | forever
%% @end
%%--------------------------------------------------------------------
groups() ->
    [].

%%--------------------------------------------------------------------
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
all() ->
    [
        disable_tco,
        disable_tco_nested_control_flow,
        disable_tco_transform_contract,
        disable_tco_nested_control_flow_transform_contract
    ].

%%--------------------------------------------------------------------
%% @spec TestCase() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
disable_tco() ->
    [].

%%--------------------------------------------------------------------
%% @spec TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% Comment = term()
%% @end
%%--------------------------------------------------------------------
disable_tco(_Config) ->
    try
        disable_tco_example:f(1)
    catch
        _:_:Stacktrace ->
            ?assertEqual(
                [{s, [1]}, {g, 2}, {'-f/1-fun-0-', 2}, {f, 1}], extract_stacktrace(Stacktrace)
            )
    end.

disable_tco_nested_control_flow(_Config) ->
    try
        disable_tco_example:nested_control_flow(1)
    catch
        _:_:Stacktrace ->
            ?assertEqual(
                [
                    {s, [1]},
                    {nested_helper, 1},
                    {nested_control_flow, 1}
                ],
                extract_stacktrace(Stacktrace)
            )
    end.

disable_tco_transform_contract(_Config) ->
    SelfCall = {call, 2, {atom, 2, self_call}, []},
    SelfFunction =
        {function, 2, self_call, 0, [{clause, 2, [], [], [SelfCall]}]},
    RemoteCall =
        {call, 3, {remote, 3, {atom, 3, erlang}, {atom, 3, error}}, [{atom, 3, expected}]},
    RemoteFunction =
        {function, 3, remote_call, 0, [{clause, 3, [], [], [RemoteCall]}]},
    LocalCall = {call, 4, {atom, 4, helper}, []},
    LocalFunction =
        {function, 4, local_call, 0, [{clause, 4, [], [], [LocalCall]}]},
    PlainExpression = {atom, 5, plain},
    PlainFunction =
        {function, 5, plain_expression, 0, [{clause, 5, [], [], [PlainExpression]}]},
    MultiHead = {integer, 6, 1},
    MultiTail = {call, 6, {atom, 6, helper}, []},
    MultiFunction =
        {function, 6, multiple_expressions, 0, [{clause, 6, [], [], [MultiHead, MultiTail]}]},
    CollisionCall =
        {call, 7, {remote, 7, {atom, 7, erlang}, {atom, 7, error}}, [{atom, 7, collision}]},
    CollisionFunction =
        {function, 7, variable_collision, 3, [
            {clause, 7,
                [
                    {var, 7, 'Class0'},
                    {var, 7, 'Exception0'},
                    {var, 7, 'StackTrace0'}
                ],
                [], [CollisionCall]}
        ]},
    AnonymousCall = {call, 8, {atom, 8, helper}, []},
    AnonymousFun =
        {'fun', 8, {clauses, [{clause, 8, [], [], [AnonymousCall]}]}},
    AnonymousFunction =
        {function, 8, anonymous_fun, 0, [{clause, 8, [], [], [AnonymousFun]}]},
    NamedCall = {call, 9, {atom, 9, helper}, []},
    NamedFun =
        {named_fun, 9, 'Loop', [{clause, 9, [], [], [NamedCall]}]},
    NamedFunction =
        {function, 9, named_fun, 0, [{clause, 9, [], [], [NamedFun]}]},
    [
        SelfFunction,
        TransformedRemoteFunction,
        TransformedLocalFunction,
        PlainFunction,
        TransformedMultiFunction,
        TransformedCollisionFunction,
        TransformedAnonymousFunction,
        TransformedNamedFunction
    ] =
        astranaut_disable_tco:parse_transform(
            [
                SelfFunction,
                RemoteFunction,
                LocalFunction,
                PlainFunction,
                MultiFunction,
                CollisionFunction,
                AnonymousFunction,
                NamedFunction
            ],
            []
        ),
    {function, 3, remote_call, 0, [{clause, 3, [], [], [TransformedRemoteCall]}]} =
        TransformedRemoteFunction,
    ?assertMatch(
        {'try', _, _, _, _, _},
        TransformedRemoteCall
    ),
    {function, 4, local_call, 0, [{clause, 4, [], [], [TransformedLocalCall]}]} =
        TransformedLocalFunction,
    ?assertMatch(
        {'try', _, _, _, _, _},
        TransformedLocalCall
    ),
    {function, 6, multiple_expressions, 0, [{clause, 6, [], [], [MultiHead, TransformedMultiTail]}]} =
        TransformedMultiFunction,
    ?assertMatch(
        {'try', _, _, _, _, _},
        TransformedMultiTail
    ),
    {function, 7, variable_collision, 3, [{clause, 7, _, [], [TransformedCollisionCall]}]} =
        TransformedCollisionFunction,
    CollisionVariables = variable_names(TransformedCollisionCall),
    ?assert(
        lists:any(
            fun(Name) -> variable_has_prefix("Class", Name) end,
            CollisionVariables
        )
    ),
    ?assert(
        lists:any(
            fun(Name) -> variable_has_prefix("Exception", Name) end,
            CollisionVariables
        )
    ),
    ?assertEqual(
        [],
        [
            Name
         || Name <- ['Class0', 'Exception0', 'StackTrace0'],
            lists:member(Name, CollisionVariables)
        ]
    ),
    {function, 8, anonymous_fun, 0, [
        {clause, 8, [], [], [
            {'fun', 8, {clauses, [{clause, 8, [], [], [TransformedAnonymousCall]}]}}
        ]}
    ]} =
        TransformedAnonymousFunction,
    ?assertMatch(
        {'try', _, _, _, _, _},
        TransformedAnonymousCall
    ),
    {function, 9, named_fun, 0, [
        {clause, 9, [], [], [{named_fun, 9, 'Loop', [{clause, 9, [], [], [TransformedNamedCall]}]}]}
    ]} =
        TransformedNamedFunction,
    ?assertMatch(
        {'try', _, _, _, _, _},
        TransformedNamedCall
    ),
    ok.

disable_tco_nested_control_flow_transform_contract(_Config) ->
    NestedFunction =
        parse_form(
            "nested(X) -> "
            "case X of "
            "first -> before(), if true -> begin target() end end; "
            "self -> nested(X) "
            "end."
        ),
    MutualA =
        parse_form(
            "mutual_a(X) -> "
            "case X of stop -> ok; _ -> mutual_b(X) end."
        ),
    MutualB =
        parse_form(
            "mutual_b(X) -> "
            "if X =:= stop -> ok; true -> mutual_a(X) end."
        ),
    ReceiveFunction =
        parse_form(
            "receive_control() -> "
            "receive go -> target() after 0 -> timeout() end."
        ),
    BooleanFunction =
        parse_form(
            "boolean_control(X) -> X andalso target()."
        ),
    TryFunction =
        parse_form(
            "try_control() -> "
            "try source() of "
            "X -> target(X) "
            "catch _:_ -> recover() "
            "end."
        ),
    [
        TransformedNested,
        TransformedMutualA,
        TransformedMutualB,
        TransformedReceive,
        TransformedBoolean,
        TransformedTry
    ] =
        astranaut_disable_tco:parse_transform(
            [
                NestedFunction,
                MutualA,
                MutualB,
                ReceiveFunction,
                BooleanFunction,
                TryFunction
            ],
            []
        ),
    {function, _, nested, 1, [
        {clause, _, _, _, [
            {'case', _, _, [
                {clause, _, [{atom, _, first}], _, [
                    BeforeCall,
                    {'if', _, [{clause, _, _, _, [{block, _, [NestedTailCall]}]}]}
                ]},
                {clause, _, [{atom, _, self}], _, [SelfCall]}
            ]}
        ]}
    ]} =
        TransformedNested,
    ?assertMatch({call, _, {atom, _, before}, []}, BeforeCall),
    ?assertMatch({'try', _, _, _, _, _}, NestedTailCall),
    ?assertMatch({call, _, {atom, _, nested}, [_]}, SelfCall),
    ?assertEqual(MutualA, TransformedMutualA),
    ?assertEqual(MutualB, TransformedMutualB),
    {function, _, receive_control, 0, [
        {clause, _, _, _, [
            {'receive', _, [{clause, _, _, _, [ReceiveTailCall]}], _, [TimeoutTailCall]}
        ]}
    ]} =
        TransformedReceive,
    ?assertMatch({'try', _, _, _, _, _}, ReceiveTailCall),
    ?assertMatch({'try', _, _, _, _, _}, TimeoutTailCall),
    {function, _, boolean_control, 1, [{clause, _, _, _, [{op, _, 'andalso', _, BooleanTailCall}]}]} =
        TransformedBoolean,
    ?assertMatch({'try', _, _, _, _, _}, BooleanTailCall),
    {function, _, try_control, 0, [
        {clause, _, _, _, [
            {'try', _, [ProtectedCall], [{clause, _, _, _, [TryTailCall]}],
                [{clause, _, _, _, [CatchTailCall]}], []}
        ]}
    ]} =
        TransformedTry,
    ?assertMatch({call, _, {atom, _, source}, []}, ProtectedCall),
    ?assertMatch({'try', _, _, _, _, _}, TryTailCall),
    ?assertMatch({'try', _, _, _, _, _}, CatchTailCall),
    ok.

extract_stacktrace(StackTrace) ->
    lists:reverse(
        lists:foldl(
            fun
                ({disable_tco_example, Function, Arity, _Attrs}, Acc) ->
                    [{Function, Arity} | Acc];
                (_, Acc) ->
                    Acc
            end,
            [],
            StackTrace
        )
    ).

variable_names({var, _Pos, Name}) ->
    [Name];
variable_names(Tuple) when is_tuple(Tuple) ->
    variable_names(tuple_to_list(Tuple));
variable_names([Head | Tail]) ->
    variable_names(Head) ++ variable_names(Tail);
variable_names(_) ->
    [].

variable_has_prefix(Prefix, Name) ->
    lists:prefix(Prefix, atom_to_list(Name)).

parse_form(Source) ->
    {ok, Tokens, _EndLocation} = erl_scan:string(Source),
    {ok, Form} = erl_parse:parse_form(Tokens),
    Form.
