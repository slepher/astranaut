%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2020, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  6 Jul 2020 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_traverse_m_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("astranaut/include/astranaut_do.hrl").

%%--------------------------------------------------------------------
%% @spec suite() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{seconds,30}}].

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
    [test_return, test_bind, test_error_0, test_state, 
     test_line, test_line_2, test_file_line, test_fail, test_sequence_either,
     test_bind_node, test_bind_node_continue, test_bind_node_update].

%%--------------------------------------------------------------------
%% @spec TestCase() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
test_return() -> 
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
test_return(_Config) -> 
    MA = astranaut_traverse_m:return(10),
    Result = astranaut_return_m:return_ok({10, ok}),
    ?assertEqual(Result, astranaut_traverse_m:run(MA, undefined, ok)),
    ok.

test_bind(_Config) ->
    MA = 
        do([astranaut_traverse_m ||
               A <- astranaut_traverse_m:return(10),
               return(A + 10)
           ]),
    Result = astranaut_return_m:return_ok({20, ok}),
    ?assertEqual(Result, astranaut_traverse_m:run(MA, undefined, ok)),
    ok.

test_error_0(_Config) ->
    MA = 
        do([astranaut_traverse_m ||
               A <- astranaut_traverse_m:return(10),
               astranaut_traverse_m:update_line(
                 10, astranaut_traverse_m:error(error_0)),
               return(A + 10)
           ]),
    ErrorState = astranaut_error_state:new(),
    ErrorState1 = astranaut_error_state:append_errors([{10, formatter_0, error_0}], ErrorState),
    Result = astranaut_return_m:return_ok({20, ok}, ErrorState1),
    ?assertEqual(Result, astranaut_traverse_m:run(MA, formatter_0, ok)),
    ok.

test_state(_Config) ->
    MA = 
        do([astranaut_traverse_m ||
               astranaut_traverse_m:put(10),
               astranaut_traverse_m:state(
                 fun(A) ->
                         {A + 10, A + 20}
                 end)
           ]),
    Result = astranaut_return_m:return_ok({20, 30}),
    ?assertEqual(Result, astranaut_traverse_m:run(MA, undefined, ok)),
    ok.

test_line(_Config) ->
    MA = 
        do([astranaut_traverse_m ||
               astranaut_traverse_m:put(10),
               astranaut_traverse_m:update_line(
                 20, astranaut_traverse_m:error(error_0)),
               astranaut_traverse_m:state(
                 fun(A) ->
                         {A + 10, A + 20}
                 end)
           ]),
    Errors = [{20, formatter_0, error_0}], 
    #{return := Return, error := Error} = astranaut_traverse_m:run(MA, formatter_0, ok),
    ?assertEqual({{20, 30}, Errors}, {Return, astranaut_error_state:errors(Error)}),
    ok.


test_line_2(_Config) ->
    MA = 
        do([astranaut_traverse_m ||
               astranaut_traverse_m:update_line(
                 20, astranaut_traverse_m:error(error_0)),
               return(10)
           ]),
    Errors = [{20, formatter_0, error_0}],
    #{return := Return, error := Error} = astranaut_traverse_m:run(MA, formatter_0, ok),
    ?assertEqual({{10, ok}, Errors}, {Return, astranaut_error_state:errors(Error)}),
    ok.

test_file_line(_Config) ->
    MA = 
        do([astranaut_traverse_m ||
               astranaut_traverse_m:update_file(?FILE),
               astranaut_traverse_m:put(10),
               astranaut_traverse_m:with_formatter(
                 astranaut_traverse_m,
                 astranaut_traverse_m:update_line(
                   20,
                 astranaut_traverse_m:error(error_0)
                )),
               astranaut_traverse_m:update_line(
                 25, astranaut_traverse_m:warning(warning_0)),
               B <- astranaut_traverse_m:get(),
               astranaut_traverse_m:modify(
                 fun(A) ->
                         A + 20
                 end),
               astranaut_traverse_m:eof(),
               return(B + 10)
           ]),
    FileErrors = [{?FILE, [{20, astranaut_traverse_m, error_0}]}],
    FileWarnings = [{?FILE, [{25, ?MODULE, warning_0}]}],
    #{return := Result, error := Error} = astranaut_traverse_m:run(MA, ?MODULE, ok),
    ?assertEqual({{20, 30}, {FileErrors, FileWarnings}}, {Result, astranaut_error_state:realize(Error)}),
    ok.

test_fail(_Config) ->
    MA =
        do([astranaut_traverse_m ||
               astranaut_traverse_m:put(10),
               astranaut_traverse_m:with_formatter(
                 astranaut_traverse_m,
                 astranaut_traverse_m:update_line(
                   20,
                   astranaut_traverse_m:error(error_0))
                ),
               astranaut_traverse_m:update_line(
                 25, astranaut_traverse_m:warning(warning_0)),
               B <- astranaut_traverse_m:get(),
               astranaut_traverse_m:modify(
                 fun(A) ->
                         A + 20
                 end),
               return(B)
           ]),
    MB = do([astranaut_traverse_m ||
                astranaut_traverse_m:fail_on_error(MA),
                astranaut_traverse_m:put(30),
                astranaut_traverse_m:update_line(
                  30, astranaut_traverse_m:error(error_1))
            ]),
    Errors = [{20, astranaut_traverse_m, error_0}],
    Warnings = [{25, ?MODULE, warning_0}],
    #{error := Error} = astranaut_traverse_m:run(MB, ?MODULE, ok),
    ?assertEqual({Errors, Warnings}, {astranaut_error_state:errors(Error), astranaut_error_state:warnings(Error)}),
    ok.

test_sequence_either(_Config) ->
    MA1 =
        astranaut_traverse_m:fail_on_error(
          do([astranaut_traverse_m ||
                 A <- astranaut_traverse_m:get(),
                 astranaut_traverse_m:put(A + 10),
                 astranaut_traverse_m:update_line(
                   20, astranaut_traverse_m:error(error_0))
             ])),
    MA2 =
        astranaut_traverse_m:fail_on_error(
          do([astranaut_traverse_m ||
                 A <- astranaut_traverse_m:get(),
                 astranaut_traverse_m:put(A + 10),
                 astranaut_traverse_m:update_line(
                   25, astranaut_traverse_m:warning(warning_0))
             ])),
    MA3 =
        astranaut_traverse_m:fail_on_error(
          do([astranaut_traverse_m ||
                 A <- astranaut_traverse_m:get(),
                 astranaut_traverse_m:put(A + 10),
                 astranaut_traverse_m:update_line(
                   30, astranaut_traverse_m:fail(error_1))
             ])),
    MA4 = astranaut_traverse_m:update_file(?FILE),
    MA5 = astranaut_traverse_m:eof(),
    MAS0 = astranaut_traverse_m:sequence_either([MA1, MA2, MA3]),
    MAS1 = astranaut_traverse_m:sequence_either([MA4, MAS0, MA5]),
    FileErrors = [{?FILE, [{20, ?MODULE, error_0}, {30, ?MODULE, error_1}]}],
    FileWarnings = [{?FILE,[{25, ?MODULE, warning_0}]}],
    #{error := Error} = astranaut_traverse_m:run(MAS1, ?MODULE, 0),
    ?assertEqual({FileErrors, FileWarnings}, astranaut_error_state:realize(Error)),
    ok.

%% test_bind_node(_Config) -> 
%%     NodeA = {atom, 10, 'A'},
%%     Walk = astranaut_walk_return:new(#{}),
%%     Return = 
%%         bind_pre(NodeA, Walk,
%%                  fun({atom, _Line, A}) ->
%%                          NodeC = {atom, 20, A},
%%                          astranaut_walk_return:new(#{node => NodeC})
%%                  end),
%%     ?assertEqual({atom, 20, 'A'}, Return),
%%     ok.

%% test_bind_node_continue(_Config) -> 
%%     NodeA = {atom, 10, 'A'},
%%     Walk = astranaut_walk_return:new(#{continue => true, node => {atom, 10, 'B'}}),
%%     Return = 
%%         bind_pre(NodeA, Walk,
%%                  fun({atom, _Line, A}) ->
%%                          NodeC = {atom, 20, A},
%%                          astranaut_walk_return:new(#{node => NodeC})
%%                  end),
%%     ?assertEqual({atom, 10, 'B'}, Return),
%%     ok.

%% test_bind_node_update(_Config) ->
%%     NodeA = {atom, 10, 'A'},
%%     Walk = astranaut_walk_return:new(#{node => {atom, 10, 'B'}}),
%%     Return = 
%%         bind_pre(NodeA, Walk,
%%                  fun({atom, _Line, A}) ->
%%                          NodeC = {atom, 20, A},
%%                          astranaut_walk_return:new(#{node => NodeC})
%%                  end),
%%     ?assertEqual({atom, 20, 'B'}, Return),
%%     ok.

test_bind_node(_Config) -> 
    NodeA = {atom, 10, 'A'},
    Walk = astranaut_walk_return:new(#{}),
    Return = 
        bind_pre(NodeA, Walk,
                 fun({atom, _Line, A}) ->
                         NodesC = [{atom, 20, A}, {atom, 30, A}],
                         astranaut_walk_return:new(#{nodes => NodesC})
                 end),
    ?assertEqual([{atom, 20, 'A'}, {atom, 30, 'A'}], Return),
    ok.

test_bind_node_continue(_Config) -> 
    NodeA = {atom, 10, 'A'},
    Walk = astranaut_walk_return:new(#{continue => true, nodes => [{atom, 10, 'B'}, {atom, 10, 'C'}]}),
    Return = 
        bind_pre(NodeA, Walk,
                 fun({atom, _Line, A}) ->
                         NodeC = {atom, 20, A},
                         astranaut_walk_return:new(#{node => NodeC})
                 end),
    ?assertEqual([{atom, 10, 'B'}, {atom, 10, 'C'}], Return),
    ok.

test_bind_node_update(_Config) ->
    NodeA = {atom, 10, 'A'},
    Walk = astranaut_walk_return:new(#{nodes => [{atom, 10, 'B'}, {atom, 10, 'C'}]}),
    Return = 
        bind_pre(NodeA, Walk,
                 fun({atom, _Line, A}) ->
                         NodesC = [{atom, 20, A}, {atom, 30, A}],
                         astranaut_walk_return:new(#{nodes => NodesC})
                 end),
    ?assertEqual([{atom, 20, 'B'}, {atom, 30, 'B'}, {atom, 20, 'C'}, {atom, 30, 'C'}], Return),
    ok.

bind_pre(NodeA, Walk, BWC) ->
    MNodeB = astranaut_traverse_m:astranaut_traverse_m(Walk),
    MNodeC = 
        astranaut_traverse_m:pop_nodes(
          astranaut_traverse_m:bind_continue_nodes(
            astranaut_traverse_m:updated_node(NodeA, MNodeB),
            fun(NodeB) ->
                    Walk2 = BWC(NodeB),
                    MNodeC = astranaut_traverse_m:astranaut_traverse_m(Walk2),
                  astranaut_traverse_m:updated_node(NodeB, MNodeC)
            end)),
    io:format("new node is ~p~n", [MNodeC]),
    #{return := Return} = astranaut_traverse_m:eval(MNodeC, hello, ok),
    Return.
