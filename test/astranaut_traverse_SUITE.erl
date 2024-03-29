%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2020, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  6 Jul 2020 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_traverse_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include("do.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

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
     test_pos, test_pos_2, test_file_pos, test_fail].

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
    MA = astranaut_traverse:return(10),
    Result = astranaut_return:ok({10, ok}),
    ?assertEqual(Result, astranaut_traverse:run(MA, undefined, #{}, ok)),
    ok.

test_bind(_Config) ->
    MA = 
        do([ traverse ||
               A <- astranaut_traverse:return(10),
               return(A + 10)
           ]),
    Result = astranaut_return:ok({20, ok}),
    ?assertEqual(Result, astranaut_traverse:run(MA, undefined, #{}, ok)),
    ok.

test_error_0(_Config) ->
    MA = 
        do([ traverse ||
               A <- astranaut_traverse:return(10),
               astranaut_traverse:update_pos(
                 10, astranaut_traverse:error(error_0)),
               return(A + 10)
           ]),
    ErrorState = astranaut_error:new(),
    ErrorState1 = astranaut_error:append_formatted_errors([{10, formatter_0, error_0}], ErrorState),
    ErrorState2 = astranaut_error:printable(ErrorState1),
    ErrorStateM0 = astranaut_return:run_error(astranaut_traverse:run(MA, formatter_0, #{}, ok)),
    ErrorStateM1 = astranaut_error:printable(ErrorStateM0),
    ?assertEqual(ErrorState2, ErrorStateM1),
    ok.

test_state(_Config) ->
    MA = 
        do([ traverse ||
               astranaut_traverse:put(10),
               astranaut_traverse:state(
                 fun(A) ->
                         {A + 10, A + 20}
                 end)
           ]),
    Result = astranaut_return:ok({20, 30}),
    ?assertEqual(Result, astranaut_traverse:run(MA, undefined, #{}, ok)),
    ok.

test_pos(_Config) ->
    MA = 
        do([ traverse ||
               astranaut_traverse:put(10),
               astranaut_traverse:update_pos(
                 20, astranaut_traverse:error(error_0)),
               astranaut_traverse:state(
                 fun(A) ->
                         {A + 10, A + 20}
                 end)
           ]),
    Errors = [{20, formatter_0, error_0}], 
    #{return := Return, error := Error} = astranaut_traverse:run(MA, formatter_0, #{}, ok),
    ?assertEqual({{20, 30}, Errors}, {Return, astranaut_error:formatted_errors(Error)}),
    ok.


test_pos_2(_Config) ->
    MA = 
        do([ traverse ||
               astranaut_traverse:update_pos(
                 20, astranaut_traverse:error(error_0)),
               return(10)
           ]),
    Errors = [{20, formatter_0, error_0}],
    #{return := Return, error := Error} = astranaut_traverse:run(MA, formatter_0, #{}, ok),
    ?assertEqual({{10, ok}, Errors}, {Return, astranaut_error:formatted_errors(Error)}),
    ok.

test_file_pos(_Config) ->
    MA = 
        do([ traverse ||
               astranaut_traverse:update_file(?FILE),
               astranaut_traverse:put(10),
               astranaut_traverse:with_formatter(
                 astranaut_traverse,
                 astranaut_traverse:update_pos(
                   20,
                 astranaut_traverse:error(error_0)
                )),
               astranaut_traverse:update_pos(
                 25, astranaut_traverse:warning(warning_0)),
               B <- astranaut_traverse:get(),
               astranaut_traverse:modify(
                 fun(A) ->
                         A + 20
                 end),
               astranaut_traverse:eof(),
               return(B + 10)
           ]),
    FileErrors = [{?FILE, [{20, astranaut_traverse, error_0}]}],
    FileWarnings = [{?FILE, [{25, ?MODULE, warning_0}]}],
    #{return := Result, error := Error} = astranaut_traverse:run(MA, ?MODULE, #{}, ok),
    ?assertEqual({{20, 30}, {FileErrors, FileWarnings}}, {Result, astranaut_error:realize(Error)}),
    ok.

test_fail(_Config) ->
    MA =
        do([ traverse ||
               astranaut_traverse:put(10),
               astranaut_traverse:with_formatter(
                 astranaut_traverse,
                 astranaut_traverse:update_pos(
                   20,
                   astranaut_traverse:error(error_0))
                ),
               astranaut_traverse:update_pos(
                 25, astranaut_traverse:warning(warning_0)),
               B <- astranaut_traverse:get(),
               astranaut_traverse:modify(
                 fun(A) ->
                         A + 20
                 end),
               return(B)
           ]),
    MB = do([ traverse ||
                astranaut_traverse:fail_on_error(MA),
                astranaut_traverse:put(30),
                astranaut_traverse:update_pos(
                  30, astranaut_traverse:error(error_1))
            ]),
    Errors = [{20, astranaut_traverse, error_0}],
    Warnings = [{25, ?MODULE, warning_0}],
    #{error := Error} = astranaut_traverse:run(MB, ?MODULE, #{}, ok),
    ?assertEqual({Errors, Warnings}, {astranaut_error:formatted_errors(Error),
                                      astranaut_error:formatted_warnings(Error)}),
    ok.


%% test_bind_node(_Config) -> 
%%     NodeA = {atom, 10, 'A'},
%%     Walk = astranaut_walk_return:new(#{}),
%%     Return = 
%%         bind_pre(NodeA, Walk,
%%                  fun({atom, _Pos, A}) ->
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
%%                  fun({atom, _Pos, A}) ->
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
%%                  fun({atom, _Pos, A}) ->
%%                          NodeC = {atom, 20, A},
%%                          astranaut_walk_return:new(#{node => NodeC})
%%                  end),
%%     ?assertEqual({atom, 20, 'B'}, Return),
%%     ok.
