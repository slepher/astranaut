%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2020, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  6 Jul 2020 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_error_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("astranaut/include/rebinding.hrl").

-rebinding_all([{clause_pinned, true}]).

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
    [test_state_1, test_state_2, test_state_3, test_state_4, test_state_5, test_state_6].

%%--------------------------------------------------------------------
%% @spec TestCase() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
test_merge_1() -> 
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
test_state_1(_Config) ->
    Init = astranaut_error_state:new(),
    Errors = [error_0],
    Warnings = [],
    State = astranaut_error_state:append_error(error_0, Init),
    ?assertEqual({Errors, Warnings}, {astranaut_error_state:errors(State), astranaut_error_state:warnings(State)}),
    ok.

test_state_2(_Config) ->
    Init = astranaut_error_state:new(),
    Errors = [error_0, error_1],
    State = astranaut_error_state:append_error(error_0, Init),
    State = astranaut_error_state:append_error(error_1, State),
    ?assertEqual(Errors, astranaut_error_state:errors(State)),
    ok.

test_state_3(_Config) ->
    Init = astranaut_error_state:new(),
    Errors = [error_0, error_1],
    Warnings = [warning_0, warning_1, warning_2],
    State = astranaut_error_state:append_warnings([warning_0, warning_1], Init),
    State = astranaut_error_state:append_error(error_0, State),
    State = astranaut_error_state:append_error(error_1, State),
    State = astranaut_error_state:append_warning(warning_2, State),
    ?assertEqual({Errors, Warnings}, {astranaut_error_state:errors(State), astranaut_error_state:warnings(State)}),
    ok.

test_state_4(_Config) ->
    State = astranaut_error_state:new(),
    Errors = [{10, ?MODULE, error_0}, {20, ?MODULE, error_1}],
    Warnings = [{5, ?MODULE, warning_0}, {15, ?MODULE, warning_1}, {25, ?MODULE, warning_2}],
    State = astranaut_error_state:append_warning(warning_0, State),
    State = astranaut_error_state:update_line(5, ?MODULE, State),
    State = astranaut_error_state:append_error(error_0, State),
    State = astranaut_error_state:update_line(10, ?MODULE, State),
    State = astranaut_error_state:append_warning(warning_1, State),
    State = astranaut_error_state:update_line(15, ?MODULE, State),
    State = astranaut_error_state:append_error(error_1, State),
    State = astranaut_error_state:update_line(20, ?MODULE, State),
    State = astranaut_error_state:append_warning(warning_2, State),
    State = astranaut_error_state:update_line(25, ?MODULE, State),
    ?assertEqual({Errors, Warnings}, {astranaut_error_state:formatted_errors(State), 
                                      astranaut_error_state:formatted_warnings(State)}),
    ok.

test_state_5(_Config) ->
    State = astranaut_error_state:new(),
    Errors = [{?FILE, [{10, ?MODULE, error_0}, {20, ?MODULE, error_1}]}],
    Warnings = [{?FILE, [{5, ?MODULE, warning_0}, {15, ?MODULE, warning_1}, {25, ?MODULE, warning_2}]}],
    State = astranaut_error_state:append_warnings([warning_0], State),
    State = astranaut_error_state:update_line(5, ?MODULE, State),
    State = astranaut_error_state:append_errors([error_0], State),
    State = astranaut_error_state:update_line(10, ?MODULE, State),
    State = astranaut_error_state:append_warnings([warning_1], State),
    State = astranaut_error_state:update_line(15, ?MODULE, State),
    State = astranaut_error_state:update_file(?FILE,  State),
    State = astranaut_error_state:append_errors([error_1], State),
    State = astranaut_error_state:update_line(20, ?MODULE, State),
    State = astranaut_error_state:append_warnings([warning_2], State),
    State = astranaut_error_state:update_line(25, ?MODULE, State),
    State = astranaut_error_state:eof(State),
    ?assertEqual({Errors, Warnings}, astranaut_error_state:realize(State)),
    ok.

test_state_6(_Config) ->
    State = astranaut_error_state:new(),
    File2 = ?FILE ++ "_2",
    Errors = maps:to_list(#{?FILE =>[{5, ?MODULE, error_0}, {10, ?MODULE, error_1}], File2 => [{20, ?MODULE, error_2}]}),
    Warnings = [{?FILE, [{5, ?MODULE, warning_0}, {15, ?MODULE, warning_1}, 
                         {25, ?MODULE, warning_2}, {30, ?MODULE, warning_3}]}],
    State = astranaut_error_state:append_warning(warning_0, State),
    State = astranaut_error_state:append_error(error_0, State),
    State = astranaut_error_state:update_line(5, ?MODULE, State),
    State = astranaut_error_state:append_error(error_1, State),
    State = astranaut_error_state:update_line(10, ?MODULE, State),
    State = astranaut_error_state:update_file(?FILE, State),
    State = astranaut_error_state:append_warning(warning_1, State),
    State = astranaut_error_state:update_line(15, ?MODULE, State),
    State = astranaut_error_state:update_file(File2,  State),
    State = astranaut_error_state:append_error(error_2, State),
    State = astranaut_error_state:update_line(20, ?MODULE, State),
    State = astranaut_error_state:update_file(?FILE, State),
    State = astranaut_error_state:append_warning(warning_2, State),
    State = astranaut_error_state:update_line(25, ?MODULE, State),
    State = astranaut_error_state:append_warning(warning_3, State),
    State = astranaut_error_state:update_line(30, ?MODULE, State),
    State = astranaut_error_state:eof(State),
    ?assertEqual({Errors, Warnings}, astranaut_error_state:realize(State)),
    ok.
