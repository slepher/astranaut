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
-include("stacktrace.hrl").
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
    [disable_tco,
     disable_tco_transform_contract,
     disable_tco_format_error_contract].

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
        _:_?CAPTURE_STACKTRACE ->
            ?assertEqual([{s, [1]}, {g, 2}, {'-f/1-fun-0-', 2}, {f, 1}], extract_stacktrace(?GET_STACKTRACE))
    end.

disable_tco_transform_contract(_Config) ->
    SelfCall = {call, 2, {atom, 2, self_call}, []},
    SelfFunction =
        {function, 2, self_call, 0,
         [{clause, 2, [], [], [SelfCall]}]},
    RemoteCall =
        {call, 3,
         {remote, 3, {atom, 3, erlang}, {atom, 3, error}},
         [{atom, 3, expected}]},
    RemoteFunction =
        {function, 3, remote_call, 0,
         [{clause, 3, [], [], [RemoteCall]}]},
    [SelfFunction, TransformedRemoteFunction] =
        astranaut_disable_tco:parse_transform(
          [SelfFunction, RemoteFunction], []),
    {function, 3, remote_call, 0,
     [{clause, 3, [], [], [TransformedRemoteCall]}]} =
        TransformedRemoteFunction,
    ?assertMatch(
       {'try', _, _, _, _, _},
       TransformedRemoteCall),
    ok.

disable_tco_format_error_contract(_Config) ->
    ?assertEqual(
       "already formatted",
       astranaut_disable_tco:format_error("already formatted")),
    ?assertEqual(
       "not_formatted",
       lists:flatten(
         astranaut_disable_tco:format_error(not_formatted))),
    ok.

extract_stacktrace(StackTrace) ->
    lists:reverse(
      lists:foldl(
        fun({disable_tco_example, Function, Arity, _Attrs}, Acc) ->
                [{Function, Arity}|Acc];
           (_, Acc) ->
                Acc
        end, [], StackTrace)).
    
