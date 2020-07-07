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
    [test_return, test_bind, test_error_0, test_state, test_line, test_line_2, test_file_line, test_fail, test_sequence_either].

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
    Result = astranaut_traverse_m:state_ok(10, ok),
    ?assertEqual(Result, astranaut_traverse_m:run(MA, undefined, ok)),
    ok.

test_bind(_Config) ->
    MA = 
        do([astranaut_traverse_m ||
               A <- astranaut_traverse_m:return(10),
               return(A + 10)
           ]),
    Result = astranaut_traverse_m:state_ok(20, ok),
    ?assertEqual(Result, astranaut_traverse_m:run(MA, undefined, ok)),
    ok.

test_error_0(_Config) ->
    MA = 
        do([astranaut_traverse_m ||
               A <- astranaut_traverse_m:return(10),
               astranaut_traverse_m:error(error_0),
               return(A + 10)
           ]),
    ErrorState = astranaut_traverse_m_error:new(),
    ErrorState1 = astranaut_traverse_m_error:error(error_0, ErrorState),
    Result = astranaut_traverse_m:state_ok(20, ok, ErrorState1),
    ?assertEqual(Result, astranaut_traverse_m:run(MA, undefined, ok)),
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
    Result = astranaut_traverse_m:state_ok(20, 30),
    ?assertEqual(Result, astranaut_traverse_m:run(MA, undefined, ok)),
    ok.

test_line(_Config) ->
    MA = 
        do([astranaut_traverse_m ||
               astranaut_traverse_m:put(10),
               astranaut_traverse_m:update_line(20),
               astranaut_traverse_m:error(error_0),
               astranaut_traverse_m:state(
                 fun(A) ->
                         {A + 10, A + 20}
                 end)
           ]),
    ErrorState = astranaut_traverse_m_error:new(),
    ErrorState1 = astranaut_traverse_m_error:error(error_0, ErrorState),
    ErrorState2 = astranaut_traverse_m_error:update_line(20, ?MODULE, ErrorState1),
    Result = astranaut_traverse_m:state_ok(20, 30, ErrorState2),
    ?assertEqual(Result, astranaut_traverse_m:run(MA, ?MODULE, ok)),
    ok.


test_line_2(_Config) ->
    MA = 
        do([astranaut_traverse_m ||
               astranaut_traverse_m:update_line(20),
               astranaut_traverse_m:error(error_0),
               astranaut_traverse_m:update_line(25),
               return(10)
           ]),
    ErrorState = astranaut_traverse_m_error:new(),
    ErrorState1 = ErrorState#{formatted_errors => [{20, ?MODULE, error_0}],
                              line => 25, formatter => ?MODULE},
    Result = astranaut_traverse_m:state_ok(10, ok, ErrorState1),
    ?assertEqual(Result, astranaut_traverse_m:run(MA, ?MODULE, ok)),
    ok.

test_file_line(_Config) ->
    MA = 
        do([astranaut_traverse_m ||
               astranaut_traverse_m:update_file(?FILE),
               astranaut_traverse_m:put(10),
               astranaut_traverse_m:update_line(20),
               astranaut_traverse_m:with_formatter(
                 astranaut_traverse_m,
                 astranaut_traverse_m:error(error_0)
                ),
               astranaut_traverse_m:update_line(25),
               astranaut_traverse_m:warning(warning_0),
               B <- astranaut_traverse_m:get(),
               astranaut_traverse_m:modify(
                 fun(A) ->
                         A + 20
                 end),
               astranaut_traverse_m:eof(),
               return(B + 10)
           ]),
    ErrorState = astranaut_traverse_m_error:new(),
    ErrorState1 = ErrorState#{file => undefined,
                              file_errors => #{?FILE => [{20, astranaut_traverse_m, error_0}]},
                              file_warnings => #{?FILE => [{25, ?MODULE, warning_0}]},
                              formatter => ?MODULE,
                              line => 25},
    Result = astranaut_traverse_m:state_ok(20, 30, ErrorState1),
    ?assertEqual(Result, astranaut_traverse_m:run(MA, ?MODULE, ok)),
    ok.

test_fail(_Config) ->
    MA =
        do([astranaut_traverse_m ||
               astranaut_traverse_m:put(10),
               astranaut_traverse_m:update_line(20),
               astranaut_traverse_m:with_formatter(
                 astranaut_traverse_m,
                 astranaut_traverse_m:error(error_0)
                ),
               astranaut_traverse_m:update_line(25),
               astranaut_traverse_m:warning(warning_0),
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
                astranaut_traverse_m:update_line(30),
                astranaut_traverse_m:error(error_1)
            ]),
    ErrorState = astranaut_traverse_m_error:new(),
    ErrorState1 = ErrorState#{formatted_errors => [{20, astranaut_traverse_m, error_0}],
                              formatted_warnings => [{25, ?MODULE, warning_0}],
                              line => 25, formatter => ?MODULE},
    Result = astranaut_traverse_m:state_fail(30, ErrorState1),
    ?assertEqual(Result, astranaut_traverse_m:run(MB, ?MODULE, ok)),
    ok.

test_sequence_either(_Config) ->
    MA1 =
        astranaut_traverse_m:fail_on_error(
          do([astranaut_traverse_m ||
                 A <- astranaut_traverse_m:get(),
                 astranaut_traverse_m:put(A + 10),
                 astranaut_traverse_m:update_line(20),
                 astranaut_traverse_m:error(error_0)
             ])),
    MA2 =
        astranaut_traverse_m:fail_on_error(
          do([astranaut_traverse_m ||
                 A <- astranaut_traverse_m:get(),
                 astranaut_traverse_m:put(A + 10),
                 astranaut_traverse_m:update_line(25),
                 astranaut_traverse_m:warning(warning_0)
             ])),
    MA3 =
        astranaut_traverse_m:fail_on_error(
          do([astranaut_traverse_m ||
                 A <- astranaut_traverse_m:get(),
                 astranaut_traverse_m:put(A + 10),
                 astranaut_traverse_m:update_line(30),
                 astranaut_traverse_m:fail(error_1)
             ])),
    MA4 = astranaut_traverse_m:update_file(?FILE),
    MA5 = astranaut_traverse_m:eof(),
    MAS0 = astranaut_traverse_m:sequence_either([MA1, MA2, MA3]),
    MAS1 = astranaut_traverse_m:sequence_either([MA4, MAS0, MA5]),
    ErrorState = astranaut_traverse_m_error:new(),
    ErrorState1 = ErrorState#{file_errors => #{?FILE => [{20, ?MODULE, error_0}, {30, ?MODULE, error_1}]},
                              file_warnings => #{?FILE =>  [{25, ?MODULE, warning_0}]},
                              file => undefined,
                              line => 30, formatter => ?MODULE},
    Result = astranaut_traverse_m:state_fail(30, ErrorState1),
    ?assertEqual(Result, astranaut_traverse_m:run(MAS1, ?MODULE, 0)),
    ok.
