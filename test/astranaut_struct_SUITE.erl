%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2020, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 16 Jun 2020 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_struct_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").
-include("astranaut_struct_SUITE_data/test_record.hrl").

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
    TestModules = [astranaut_struct_test],
    astranaut_test_lib:load_data_modules(Config, TestModules).

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
    [test_struct_new, test_struct_update, test_struct_test,
     test_record_wrapper_index_access,
     test_from_record, test_to_record, test_from_map, test_update_struct,
     test_from_other_record,
     test_from_map_missing_name, test_update_missing_name, test_update_fail,
     test_parse_transform_import_contract, test_format_error_contract,
     test_compile_enforce_fail, test_compile_non_record_fail,
     test_compile_unknown_fields_fail, test_compile_missing_enforce_key_fail,
     test_compile_invalid_struct_name_fail].

%%--------------------------------------------------------------------
%% @spec TestCase() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @spec TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% Comment = term()
%% @end
%%--------------------------------------------------------------------
test_struct_new(_Config) -> 
    Test = astranaut_struct_test:new(),
    #{name := hello, value := <<"world">>} = Test,
    ok.

test_struct_update(_Config) -> 
    Test = astranaut_struct_test:new(),
    Test1 = astranaut_struct_test:update_name(Test, bye),
    #{name := bye, value := <<"world">>} = Test1,
    ok.

test_struct_test(_Config) ->
    Test = astranaut_struct_test:new(),
    hello = astranaut_struct_test:test(Test),
    ok.

test_record_wrapper_index_access(_Config) ->
    ?assertEqual(value, astranaut_struct_test:record_index()),
    Test = #{'__struct__' => test, name => hello},
    ?assertEqual(hello, astranaut_struct_test:access_name(Test)),
    ?assertEqual(<<"world">>, astranaut_struct_test:access_value(Test)),
    ok.
    
test_from_record(_Config) ->
    Test = #test{name = hello, value = world},
    Test1 = astranaut_struct_test:from_record(Test),
    ?assertEqual(#{'__struct__' => test,
                   name => hello, value => world, enable => true,
                   desc => undefined}, Test1),
    ok.

test_to_record(_Config) ->
    Test = #{'__struct__' => test, name => hello, value => world},
    Test1 = astranaut_struct_test:to_record(Test),
    ?assertEqual(#test{name = hello, value = world, 
                       enable = undefined, desc = undefined}, Test1),
    ok.

test_from_map(_Config) ->
    Test = #{name => test_name, desc => test_desc, beep => none},
    Test1 = astranaut_struct_test:from_map(Test),
    ?assertEqual(#{'__struct__' => test,
                   name => test_name, 
                   value => <<"world">>,
                   desc => test_desc,
                   enable => true}, 
                 Test1),
    ok.

test_from_map_missing_name(_Config) ->
    Test = #{desc => test_desc, beep => none},
    ?assertException(exit, {missing_enforce_keys, test, [name]}, 
                     astranaut_struct_test:from_map(Test)),
    ok.

test_from_other_record(_Config) ->
    Test2 = #test2{name = test_name, value = test_value},
    Test1 = astranaut_struct_test:to_test1(Test2),
    Test3 = astranaut_struct_test:to_test3(Test2),
    ?assertEqual(#{'__struct__' => test,
                   name => test_name,
                   value => test_value,
                   enable => true
                  },
                 Test1),
    ?assertEqual(#{'__struct__' => test3,
                   name => test_name,
                   value => test_value},
                 Test3),
    ok.

test_update_struct(_Config) ->
    Test = #{'__struct__' => test, name => bye},
    Test1 = astranaut_struct_test:update(Test),
    ?assertEqual(#{'__struct__' => test,
                   name => bye,
                   value => <<"world">>,
                   enable => true
                  }, Test1),
    ok.

test_update_missing_name(_Config) ->
    Test = #{'__struct__' => test, desc => bye},
    ?assertException(exit, {missing_enforce_keys, test, [name]}, 
                     astranaut_struct_test:update(Test)),
    ok.

test_update_fail(_Config) ->
    Test = #{'__struct__' => test2, name => bye},
    ?assertException(exit, {invalid_struct, test, Test}, astranaut_struct_test:update(Test)),
    ok.

test_parse_transform_import_contract(_Config) ->
    RecordNode = {record, 4, test, []},
    WrapperCall =
        {call, 4,
         {remote, 4,
          {atom, 4, astranaut_struct},
          {atom, 4, record}},
         [RecordNode]},
    Forms =
        [{attribute, 1, module, struct_auto_import_test},
         {attribute, 2, record, {test, []}},
         {attribute, 3, astranaut_struct, test},
         {function, 4, wrapper, 0,
          [{clause, 4, [], [], [WrapperCall]}]},
         {eof, 5}],
    Transformed = astranaut_struct_transformer:parse_transform(Forms, []),
    ?assertMatch(
       {function, 4, wrapper, 0,
        [{clause, 4, [], [], [RecordNode]}]},
       lists:keyfind(wrapper, 3, Transformed)),
    ?assert(lists:any(
              fun({attribute, _, import_macro, astranaut_struct}) -> true;
                 (_) -> false
              end, Transformed)),
    FormsWithImport =
        [{attribute, 1, module, struct_existing_import_test},
         {attribute, 2, import_macro, {astranaut_struct, [from_record]}},
         {attribute, 3, record, {test, []}},
         {attribute, 4, astranaut_struct, test},
         {eof, 5}],
    TransformedWithImport =
        astranaut_struct_transformer:parse_transform(FormsWithImport, []),
    ?assertEqual(
       1,
       length(
         [ok || {attribute, _, import_macro, Import} <- TransformedWithImport,
                import_module(Import) =:= astranaut_struct])),
    ok.

test_format_error_contract(_Config) ->
    Reasons =
        [{undefined_record, test},
         {invalid_struct_name, 42},
         {enforce_keys_not_in_struct, test, [unknown]},
         {missing_enforce_keys, test, [name]},
         {undefined_record_field, test, unknown}],
    lists:foreach(
      fun(Reason) ->
              ?assert(io_lib:deep_char_list(
                        astranaut_struct_transformer:format_error(Reason)))
      end, Reasons),
    Text = "already formatted",
    ?assertEqual(Text, astranaut_struct_transformer:format_error(Text)),
    ?assert(io_lib:deep_char_list(
              astranaut_struct_transformer:format_error(unexpected_reason))),
    ok.

test_compile_enforce_fail(Config) ->
    Forms = astranaut_test_lib:test_module_forms(astranaut_struct_fail_0, Config),
    Baseline = astranaut_test_lib:get_baseline(yep, Forms),
    Return = astranaut_test_lib:compile_test_forms(Forms),
    ErrorStruct = astranaut_return:run_error(Return),
    {[{File, [Error]}], []} = astranaut_test_lib:realize_with_baseline(Baseline, ErrorStruct),
    [{LineAbs, {test, _Opts}}] = attributes_with_line(astranaut_struct, Forms),
    Line = normalize_line(LineAbs) - Baseline,
    ?assertEqual("astranaut_struct_fail_0.erl", filename:basename(File)),
    ?assertEqual(Line, error_line(Error)),
    ?assertEqual(astranaut_struct_transformer, error_formatter(Error)),
    ?assertEqual({enforce_keys_not_in_struct,test,[desc]}, error_reason(Error)),
    astranaut_test_lib:assert_formatted_messages([Error]),
    ok.

test_compile_non_record_fail(Config) ->
    Forms = astranaut_test_lib:test_module_forms(astranaut_struct_fail_1, Config),
    Baseline = astranaut_test_lib:get_baseline(yep, Forms),
    Return = astranaut_test_lib:compile_test_forms(Forms),
    ErrorStruct = astranaut_return:run_error(Return),
    {[{File, [Error]}], []} = astranaut_test_lib:realize_with_baseline(Baseline, ErrorStruct),
    [{LineAbs, other_test}] = attributes_with_line(astranaut_struct, Forms),
    Line = normalize_line(LineAbs) - Baseline,
    ?assertEqual("test_record_1.hrl", filename:basename(File)),
    ?assertEqual(Line, error_line(Error)),
    ?assertEqual(astranaut_struct_transformer, error_formatter(Error)),
    ?assertEqual({undefined_record,other_test}, error_reason(Error)),
    astranaut_test_lib:assert_formatted_messages([Error]),
    ok.

test_compile_unknown_fields_fail(Config) ->
    assert_compile_reasons(
      astranaut_struct_unknown_fields_fail, Config,
      [{undefined_record_field, test, unknown},
       {undefined_record_field, test, unknown},
       {undefined_record_field, test, unknown},
       {undefined_record_field, test, unknown}]).

test_compile_missing_enforce_key_fail(Config) ->
    assert_compile_reasons(
      astranaut_struct_missing_enforce_fail, Config,
      [{missing_enforce_keys, test, [name]}]).

test_compile_invalid_struct_name_fail(Config) ->
    assert_compile_reasons(
      astranaut_struct_invalid_name_fail, Config,
      [{invalid_struct_name, 42}]).

assert_compile_reasons(Module, Config, ExpectedReasons) ->
    Forms = astranaut_test_lib:test_module_forms(Module, Config),
    Return = astranaut_test_lib:compile_test_forms(Forms),
    ErrorStruct = astranaut_return:run_error(Return),
    {FileErrors, []} = astranaut_test_lib:realize_with_baseline(1, ErrorStruct),
    Errors = lists:append([Errors0 || {_File, Errors0} <- FileErrors]),
    ?assertEqual(ExpectedReasons, [error_reason(Error) || Error <- Errors]),
    ?assert(lists:all(
              fun(Error) ->
                      error_formatter(Error) =:= astranaut_struct_transformer
              end, Errors)),
    astranaut_test_lib:assert_formatted_messages(Errors),
    ok.

import_module({Module, _Spec}) ->
    Module;
import_module(Module) ->
    Module.

attributes_with_line(Attribute, Forms) ->
    lists:reverse(
      lists:foldl(
        fun({attribute, Line, Attr, Value}, Acc) when Attr =:= Attribute ->
                [{Line, Value}|Acc];
           (_Node, Acc) ->
                Acc
        end, [], Forms)).

error_line({{Line, _Column}, _Formatter, _Reason}) ->
    normalize_line(Line);
error_line({Line, _Formatter, _Reason}) ->
    normalize_line(Line).

error_formatter({_Line, Formatter, _Reason}) ->
    Formatter.

error_reason({_Line, _Formatter, Reason}) ->
    Reason.

normalize_line({L, _C}) when is_integer(L) ->
    L;
normalize_line(L) ->
    L.
