%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2021, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 16 Jun 2021 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_quote_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

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
    TestModules = [quote_example],
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
    [test_literal_atom, test_literal_integer, test_literal_tuple,
     test_pattern_match, test_pattern_function_1, test_pattern_function_2,
     test_pattern_case_1, test_pattern_case_2, test_pattern_case_3,
     test_unquote, test_binding, test_atom_binding,
     test_dynamic_binding, test_dynamic_binding_pattern,
     test_unquote_splicing_1, test_unquote_splicing_2,
     test_user_type, test_system_type, test_remote_type, test_record, test_spec, test_guard].
%%--------------------------------------------------------------------
%% @spec TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% Comment = term()
%% @end
%%--------------------------------------------------------------------
test_literal_atom(_Config) ->
    Atom = quote_example:atom(),
    Ast = astranaut_lib:abstract_form(ok),
    ?assertEqual(Ast, Atom),
    ok.

test_literal_integer(_Config) ->
    Integer = quote_example:integer(),
    Ast = astranaut_lib:abstract_form(10),
    ?assertEqual(Ast, Integer),
    ok.

test_literal_tuple(_Config) ->
    Tuple = quote_example:tuple(),
    Ast = astranaut_lib:abstract_form({hello, world}),
    ?assertEqual(Ast, Tuple),
    ok.

test_pattern_match(_Config) ->
    Ast1 = merl:quote(0, "hello(world, foo, bar)"),
    Pattern = quote_example:match_pattern(Ast1),
    Ast2 = astranaut_lib:abstract_form({hello, world, foo, bar}),
    ?assertEqual(Ast2, Pattern),
    ok.

test_pattern_function_1(_Config) ->
    Ast1 = astranaut_lib:abstract_form({hello, world}),
    Pattern = quote_example:function_pattern(Ast1),
    Ast2 = astranaut_lib:abstract_form({ok, {hello2, world, world, {hello, world}}}),
    ?assertEqual(Ast2, Pattern),
    ok.

test_pattern_function_2(_Config) ->
    Ast1 = astranaut_lib:abstract_form({foo, bar}),
    Pattern = quote_example:function_pattern(Ast1),
    Ast2 = astranaut_lib:abstract_form({error, {foo, bar}}),
    ?assertEqual(Ast2, Pattern),
    ok.

test_pattern_case_1(_Config) ->
    Ast1 = merl:quote(0, "F(10)"),
    Pattern = quote_example:case_pattern(Ast1),
    Ast2 = merl:quote(0, "{ok, F(10 + 1)}"),
    ?assertEqual(Ast2, Pattern),
    ok.

test_pattern_case_2(_Config) ->
    Ast1 = merl:quote(0, "hello:world(foo, bar)"),
    Pattern = quote_example:case_pattern(Ast1),
    Ast2 = astranaut_lib:abstract_form({ok, {hello, world, foo, bar}}),
    ?assertEqual(Ast2, Pattern),
    ok.

test_pattern_case_3(_Config) ->
    Ast1 = merl:quote(0, "task"),
    Pattern = quote_example:case_pattern(Ast1),
    Ast2 = astranaut_lib:abstract_form({error, task}),
    ?assertEqual(Ast2, Pattern),
    ok.

test_unquote(_Config) ->
    Atom = quote_example:atom(),
    OkAtom = quote_example:unquote(Atom),
    Ast = astranaut_lib:abstract_form({ok, ok}),
    ?assertEqual(Ast, OkAtom),
    ok.

test_binding(_Config) ->
    Atom = quote_example:atom(),
    OkAtom = quote_example:binding(Atom),
    Ast = astranaut_lib:abstract_form({ok, ok}),
    ?assertEqual(Ast, OkAtom),
    ok.

test_atom_binding(_Config) ->
    OkAtom = quote_example:atom_binding(hello),
    Ast = astranaut_lib:abstract_form({ok, hello}),
    ?assertEqual(Ast, OkAtom),
    ok.

test_dynamic_binding(_Config) ->
    Ok = quote_example:dynamic_binding({hello, 10, 10.0}),
    Ast = astranaut_lib:abstract_form({ok, {hello, 10, 10.0}}),
    ?assertEqual(Ast, Ok),
    ok.

test_dynamic_binding_pattern(_Config) ->
    World = quote_example:dynamic_binding_pattern(),
    ?assertEqual(world, World),
    ok.

test_unquote_splicing_1(_Config) ->
    Hello = quote_example:atom(hello),
    World = quote_example:atom(world),
    HelloWorld = quote_example:unquote_splicing_1(Hello, World),
    Ast = astranaut_lib:abstract_form({ok, {hello, hello, world, world}}),
    ?assertEqual(Ast, HelloWorld),
    ok.

test_unquote_splicing_2(_Config) ->
    Hello = quote_example:atom(hello),
    World = quote_example:atom(world),
    HelloWorld = quote_example:unquote_splicing_2(Hello, World),
    Ast = astranaut_lib:abstract_form({ok, [hello, hello, world, world]}),
    ?assertEqual(Ast, HelloWorld),
    ok.

test_user_type(_Config) ->
    Type = quote_example:user_type(hello, world),
    Ast = merl:quote(0, "-type hello() :: world()."),
    ?assertEqual(Ast, Type),
    ok.

test_system_type(_Config) ->
    Type = quote_example:system_type(hello, atom),
    Ast = merl:quote(0, "-type hello() :: atom()."),
    ?assertEqual(Ast, Type),
    ok.

test_remote_type(_Config) ->
    World = astranaut_lib:abstract_form(world),
    Type = quote_example:remote_type(hello, hello, World),
    Ast = merl:quote(0, "-type hello() :: hello:world()."),
    ?assertEqual(Ast, Type),
    ok.

test_record(_Config) ->
    Record = quote_example:record(hello_world),
    Ast = merl:quote(0, "-record(hello_world, {id, hello, world})."),
    ?assertEqual(Ast, Record),
    ok.

test_spec(_Config) ->
    Spec = quote_example:spec(hello),
    Ast = merl:quote(0, "-spec hello(atom()) -> atom()."),
    ?assertEqual(Ast, Spec),
    ok.


test_guard(_Config) ->
    Var = merl:quote(0, "A"),
    Guard = merl:quote(0, "A == hello"),
    TestGuard = quote_example:guard(Var, Guard),
    Ast = merl:quote(
            ["case A of",
             "  A when A == hello ->",
             "    A;",
             "  _ ->",
             "    {error, not_match}"
             "end"]),
    Ast1 = astranaut_lib:replace_line(Ast, 0),
    ?assertEqual(Ast1, TestGuard),
    ok.
