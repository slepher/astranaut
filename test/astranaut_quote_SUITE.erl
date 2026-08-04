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
-include("otp_vsn.hrl").

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
     test_pattern_match, test_pattern_function_1, test_pattern_function_2, test_pattern_function_3,
     test_pattern_case_1, test_pattern_case_2, test_pattern_case_3,
     test_function_expression, test_function_expression_error,
     test_named_function_expression_1, test_named_function_expression_2,
     test_pos_1, test_pos_2,
     test_unquote, test_unquote_map, test_unquote_map_match, test_unquote_map_match_list,
     test_unquote_record, test_unquote_record_match, test_unquote_record_match_list,
     test_binding, test_atom_binding,
     test_dynamic_binding, test_dynamic_binding_pattern,
     test_unquote_splicing_1, test_unquote_splicing_2, test_unquote_splicing_map,
     test_type, test_type_atom, test_type_map, test_type_tuple, test_exp_type, test_remote_type,
     test_record, test_native_records, test_native_record_attributes,
     test_spec, test_callback, test_opaque,
     test_empty_quote_code, test_empty_quote_type_code,
     test_quote_code_options_must_be_last, test_invalid_binding_values,
     test_quote_public_helpers, test_quote_code_options_last,
     test_binding_warning_format,
     test_quoted_invalid_unquote_splicing_warning,
     test_quoted_invalid_unquote_splicing_binding_warning,
     test_quoted_type_binding_warning,
     test_quoted_pattern_splicing_tail_warning,
     test_parse_transform_tuple_pos_warning,
     test_parse_transform_literal_name_binding_warning,
     test_quoted_tuple_pos_warning,
     test_quoted_literal_name_binding_warning,
     test_codec,
     test_default_context,
     test_explicit_context,
     test_no_context,
     test_no_context_false,
     test_context_no_context_conflict,
     test_invalid_context,
     test_empty_context,
     test_empty_default_context,
     test_context_undefined,
     test_low_level_option_validation,
     test_context_option_forms,
     test_wildcard,
     test_low_level_no_context,
     test_no_context_named_fun,
     test_guard].
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
    Ast1 = astranaut_lib:abstract_form({hello2, world}),
    Pattern = quote_example:function_pattern(Ast1),
    Ast2 = astranaut_lib:abstract_form({ok, {hello3, world, world, {hello2, world}}}),
    ?assertEqual(Ast2, Pattern),
    ok.

test_pattern_function_3(_Config) ->
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

test_function_expression(_Config) ->
    Expression = quote_example:function_expression(send),
    Ast = merl:quote(0, "fun send/3"),
    ?assertEqual(Ast, Expression),
    ok.

test_function_expression_error(_Config) ->
    ?assertException(error, {unexpected_type_of_var, _, atom_value, 233}, quote_example:function_expression(233)),
    ok.

test_named_function_expression_1(_Config) ->
    Expression = quote_example:named_function_expression_1('H'),
    Ast = merl:quote(0, "fun H(0) -> 0; H(N@astranaut_quote@quote_example) -> H(N@astranaut_quote@quote_example - 1) + N@astranaut_quote@quote_example end"),
    ?assertEqual(Ast, Expression),
    ok.

test_named_function_expression_2(_Config) ->
    Expression = quote_example:named_function_expression_2(),
    Ast = merl:quote(0, "fun Name@astranaut_quote@quote_example(0) -> 0; Name@astranaut_quote@quote_example(N@astranaut_quote@quote_example) -> Name@astranaut_quote@quote_example(N@astranaut_quote@quote_example - 1) + N@astranaut_quote@quote_example end"),
    ?assertEqual(Ast, Expression),
    ok.

test_pos_1(_Config) ->
    Ast = {atom, {10, 11}, world},
    HelloWorld = quote_example:pos_1(Ast),
    ?assertEqual({tuple, {10, 11}, [{atom, {10, 11}, hello}, {atom, {10, 11}, world}]}, HelloWorld),
    ok.

test_pos_2(_Config) ->
    Ast = {atom, {10, 11}, world},
    HelloWorld = quote_example:pos_2(Ast),
    ?assertEqual({tuple, 12, [{atom, 12, ok}, {tuple, 13, [{atom, 13, hello}, {atom, {10, 11}, world}]}]}, HelloWorld),
    ok.

test_unquote(_Config) ->
    Atom = quote_example:atom(),
    OkAtom = quote_example:unquote(Atom),
    Ast = astranaut_lib:abstract_form({ok, ok}),
    ?assertEqual(Ast, OkAtom),
    ok.

test_unquote_map(_Config) ->
    Ast1 = merl:quote(0, "#{a => 1}"),
    [Ast2] = erl_syntax:map_expr_fields(Ast1),
    Map = quote_example:unquote_map(Ast2),
    Ast3 = merl:quote(0, "{ok, #{a => 1}}"),
    ?assertEqual(Ast3, Map),
    ok.

test_unquote_map_match(_Config) ->
    Ast1 = merl:quote(0, "#{a => 1}"),
    [Ast2] = erl_syntax:map_expr_fields(Ast1),
    Matched = quote_example:unquote_map_match(Ast1),
    ?assertEqual(Ast2, Matched),
    ok.

test_unquote_map_match_list(_Config) ->
    Ast1 = merl:quote(0, "#{a => 1, b => 2, c => 3}"),
    Ast2 = merl:quote(0, "#{b => 2, c => 3}"),
    Ast3 = erl_syntax:map_expr_fields(Ast2),
    Map = quote_example:unquote_map_match_list(Ast1),
    ?assertEqual(Ast3, Map),
    ok.

test_unquote_record(_Config) ->
    Ast1 = merl:quote(0, "#test{a = 1}"),
    {record, _, _Name, [Ast2]} = Ast1,
    Rec = quote_example:unquote_record(Ast2),
    Ast3 = merl:quote(0, "{ok, #test{a = 1}}"),
    ?assertEqual(Ast3, Rec),
    ok.

test_unquote_record_match(_Config) ->
    Ast1 = merl:quote(0, "#test{a = 1}"),
    {record, _, _Name, [Ast2]} = Ast1,
    Matched = quote_example:unquote_record_match(Ast1),
    ?assertEqual(Ast2, Matched),
    ok.

test_unquote_record_match_list(_Config) ->
    Ast1 = merl:quote(0, "#test{a = 1, b = 2, c = 3}"),
    Ast2 = merl:quote(0, "#test{b = 2, c = 3}"),
    {record, _, _Name, Ast3} = Ast2,
    RecordFields = quote_example:unquote_record_match_list(Ast1),
    ?assertEqual(Ast3, RecordFields),
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

test_unquote_splicing_map(_Config) ->
    Ast1 = erl_syntax:map_expr_fields(merl:quote(0, "#{a => 0, b => 1}")),
    Ast2 = erl_syntax:map_expr_fields(merl:quote(0, "#{c => 2, d => 3}")),
    HelloWorld = quote_example:unquote_splicing_map(Ast1, Ast2),
    Ast3 = merl:quote(0, "{ok, #{hello => 1, a => 0, b => 1, c => 2, d => 3, world => 2}}"),
    ?assertEqual(Ast3, HelloWorld),
    ok.

test_type(_Config) ->
    Type = quote_example:type(hello, world),
    Ast = merl:quote(0, "-type hello() :: world()."),
    ?assertEqual(Ast, Type),
    ok.

test_type_atom(_Config) ->
    Type = quote_example:type(hello, atom),
    Ast = merl:quote(0, "-type hello() :: atom()."),
    ?assertEqual(Ast, Type),
    ok.

test_type_tuple(_Config) ->
    Type = quote_example:type(hello, tuple),
    Ast = merl:quote(0, "-type hello() :: tuple()."),
    ?assertEqual(Ast, Type),
    ok.

test_type_map(_Config) ->
    Type = quote_example:type(hello, map),
    Ast = merl:quote(0, "-type hello() :: map()."),
    ?assertEqual(Ast, Type),
    ok.

test_exp_type(_Config) ->
    Type = quote_example:exp_type(hello),
    Ast1 = merl:quote(0, "-type hello() :: hello:world()."),
    ?assertEqual(Ast1, Type).

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

-if(?ASTRANAUT_OTP_VSN_GE(29)).
test_native_records(_Config) ->
    [QualifiedCreate, QualifiedUpdate, AnonymousPattern,
     AnonymousUpdate, QualifiedAccess, AnonymousAccess] =
        quote_example:native_records(),
    ?assertMatch({record, _, {mod, rec},
                  [{record_field, _, {atom, _, x}, {integer, _, 1}}]},
                 QualifiedCreate),
    ?assertMatch({record, _, {var, _, _}, {mod, rec},
                  [{record_field, _, {atom, _, x}, {integer, _, 2}}]},
                 QualifiedUpdate),
    ?assertMatch({record, _, [],
                  [{record_field, _, {atom, _, x}, {var, _, _}}]},
                 AnonymousPattern),
    ?assertMatch({record, _, {var, _, _}, [],
                  [{record_field, _, {atom, _, x}, {integer, _, 3}}]},
                 AnonymousUpdate),
    ?assertMatch({record_field, _, {var, _, _}, {mod, rec}, {atom, _, x}},
                 QualifiedAccess),
    ?assertMatch({record_field, _, {var, _, _}, [], {atom, _, x}},
                 AnonymousAccess),
    ok.

test_native_record_attributes(_Config) ->
    [NativeRecord, ExportRecord, ImportRecord] =
        quote_example:native_record_attributes(),
    ?assertMatch({attribute, _, native_record,
                  {rec, [{record_field, _, {atom, _, x}}]}},
                 NativeRecord),
    ?assertMatch({attribute, _, export_record, [rec]}, ExportRecord),
    ?assertMatch({attribute, _, import_record, {mod, [rec]}}, ImportRecord),
    ok.
-else.
test_native_records(_Config) ->
    ok.

test_native_record_attributes(_Config) ->
    ok.
-endif.

test_spec(_Config) ->
    Spec = quote_example:spec(hello, map, world),
    Ast = merl:quote(0, "-spec hello(map()) -> world()."),
    ?assertEqual(Ast, Spec),
    ok.

test_callback(_Config) ->
    Callback = quote_example:callback(hello),
    Ast = merl:quote(0, "-callback hello(atom()) -> atom()."),
    ?assertEqual(Ast, Callback),
    ok.

test_opaque(_Config) ->
    Opaque = quote_example:opaque(hello),
    Ast = merl:quote(0, "-opaque hello() :: atom()."),
    ?assertEqual(Ast, Opaque),
    ok.

test_empty_quote_code(_Config) ->
    assert_invalid_quote("quote_code()", quote_code).

test_empty_quote_type_code(_Config) ->
    assert_invalid_quote("quote_type_code()", quote_type_code).

test_quote_code_options_must_be_last(_Config) ->
    assert_invalid_quote(
      "quote_code(#{code_pos => true}, \"ok\")", quote_code),
    assert_invalid_quote(
      "quote_code(\"first\", #{code_pos => true}, \"last\")", quote_code).

test_invalid_binding_values(_Config) ->
    Cases =
        [{integer, not_integer},
         {float, not_float},
         {string, [not_a_character]},
         {string, <<255>>},
         {atom, <<255>>},
         {var, <<255>>},
         {atom_value, <<255>>}],
    lists:foreach(
      fun({Type, Value}) ->
              Opts = #{type => Type, pos => 0, name => 'Value'},
              ?assertError(
                 {unexpected_type_of_var, 'Value', Type, Value},
                 astranaut_quote:bind_var(Value, Opts))
      end, Cases).

test_quote_public_helpers(_Config) ->
    A = {atom, 10, a},
    B = {atom, 11, b},
    C = {atom, 12, c},
    Cons = {cons, 20, A, {cons, 21, B, {nil, 22}}},
    ?assertEqual([A, B], astranaut_quote:flattencons(Cons)),
    ?assertEqual([A, B], astranaut_quote:flattencons([A, B])),
    ?assertEqual([A, B], astranaut_quote:flattencons(Cons, [])),
    ?assertEqual([A, B, C], astranaut_quote:flattencons(Cons, [C])),
    ?assertEqual(
       {cons, 0, A, {cons, 0, B, {cons, 30, C, {nil, 31}}}},
       astranaut_quote:mergecons([A, B], {cons, 30, C, {nil, 31}})),
    ?assertEqual(
       {cons, 20, A, {cons, 21, B, {cons, 30, C, {nil, 31}}}},
       astranaut_quote:mergecons(Cons, {cons, 30, C, {nil, 31}})),

    ?assertEqual(value, astranaut_quote:bind_var(value, #{type => value})),
    ?assertEqual({float, 40, 3.0},
                 astranaut_quote:bind_var(3, #{type => float, pos => 40})),
    ?assertEqual({float, 40, 3.5},
                 astranaut_quote:bind_var(3.5, #{type => float, pos => 40})),
    ?assertEqual({string, 41, "hello"},
                 astranaut_quote:bind_var(<<"hello">>, #{type => string, pos => 41})),
    ?assertEqual({atom, 42, hello},
                 astranaut_quote:bind_var("hello", #{type => atom, pos => 42})),
    ?assertEqual({atom, 42, hello},
                 astranaut_quote:bind_var(<<"hello">>, #{type => atom, pos => 42})),
    ?assertError(
       {unexpected_type_of_var, atom_name, atom, 10},
       astranaut_quote:bind_var(
         10, #{type => atom, pos => 42, name => atom_name})),

    Quoted = {atom, 50, ok},
    ?assertEqual(Quoted, astranaut_quote:validate_pos(Quoted, 50)),
    ?assertEqual(Quoted, astranaut_quote:validate_pos(Quoted, {50, 3})),
    ?assertError({invalid_pos_value, invalid},
                 astranaut_quote:validate_pos(Quoted, invalid)),

    {attribute, 0, type, {dummy, ExpectedType, []}} =
        merl:quote(0, "-type dummy() :: #{atom() => integer()}."),
    ?assertEqual(
       ExpectedType,
       astranaut_quote:quote_type_code("#{atom() => integer()}")),
    {value, QuotedValue, _Bindings} =
        erl_eval:expr(
          astranaut_quote:quoted(Quoted, 77), erl_eval:new_bindings()),
    ?assertEqual({atom, 77, ok}, QuotedValue),
    ok.

test_quote_code_options_last(_Config) ->
    Forms =
        merl:quote(
          ["-file(\"quote_options_last_test.erl\", 1).",
           "-module(quote_options_last_test).",
           "-export([static/0, dynamic/1]).",
           "static() -> quote_code(\"ok\", #{pos => 77}).",
           "dynamic(Pos) -> quote_code(\"ok\", #{pos => Pos})."])
        ++ [{eof, 6}],
    Transformed = astranaut_quote:parse_transform(Forms, []),
    {function, _, static, 0,
     [{clause, _, [], [], [StaticExpression]}]} =
        lists:keyfind(static, 3, Transformed),
    {value, StaticValue, _StaticBindings} =
        erl_eval:expr(
          StaticExpression, erl_eval:new_bindings()),
    ?assertEqual({atom, 77, ok}, StaticValue),
    {function, _, dynamic, 1,
     [{clause, _, [{var, _, 'Pos'}], [],
       [DynamicExpression]}]} =
        lists:keyfind(dynamic, 3, Transformed),
    DynamicBindings =
        erl_eval:add_binding(
          'Pos', 88, erl_eval:new_bindings()),
    {value, DynamicValue, _DynamicBindings} =
        erl_eval:expr(DynamicExpression, DynamicBindings),
    ?assertEqual({atom, 88, ok}, DynamicValue),
    ok.

test_binding_warning_format(_Config) ->
    Forms = merl:quote(
              ["-file(\"quote_binding_warning_test.erl\", 1).",
               "-module(quote_binding_warning_test).",
               "-export([run/2]).",
               "run(Module, Type) ->",
               "  quote_code(\"-type foo() :: '_V@Module':'_A@Type'().\")."])
        ++ [{eof, 6}],
    {warning, _QuotedForms, [{_File, Warnings}]} =
        astranaut_quote:parse_transform(Forms, []),
    ?assert(
       lists:any(
         fun({_Pos, astranaut_quote,
              {only_bindings_supported, ["A", ""], 'Module', '_V@Module'}}) ->
                 true;
            (_) ->
                 false
         end, Warnings)),
    astranaut_test_lib:assert_formatted_messages(Warnings),
    ok.

test_quoted_invalid_unquote_splicing_warning(_Config) ->
    Node = merl:quote(10, "unquote_splicing(A)"),
    {warning, _Return, [Warning]} = astranaut_quote:quoted(Node),
    ?assertMatch(
       {10, astranaut_quote,
        {invalid_unquote_splicing, {var, 10, 'A'}}},
       Warning),
    astranaut_test_lib:assert_formatted_messages([Warning]),
    ok.

test_quoted_invalid_unquote_splicing_binding_warning(_Config) ->
    Node = merl:quote(20, "_L@As"),
    {warning, _Return, [Warning]} = astranaut_quote:quoted(Node),
    ?assertEqual(
       {20, astranaut_quote,
        {invalid_unquote_splicing_binding, 'As'}},
       Warning),
    astranaut_test_lib:assert_formatted_messages([Warning]),
    ok.

test_quoted_type_binding_warning(_Config) ->
    Node =
        {attribute, 30, type,
         {foo,
          {remote_type, 31,
           [{atom, 32, '_V@Module'}, {atom, 33, '_A@Type'}, []]},
          []}},
    {warning, _Return, [Warning]} = astranaut_quote:quoted(Node),
    ?assertEqual(
       {32, astranaut_quote,
        {only_bindings_supported, ["A", ""], 'Module', '_V@Module'}},
       Warning),
    astranaut_test_lib:assert_formatted_messages([Warning]),
    ok.

test_quoted_pattern_splicing_tail_warning(_Config) ->
    Node = merl:quote(40, "{unquote_splicing = Ast, tail}"),
    {warning, _Return, [Warning]} =
        astranaut_quote:quoted(Node, #{quote_type => pattern}),
    ?assertEqual(
       {40, astranaut_quote,
        {unquote_splicing_pattern_non_empty_tail, [{atom, 40, tail}]}},
       Warning),
    astranaut_test_lib:assert_formatted_messages([Warning]),
    ok.

test_parse_transform_tuple_pos_warning(_Config) ->
    Forms =
        [{attribute, 1, file, {"tuple_warning.erl", 1}},
         {attribute, 2, module, tuple_warning},
         {attribute, 3, export, [{run, 0}]},
         {function, 60, run, 0,
          [{clause, 60, [], [],
            [{call, 60, {atom, 60, quote}, [{foo, bar}]}]}]},
         {eof, 61}],
    {warning, _QuotedForms, [{_File, [Warning]}]} =
        astranaut_quote:parse_transform(Forms, []),
    ?assertEqual(
       {60, astranaut_quote,
        {could_not_get_tuple_pos_value, {foo, bar}}},
       Warning),
    astranaut_test_lib:assert_formatted_messages([Warning]),
    ok.

test_parse_transform_literal_name_binding_warning(_Config) ->
    Forms = merl:quote(
              ["-file(\"binding_warning.erl\", 1).",
               "-module(binding_warning).",
               "-export([run/1]).",
               "run(Name) -> quote(fun '_V@Name'/0)."])
        ++ [{eof, 5}],
    {warning, _QuotedForms, [{_File, [Warning]}]} =
        astranaut_quote:parse_transform(Forms, []),
    ?assertEqual(
       {4, astranaut_quote,
        {only_bindings_supported, ["A"], 'Name', '_V@Name'}},
       Warning),
    astranaut_test_lib:assert_formatted_messages([Warning]),
    ok.

test_quoted_tuple_pos_warning(_Config) ->
    {warning, _Return, [Warning]} = astranaut_quote:quoted({foo, bar}),
    ?assertEqual(
       {bar, astranaut_quote,
        {could_not_get_tuple_pos_value, {foo, bar}}},
       Warning),
    astranaut_test_lib:assert_formatted_messages([Warning]),
    ok.

test_quoted_literal_name_binding_warning(_Config) ->
    Node = merl:quote(70, "fun '_V@Name'/0"),
    {warning, _Return, [Warning]} = astranaut_quote:quoted(Node),
    ?assertEqual(
       {70, astranaut_quote,
        {only_bindings_supported, ["A"], 'Name', '_V@Name'}},
       Warning),
    astranaut_test_lib:assert_formatted_messages([Warning]),
    ok.

assert_invalid_quote(CallCode, Function) ->
    Forms = merl:quote(
              ["-file(\"empty_quote_test.erl\", 1).",
               "-module(empty_quote_test).",
               "-export([run/0]).",
               "run() -> " ++ CallCode ++ "."])
        ++ [{eof, 5}],
    ?assertMatch(
       {error,
         [{_, [{_, astranaut_quote,
               {invalid_quote, {call, _, {atom, _, Function}, _}}}]}],
        []},
       astranaut_quote:parse_transform(Forms, [])),
    ok.


test_guard(_Config) ->
    Var = merl:quote(0, "A"),
    Guard = merl:quote(0, "A =:= hello"),
    TestGuard = quote_example:guard(Var, Guard),
    Ast = merl:quote(
            ["case A of",
             "  A when A =:= hello ->",
             "    A;",
             "  _ ->",
             "    {error, not_match}"
             "end"]),
    Ast1 = astranaut_lib:replace_pos(Ast, 0),
    ?assertEqual(Ast1, TestGuard),
    ok.

test_codec(_Config) ->
    ?assertEqual(
       {template, a, example_macro},
       astranaut_quote:decode_quote_variable(
         astranaut_quote:encode_quote_variable(a, example_macro))),
    ?assertEqual(
       {expanded, a, example_macro, 1},
       astranaut_quote:decode_quote_variable(
         astranaut_quote:encode_quote_variable(a, example_macro, 1))),
    ?assertEqual(
       'A%40part@astranaut_quote@macro%40ctx',
       astranaut_quote:encode_quote_variable('A@part', 'macro@ctx')),
    ?assertEqual(
       {template, 'A@part', 'macro@ctx'},
       astranaut_quote:decode_quote_variable(
         astranaut_quote:encode_quote_variable('A@part', 'macro@ctx'))),
    ?assertEqual(
       {template, 'A%part', 'macro%ctx'},
       astranaut_quote:decode_quote_variable(
         astranaut_quote:encode_quote_variable('A%part', 'macro%ctx'))),
    ?assertEqual(not_quote_variable,
                 astranaut_quote:decode_quote_variable(a)),
    ?assertEqual(not_quote_variable,
                 astranaut_quote:decode_quote_variable('a@foo')),
    ?assertEqual(not_quote_variable,
                 astranaut_quote:decode_quote_variable('a@astranaut_quote@')),
    ?assertEqual(not_quote_variable,
                 astranaut_quote:decode_quote_variable('a@astranaut_quote@ctx@x')),
    ?assertEqual(not_quote_variable,
                 astranaut_quote:decode_quote_variable('a@astranaut_quote@ctx@0')),
    ?assertEqual(not_quote_variable,
                 astranaut_quote:decode_quote_variable('a@astranaut_quote@ctx@1@2')),
    ?assertEqual(not_quote_variable,
                 astranaut_quote:decode_quote_variable('_')),
    ?assertEqual(not_quote_variable,
                 astranaut_quote:decode_quote_variable(123)),
    ?assertEqual(
       {expanded, 'A@part%x', 'macro%ctx@y', 1},
       astranaut_quote:decode_quote_variable(
         astranaut_quote:encode_quote_variable(
           'A@part%x', 'macro%ctx@y', 1))),
    ?assertError({invalid_quote_counter, 0},
                 astranaut_quote:encode_quote_variable(a, ctx, 0)),
    ?assertError({invalid_quote_counter, -1},
                 astranaut_quote:encode_quote_variable(a, ctx, -1)),
    ?assertError({invalid_quote_counter, not_an_integer},
                 astranaut_quote:encode_quote_variable(a, ctx, not_an_integer)),
    ?assertError({invalid_quote_variable_name, 123},
                 astranaut_quote:encode_quote_variable(123, ctx)),
    ?assertError({invalid_quote_context, 123},
                 astranaut_quote:encode_quote_variable(a, 123)),
    ok.

test_context_option_forms(_Config) ->
    assert_quote_var_context("[{context, ctx_list}]", 'A@astranaut_quote@ctx_list'),
    assert_quote_var_context("[no_context]", 'A'),
    assert_quote_var_context("no_context", 'A'),
    assert_quote_var_context("#{no_context => false}", 'A@astranaut_quote@ctx_option_test'),
    ok.

assert_quote_var_context(OptionsCode, ExpectedName) ->
    Forms = merl:quote(
              ["-file(\"ctx_option_test.erl\", 1).",
               "-module(ctx_option_test).",
               "-export([run/0]).",
               "run() -> quote(A, " ++ OptionsCode ++ ")."])
        ++ [{eof, 5}],
    Transformed = astranaut_quote:parse_transform(Forms, []),
    {function, _, run, 0, [{clause, _, [], [], [Expression]}]} =
        lists:keyfind(run, 3, Transformed),
    {value, {var, _, ExpectedName}, _} =
        erl_eval:expr(Expression, erl_eval:new_bindings()),
    ok.

test_default_context(_Config) ->
    Forms = merl:quote(
              ["-file(\"default_context_test.erl\", 1).",
               "-module(default_context_test).",
               "-export([run/0]).",
               "run() -> quote(A)."])
        ++ [{eof, 5}],
    Transformed = astranaut_quote:parse_transform(Forms, []),
    {function, _, run, 0, [{clause, _, [], [], [Expression]}]} =
        lists:keyfind(run, 3, Transformed),
    {value, {var, _, 'A@astranaut_quote@default_context_test'}, _} =
        erl_eval:expr(Expression, erl_eval:new_bindings()),
    ok.

test_explicit_context(_Config) ->
    Forms = merl:quote(
              ["-file(\"explicit_context_test.erl\", 1).",
               "-module(explicit_context_test).",
               "-export([run/0]).",
               "run() -> quote(A, #{context => my_ctx})."])
        ++ [{eof, 5}],
    Transformed = astranaut_quote:parse_transform(Forms, []),
    {function, _, run, 0, [{clause, _, [], [], [Expression]}]} =
        lists:keyfind(run, 3, Transformed),
    {value, {var, _, 'A@astranaut_quote@my_ctx'}, _} =
        erl_eval:expr(Expression, erl_eval:new_bindings()),
    ok.

test_no_context(_Config) ->
    Forms = merl:quote(
              ["-file(\"no_context_test.erl\", 1).",
               "-module(no_context_test).",
               "-export([run/0]).",
               "run() -> quote(A, no_context)."])
        ++ [{eof, 5}],
    Transformed = astranaut_quote:parse_transform(Forms, []),
    {function, _, run, 0, [{clause, _, [], [], [Expression]}]} =
        lists:keyfind(run, 3, Transformed),
    {value, {var, _, 'A'}, _} =
        erl_eval:expr(Expression, erl_eval:new_bindings()),
    ok.

test_no_context_false(_Config) ->
    Forms = merl:quote(
              ["-file(\"no_context_false_test.erl\", 1).",
               "-module(no_context_false_test).",
               "-export([run/0]).",
               "run() -> quote(A, #{no_context => false})."])
        ++ [{eof, 5}],
    Transformed = astranaut_quote:parse_transform(Forms, []),
    {function, _, run, 0, [{clause, _, [], [], [Expression]}]} =
        lists:keyfind(run, 3, Transformed),
    {value, {var, _, 'A@astranaut_quote@no_context_false_test'}, _} =
        erl_eval:expr(Expression, erl_eval:new_bindings()),
    ok.

test_context_no_context_conflict(_Config) ->
    Forms = merl:quote(
              ["-file(\"context_conflict_test.erl\", 1).",
               "-module(context_conflict_test).",
               "-export([run/0]).",
               "run() -> quote(A, #{context => my_ctx, no_context => true})."])
        ++ [{eof, 5}],
    ?assertMatch(
       {error, [{_, [{_, astranaut_quote,
                      {conflicting_quote_context_options, my_ctx, no_context}}]}], []},
       astranaut_quote:parse_transform(Forms, [])),
    ok.

test_invalid_context(_Config) ->
    Forms = merl:quote(
              ["-file(\"invalid_context_test.erl\", 1).",
               "-module(invalid_context_test).",
               "-export([run/1]).",
               "run(Ctx) -> quote(A, #{context => Ctx})."])
        ++ [{eof, 5}],
    ?assertMatch(
       {error, [{_, [{_, astranaut_quote,
                      {invalid_quote_context, {var, _, 'Ctx'}}}]}], _},
       astranaut_quote:parse_transform(Forms, [])),
    ok.

test_empty_context(_Config) ->
    Forms = merl:quote(
              ["-file(\"empty_context_test.erl\", 1).",
               "-module(empty_context_test).",
               "-export([run/0]).",
               "run() -> quote(A, #{context => ''})."])
        ++ [{eof, 5}],
    ?assertMatch(
       {error, [{_, [{_, astranaut_quote, {invalid_quote_context, ''}}]}], _},
       astranaut_quote:parse_transform(Forms, [])),
    ?assertError({invalid_quote_context, ''},
                 astranaut_quote:encode_quote_variable(a, '')),
    ?assertError({invalid_quote_context, ''},
                 astranaut_quote:encode_quote_variable(a, '', 1)),
    ?assertError({invalid_quote_variable_name, ''},
                 astranaut_quote:encode_quote_variable('', ctx)),
    ?assertError({invalid_quote_context, ''},
                 astranaut_quote:quoted(merl:quote(0, "A"), #{context => ''})),
    ok.

test_empty_default_context(_Config) ->
    Forms = [{attribute, 1, file, {"empty_default_context_test.erl", 1}},
             {attribute, 1, module, ''},
             {function, 2, run, 0,
              [{clause, 2, [], [],
                [{call, 2, {atom, 2, quote}, [{var, 2, 'A'}]}]}]},
             {eof, 3}],
    ?assertMatch(
       {error, [{_, [{_, astranaut_quote, {invalid_quote_context, ''}}]}], _},
       astranaut_quote:parse_transform(Forms, [])),
    ok.

test_context_undefined(_Config) ->
    Forms = merl:quote(
              ["-file(\"context_undefined_test.erl\", 1).",
               "-module(context_undefined_test).",
               "-export([run/0]).",
               "run() -> quote(A, #{context => undefined})."])
        ++ [{eof, 5}],
    Transformed = astranaut_quote:parse_transform(Forms, []),
    {function, _, run, 0, [{clause, _, [], [], [Expression]}]} =
        lists:keyfind(run, 3, Transformed),
    {value, {var, _, 'A@astranaut_quote@undefined'}, _} =
        erl_eval:expr(Expression, erl_eval:new_bindings()),
    {value, {var, _, 'B@astranaut_quote@undefined'}, _} =
        erl_eval:expr(
          astranaut_quote:quoted(
            merl:quote(0, "B"), #{context => undefined}),
          erl_eval:new_bindings()),
    ok.

test_low_level_option_validation(_Config) ->
    ?assertError({invalid_quote_context, 123},
                 astranaut_quote:quoted(
                   merl:quote(0, "A"), #{context => 123})),
    ?assertError({invalid_quote_no_context, invalid},
                 astranaut_quote:quoted(
                   merl:quote(0, "A"), #{no_context => invalid})),
    ?assertError({conflicting_quote_context_options, ctx, no_context},
                 astranaut_quote:quoted(
                   merl:quote(0, "A"),
                   #{context => ctx, no_context => true})),
    ok.

test_wildcard(_Config) ->
    {value, {var, _, '_'}, _} =
        erl_eval:expr(
          astranaut_quote:quoted({var, 0, '_'}), erl_eval:new_bindings()),
    {value, {var, _, '_'}, _} =
        erl_eval:expr(
          astranaut_quote:quoted({var, 0, '_'}, #{context => some_ctx}),
          erl_eval:new_bindings()),
    ok.

test_low_level_no_context(_Config) ->
    {value, {var, _, 'A'}, _} =
        erl_eval:expr(
          astranaut_quote:quoted(merl:quote(0, "A")), erl_eval:new_bindings()),
    ok.

test_no_context_named_fun(_Config) ->
    Forms = merl:quote(
              ["-file(\"no_context_named_fun_test.erl\", 1).",
               "-module(no_context_named_fun_test).",
               "-export([run/0]).",
               "run() ->",
               "  quote(fun Name(0) -> 0;",
               "            Name(N) -> Name(N - 1) + N",
               "        end, no_context)."])
        ++ [{eof, 7}],
    Transformed = astranaut_quote:parse_transform(Forms, []),
    {function, _, run, 0, [{clause, _, [], [], [Expression]}]} =
        lists:keyfind(run, 3, Transformed),
    {value, {named_fun, _, 'Name', Clauses}, _} =
        erl_eval:expr(Expression, erl_eval:new_bindings()),
    ?assertNot(has_quote_variable(Clauses)),
    ok.

has_quote_variable(Nodes) ->
    astranaut:search(
      fun({var, _, Name}) ->
              astranaut_quote:decode_quote_variable(Name) =/= not_quote_variable;
         ({named_fun, _, Name, _Clauses}) ->
              astranaut_quote:decode_quote_variable(Name) =/= not_quote_variable;
         (_) ->
              false
      end, Nodes, #{traverse => pre}).
