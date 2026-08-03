%%%-------------------------------------------------------------------
%%% Public macro behavior exercised through compiled fixture modules.
%%%-------------------------------------------------------------------
-module(astranaut_macro_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include("otp_vsn.hrl").

suite() ->
    [{timetrap, {seconds, 60}}].

init_per_suite(Config) ->
    TestModules = [macro_exports, macro_native_record, macro_example, macro_test,
                   macro_guard_macros, macro_guard_test,
                   macro_node_role_test],
    astranaut_test_lib:load_data_modules(Config, TestModules).

end_per_suite(_Config) ->
    ok.

all() ->
    [test_ok_case,
     test_function_case,
     test_quote_case,
     test_unquote_splicing_case,
     test_pattern_case,
     test_quote_code_case,
     test_other_case,
     test_macro_order,
     test_macro_with_vars,
     test_merge_rename_function,
     test_nested_macro,
     test_recursive_macro,
     test_native_record_macro,
     test_macro_literal,
     test_macro_node_roles,
     test_macro_simple_guard,
     test_macro_complex_guard,
     test_macro_guard_call].

test_ok_case(_Config) ->
    ?assertEqual(ok, macro_test:test_ok()).

test_function_case(_Config) ->
    ?assertEqual(ok, macro_test:test_function(world)),
    ?assertEqual({error, foo}, macro_test:test_function(foo)),
    ok.

test_quote_case(_Config) ->
    ?assertEqual({ok, ok}, macro_test:test_unquote()),
    ?assertEqual({ok, ok}, macro_test:test_binding()),
    ok.

test_unquote_splicing_case(_Config) ->
    ?assertEqual({ok, {hello, foo, bar, world}}, macro_test:test_unquote_splicing()),
    {Value1, Value2} = macro_test:test_unquote_splicing_mix(),
    ?assertEqual({ok, [hello, foo, bar, world], {hello, foo, bar, world}}, Value1),
    ?assertEqual({error, foo, zaa}, Value2),
    ok.

test_pattern_case(_Config) ->
    ?assertEqual({hello, world, foo, bar}, macro_test:test_match_pattern()),
    ?assertEqual({ok, {hello2, world, world, {hello, world}}},
                 macro_test:test_function_pattern_1()),
    ?assertEqual({error, {foo, bar}}, macro_test:test_function_pattern_2()),
    ?assertEqual({ok, 11}, macro_test:test_case_pattern_1()),
    ?assertEqual({ok, {hello, world, foo, bar}}, macro_test:test_case_pattern_2()),
    ?assertEqual({error, task}, macro_test:test_case_pattern_3()),
    ok.

test_quote_code_case(_Config) ->
    ?assertEqual(ok, macro_test:test_quote_code()),
    ?assertEqual({hello, ok}, macro_test:test_quote_pos_1()),
    Ast = {tuple, 20, [{atom, 20, a}, {atom, 20, b}]},
    NewAst = {tuple, 22, [{atom, 22, ok},
                         {tuple, 23, [{atom, 23, hello}, Ast]}]},
    ?assertEqual(NewAst, macro_example:quote_pos_2(Ast)),
    ok.

test_other_case(_Config) ->
    ?assertEqual(true, macro_test:test_case()),
    ?assertException(exit, throw, macro_test:test_try_catch()),
    ?assertEqual({hello, ok, world}, macro_test:test_function()),
    ?assertMatch({ok, {_, _, macro_test}}, macro_test:test_attributes()),
    ?assertEqual({ok, {hello, world}}, macro_test:test_group_args()),
    ok.

test_macro_order(_Config) ->
    ?assertEqual({fail, ok}, macro_test:test_macro_order()),
    ok.

test_macro_with_vars(_Config) ->
    ?assertEqual(112, macro_test:test_macro_with_vars(13)).

test_merge_rename_function(_Config) ->
    Value1 = macro_test:test_merged_function(ok_1),
    Value2 = macro_test:test_merged_function(ok_2),
    Value3 = macro_test:test_merged_function(ok_3),
    Value4 = macro_test:test_merged_function(ok_4),
    ?assertEqual({ok_1, ok_2, ok_3, ok_4},
                 {Value1, Value2, Value3, Value4}),
    ok.

test_nested_macro(_Config) ->
    ?assertEqual(45, macro_test:test_nested_macro(7)).

test_recursive_macro(_Config) ->
    ?assertEqual({4, {3, {2, {1, blast_off}}}},
                 macro_test:test_recursive_macro()).

-if(?ASTRANAUT_OTP_VSN_GE(29)).
test_native_record_macro(_Config) ->
    ?assertEqual({40, 41, 42, 42}, macro_test:test_native_record_macro()).
-else.
test_native_record_macro(_Config) ->
    ok.
-endif.

test_macro_literal(_Config) ->
    %% ?assertMatch({atom, _, ok}, macro_test:test_macro_literal()).
    ok.

test_macro_node_roles(_Config) ->
    ?assertEqual(ok, macro_node_role_test:test_node_roles()).

test_macro_simple_guard(_Config) ->
    ?assertEqual(integer, macro_guard_test:simple(1)),
    ?assertEqual(other, macro_guard_test:simple(not_an_integer)).

test_macro_complex_guard(_Config) ->
    ?assertEqual(in_range, macro_guard_test:complex(12)),
    ?assertEqual(in_range, macro_guard_test:complex(12.5)),
    ?assertEqual(out_of_range, macro_guard_test:complex(9)),
    ?assertEqual(out_of_range, macro_guard_test:complex(not_a_number)).

test_macro_guard_call(_Config) ->
    ?assertEqual(even, macro_guard_test:macro_guard(4)),
    ?assertEqual(odd, macro_guard_test:macro_guard(5)).
