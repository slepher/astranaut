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
                   macro_node_role_test,
                   macro_quote_context_example, macro_quote_context_test],
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
     test_macro_guard_call,
     test_quote_context_hygienic,
     test_quote_context_no_context,
     test_quote_context_named_fun,
     test_quote_context_same_context,
     test_quote_context_different_context,
     test_quote_context_unquote_identity,
     test_quote_context_local_hygienic,
     test_quote_context_attribute_no_counter,
     test_quote_context_no_double_counter].

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

test_quote_context_hygienic(_Config) ->
    ?assertEqual(10, macro_quote_context_test:test_hygienic()).

test_quote_context_no_context(_Config) ->
    ?assertEqual(20, macro_quote_context_test:test_no_context_capture()),
    ?assertEqual(120, macro_quote_context_test:test_no_context_named_fun()).

test_quote_context_named_fun(_Config) ->
    ?assertEqual(120, macro_quote_context_test:test_named_fun()).

test_quote_context_same_context(_Config) ->
    ?assertEqual(42, macro_quote_context_test:test_same_context()).

test_quote_context_different_context(_Config) ->
    ?assertEqual({1, 2}, macro_quote_context_test:test_different_context()).

test_quote_context_unquote_identity(_Config) ->
    ?assertEqual({6, 5}, macro_quote_context_test:test_unquote_identity()).

test_quote_context_local_hygienic(_Config) ->
    ?assertEqual(10, macro_quote_context_test:test_local_hygienic()).

test_quote_context_attribute_no_counter(_Config) ->
    Forms = [{attribute, 1, file, {"attr_no_counter_test.erl", 1}},
             {attribute, 1, module, attr_no_counter_test},
             {attribute, 2, import_macro, macro_quote_context_example},
             {attribute, 3, attr_no_counter, [42]},
             {eof, 4}],
    Output = expand_forms(Forms),
    Function = lists:keyfind(generated_attr_fun, 3, Output),
    ?assertMatch({function, _, generated_attr_fun, 0, _}, Function),
    Vars = collect_var_names(Function),
    ?assert(
       lists:member(
         'AttrVar@astranaut_quote@macro_quote_context_example', Vars)),
    ?assertNot(lists:any(fun has_expanded_counter/1, Vars)),
    ok.

test_quote_context_no_double_counter(_Config) ->
    Forms = [{attribute, 1, file, {"no_double_counter_test.erl", 1}},
             {attribute, 1, module, no_double_counter_test},
             {attribute, 2, import_macro, macro_quote_context_example},
             {function, 3, run, 0,
              [{clause, 3, [], [],
                [{call, 3,
                  {remote, 3, {atom, 3, macro_quote_context_example},
                   {atom, 3, already_expanded_macro}},
                  []}]}]},
             {eof, 4}],
    Output = expand_forms(Forms),
    {function, _, run, 0, _} = lists:keyfind(run, 3, Output),
    %% the already-expanded variable name is preserved as-is. A double append
    %% would produce a second counter, so the only var carrying the encoded
    %% 'Already' prefix must be the original single-counter name.
    Vars = collect_var_names(Output),
    ?assertEqual(
       ['Already@astranaut_quote@macro_quote_context_example@1'],
       [Name || Name <- Vars,
                lists:prefix("Already", atom_to_list(Name))]),
    ok.

expand_forms(Forms) ->
    case astranaut_macro:parse_transform(Forms, []) of
        {warning, Forms1, _Warnings} -> Forms1;
        {error, _Errors, _Warnings} = Error -> exit({expand_failed, Error});
        Forms1 -> Forms1
    end.

collect_var_names(Nodes) ->
    lists:usort(var_names(Nodes)).

var_names(Nodes) when is_list(Nodes) ->
    lists:append([var_names(Node) || Node <- Nodes]);
var_names({var, _, Name}) when is_atom(Name) ->
    [Name];
var_names(Node) when is_tuple(Node) ->
    var_names(tuple_to_list(Node));
var_names(_) ->
    [].

has_expanded_counter(Name) ->
    case astranaut_quote:decode_quote_variable(Name) of
        {expanded, _OriginalName, _Context, _Counter} -> true;
        _ -> false
    end.

test_macro_guard_call(_Config) ->
    ?assertEqual(even, macro_guard_test:macro_guard(4)),
    ?assertEqual(odd, macro_guard_test:macro_guard(5)).
