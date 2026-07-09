%%%-------------------------------------------------------------------
%%% Design-intent regression tests for public contracts that were weakly
%%% covered by line-oriented tests.
%%%-------------------------------------------------------------------
-module(astranaut_design_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

suite() ->
    [{timetrap, {seconds, 30}}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

groups() ->
    [].

all() ->
    [walk_return_contracts,
     do_parse_transform_contracts,
     do_error_contracts,
     return_error_contracts,
     error_state_contracts,
     struct_negative_contracts,
     compile_opts_contract,
     compile_meta_success_contract,
     compile_meta_warning_contract,
     compile_meta_error_contract,
     compile_meta_invalid_and_undefined_contracts].

do_parse_transform_contracts(_Config) ->
    Forms =
        [{attribute, 1, file, {"do_contract_mod.erl", 1}},
         {attribute, 1, module, do_contract_mod},
         {attribute, 2, export, [{value, 0}, {pattern_value, 0}]},
         {function, 3, value, 0,
          [{clause, 3, [], [],
            [{call, 3, {atom, 3, do},
              [lc(3, {atom, 3, monad_maybe},
                  [{generate, 4, {var, 4, 'A'}, {tuple, 4, [{atom, 4, just}, {integer, 4, 1}]}},
                   return_call(5, {op, 5, '+', {var, 5, 'A'}, {integer, 5, 1}})])]}]}]},
         {function, 7, pattern_value, 0,
          [{clause, 7, [], [],
            [{call, 7, {atom, 7, do},
              [lc(7, {atom, 7, monad_maybe},
                  [{generate, 8, {tuple, 8, [{atom, 8, ok}, {var, 8, 'A'}]},
                    {tuple, 8, [{atom, 8, just}, {tuple, 8, [{atom, 8, ok}, {integer, 8, 2}]}]}},
                   {match, 9, {var, 9, 'B'}, {integer, 9, 3}},
                   return_call(10, {op, 10, '+', {var, 10, 'A'}, {var, 10, 'B'}})])]}]}]},
         {eof, 12}],
    OutputForms = astranaut_do:parse_transform(Forms, []),
    {module, do_contract_mod} = compile_and_load(do_contract_mod, OutputForms),
    ?assertEqual({just, 2}, do_contract_mod:value()),
    ?assertEqual({just, 5}, do_contract_mod:pattern_value()).

do_error_contracts(_Config) ->
    ?assertEqual({error, expected_list_comprehension},
                 astranaut_do:do({atom, 1, not_lc}, #{})),
    Empty = lc(1, {atom, 1, monad_maybe}, []),
    EmptyError = astranaut_do:do(Empty, #{monad => astranaut_monad,
                                          monad_fail => astranaut_monad}),
    ?assertEqual([non_empty_do],
                 astranaut_error:errors(astranaut_return:run_error(EmptyError))),
    LastGenerate = lc(1, {atom, 1, monad_maybe},
                      [{generate, 2, {var, 2, 'A'}, {tuple, 2, [{atom, 2, just}, {integer, 2, 1}]}}]),
    LastGenerateError = astranaut_do:do(LastGenerate, #{monad => astranaut_monad,
                                                        monad_fail => astranaut_monad}),
    ?assertEqual([last_generate_expression],
                 astranaut_error:errors(astranaut_return:run_error(LastGenerateError))),
    ?assert(io_lib:deep_char_list(astranaut_do:format_error(non_empty_do))),
    ?assert(io_lib:deep_char_list(astranaut_do:format_error(last_generate_expression))),
    ?assert(io_lib:deep_char_list(astranaut_do:format_error({invalid_option_value, bad}))).

walk_return_contracts(_Config) ->
    Node = {atom, 10, original},
    ?assertEqual(#{'__struct__' => astranaut_walk_return,
                   return => replacement,
                   errors => [],
                   warnings => [warn]},
                 astranaut:walk_return({warning, replacement, warn})),
    ?assertEqual(#{'__struct__' => astranaut_walk_return,
                   return => Node,
                   errors => [],
                   warnings => [warn]},
                 astranaut:walk_return(#{warning => warn, return => Node})),
    ?assertException(exit, {errors_should_be_list, not_a_list},
                     astranaut:walk_return(#{errors => not_a_list})),
    ?assertException(exit, {warnings_should_be_list, not_a_list},
                     astranaut:walk_return(#{warnings => not_a_list})),
    #{return := SkipNode} = astranaut:walk_return(#{continue => true, return => Node}),
    ?assertMatch({uniplate_node_context, Node, _, _, true, _, _, _}, SkipNode).

return_error_contracts(_Config) ->
    WarningOk = astranaut_return:warning_ok(warn, value),
    ?assertEqual({just, value}, astranaut_return:run(WarningOk)),
    ?assertEqual([warn], astranaut_error:warnings(astranaut_return:run_error(WarningOk))),

    ErrorFail = astranaut_return:error_fail(fail_reason),
    ?assertEqual(nothing, astranaut_return:run(ErrorFail)),
    ?assertEqual([fail_reason], astranaut_error:errors(astranaut_return:run_error(ErrorFail))),

    ?assertEqual({just, forms}, astranaut_return:run(astranaut_return:from_compiler(forms))),
    ?assertEqual({just, [form]}, astranaut_return:run(astranaut_return:from_compiler([form]))),

    Formatted = astranaut_return:formatted_error(12, ?MODULE, formatted),
    ?assertEqual([{12, ?MODULE, formatted}],
                 astranaut_error:formatted_errors(astranaut_return:run_error(Formatted))),

    ?assertEqual(true, astranaut_return:has_error(ErrorFail)),
    ?assertException(exit, #{errors := [fail_reason]}, astranaut_return:simplify(ErrorFail)).

error_state_contracts(_Config) ->
    Error0 = astranaut_error:new(),
    Error1 = astranaut_error:append_error(error_1, Error0),
    Error2 = astranaut_error:append_warning(warning_1, Error1),
    ?assertEqual([error_1], astranaut_error:errors(Error2)),
    ?assertEqual([warning_1], astranaut_error:warnings(Error2)),
    ?assertEqual(false, astranaut_error:is_empty_error(Error2)),

    Formatted0 = astranaut_error:new(),
    Formatted1 = astranaut_error:append_formatted_errors([{10, ?MODULE, old}], Formatted0),
    Formatted2 = astranaut_error:with_all_error(fun(old) -> new; (Other) -> Other end, Formatted1),
    ?assertEqual([{10, ?MODULE, new}], astranaut_error:formatted_errors(Formatted2)),

    File0 = astranaut_error:new("a.erl"),
    File1 = astranaut_error:append_formatted_warnings([{5, ?MODULE, old_warning}], File0),
    File2 = astranaut_error:update_file("b.erl", File1),
    File3 = astranaut_error:with_all_warning(fun(old_warning) -> new_warning; (Other) -> Other end, File2),
    ?assertEqual([{"a.erl", [{5, ?MODULE, new_warning}]}], astranaut_error:file_warnings(File3)),

    Pending = astranaut_error:append_formatted_errors([{1, ?MODULE, no_file}], astranaut_error:new()),
    EofPending = astranaut_error:eof(Pending),
    ?assertEqual(eof, astranaut_error:file(EofPending)),
    ?assertEqual([{1, ?MODULE, no_file}], astranaut_error:formatted_errors(EofPending)).

struct_negative_contracts(_Config) ->
    ?assertException(exit, {invalid_record, test, not_tuple},
                     astranaut_struct:from_record_impl(test, [name], not_tuple)),
    ?assertException(exit, {invalid_record, test, {test}},
                     astranaut_struct:from_record_impl(test, [name], {test})),
    ?assertException(exit, {invalid_struct, test, #{'__struct__' := other}},
                     astranaut_struct:to_record_impl(test, [name], #{'__struct__' => other})),
    ?assertException(exit, {invalid_map, not_map},
                     astranaut_struct:from_map_impl(test, [name], [name], #{}, not_map)),
    ?assertException(exit, {missing_enforce_keys, test, [name]},
                     astranaut_struct:from_map_impl(test, [name], [name], #{}, #{})),
    ?assertEqual(#{name => value}, astranaut_struct:to_map(test, #{'__struct__' => test, name => value})).

compile_opts_contract(_Config) ->
    Forms = base_forms(compile_opts_contract_mod),
    OutputForms = astranaut_compile_opts:parse_transform(Forms, [debug_info, {i, "include"}]),
    {module, compile_opts_contract_mod} = compile_and_load(compile_opts_contract_mod, OutputForms),
    Opts = compile_opts_contract_mod:compile_opts(),
    ?assert(lists:member(debug_info, Opts)),
    ?assert(lists:member({i, "include"}, Opts)),
    ?assert(io_lib:deep_char_list(astranaut_compile_opts:format_error("already text"))).

compile_meta_success_contract(_Config) ->
    Forms = meta_forms(compile_meta_success_mod, [compile_meta_identity_transformer], []),
    OutputForms = astranaut_compile_meta_transformer:parse_transform(Forms, []),
    {module, compile_meta_success_mod} = compile_and_load(compile_meta_success_mod, OutputForms),
    ?assertEqual(ok, compile_meta_success_mod:value()),
    ?assertEqual([], compile_meta_success_mod:errors()),
    ?assertEqual([], compile_meta_success_mod:warnings()),
    ?assert(lists:any(fun({attribute, _, compile, {parse_transform, compile_meta_identity_transformer}}) -> true;
                         (_) -> false
                      end, compile_meta_success_mod:forms())).

compile_meta_warning_contract(_Config) ->
    Forms = meta_forms(compile_meta_warning_mod, [compile_meta_warning_transformer], [silent_warning]),
    OutputForms = astranaut_compile_meta_transformer:parse_transform(Forms, []),
    {module, compile_meta_warning_mod} = compile_and_load(compile_meta_warning_mod, OutputForms),
    [{File, [{3, compile_meta_warning_transformer, injected_warning}]}] =
        compile_meta_warning_mod:warnings(),
    ?assertEqual("compile_meta_warning_mod.erl", filename:basename(File)).

compile_meta_error_contract(_Config) ->
    Forms = meta_forms(compile_meta_error_mod, [compile_meta_error_transformer], [silent_error]),
    OutputForms = astranaut_compile_meta_transformer:parse_transform(Forms, []),
    {module, compile_meta_error_mod} = compile_and_load(compile_meta_error_mod, OutputForms),
    [{File, [{4, compile_meta_error_transformer, injected_error}]}] =
        compile_meta_error_mod:errors(),
    ?assertEqual("compile_meta_error_mod.erl", filename:basename(File)),
    ?assertException(error, undef, compile_meta_error_mod:value()).

compile_meta_invalid_and_undefined_contracts(_Config) ->
    InvalidForms = meta_forms(compile_meta_invalid_mod, [compile_meta_invalid_transformer], [silent_warning]),
    InvalidOutput = astranaut_compile_meta_transformer:parse_transform(InvalidForms, []),
    {module, compile_meta_invalid_mod} = compile_and_load(compile_meta_invalid_mod, InvalidOutput),
    [{_File, [{0, astranaut_compile_meta_transformer,
               {invalid_transformer_return, compile_meta_invalid_transformer, invalid_return}}]}] =
        compile_meta_invalid_mod:warnings(),

    UndefinedForms = meta_forms(compile_meta_undefined_mod, [compile_meta_missing_transformer], [silent_warning]),
    UndefinedOutput = astranaut_compile_meta_transformer:parse_transform(UndefinedForms, []),
    {module, compile_meta_undefined_mod} = compile_and_load(compile_meta_undefined_mod, UndefinedOutput),
    [{_File1, [{0, astranaut_compile_meta_transformer,
                {undefined_transformer, compile_meta_missing_transformer}}]}] =
        compile_meta_undefined_mod:warnings(),

    CrashForms = meta_forms(compile_meta_crash_mod, [compile_meta_crash_transformer], [silent_error]),
    CrashOutput = astranaut_compile_meta_transformer:parse_transform(CrashForms, []),
    {module, compile_meta_crash_mod} = compile_and_load(compile_meta_crash_mod, CrashOutput),
    [{_File2, [{none, compile, {parse_transform, compile_meta_crash_transformer, {error, injected_crash, _}}}]}] =
        compile_meta_crash_mod:errors().

base_forms(Module) ->
    [{attribute, 1, file, {atom_to_list(Module) ++ ".erl", 1}},
     {attribute, 1, module, Module},
     {attribute, 2, export, [{value, 0}]},
     {function, 3, value, 0, [{clause, 3, [], [], [{atom, 3, ok}]}]},
     {eof, 4}].

meta_forms(Module, Transformers, MetaOptions) ->
    CompileMeta =
        case MetaOptions of
            [] -> [];
            _ -> [{attribute, 2, astranaut_compile_meta, MetaOptions}]
        end,
    [{attribute, 1, file, {atom_to_list(Module) ++ ".erl", 1}},
     {attribute, 1, module, Module},
     {attribute, 2, compile, {parse_transform, astranaut_compile_meta_transformer}}] ++
        [{attribute, 2, compile, {parse_transform, Transformer}} || Transformer <- Transformers] ++
        CompileMeta ++
        [{attribute, 3, export, [{value, 0}]},
         {function, 4, value, 0, [{clause, 4, [], [], [{atom, 4, ok}]}]},
         {eof, 5}].

lc(Pos, Monad, Comprehensions) ->
    {lc, Pos, Monad, Comprehensions}.

return_call(Pos, Expr) ->
    {call, Pos, {atom, Pos, return}, [Expr]}.

compile_and_load(Module, Forms) ->
    code:purge(Module),
    code:delete(Module),
    {ok, Module, Binary, _Warnings} = compile:forms(Forms, [binary, return_errors, return_warnings]),
    code:load_binary(Module, atom_to_list(Module) ++ ".erl", Binary).
