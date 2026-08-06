%%%-------------------------------------------------------------------
%%% Parse-transform diagnostics, recovery, and formatter coverage.
%%%-------------------------------------------------------------------
-module(astranaut_macro_error_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

suite() ->
    [{timetrap, {seconds, 60}}].

init_per_suite(Config0) ->
    Config = astranaut_test_lib:with_suite_data_dir(
               Config0, astranaut_macro_SUITE),
    astranaut_test_lib:load_data_modules(
      Config, [macro_example, macro_uniform_a, macro_uniform_b]).

end_per_suite(_Config) ->
    ok.

all() ->
    [test_macro_with_warnings,
     test_macro_with_error,
     test_macro_local_formatter_legacy,
     test_macro_local_formatter_strict,
     test_macro_local_formatter_only_v2,
     test_macro_export_rejects_local_closure_options,
     test_macro_options_rejects_local_closure_options,
     test_macro_local_rejects_internal_function,
     test_macro_local_retain_warnings,
     test_macro_local_declaration_single_diagnostic,
     test_macro_local_declaration_preserves_prior_registration,
     test_macro_invalid_attr_errors,
     test_use_macro_errors,
     test_macro_format_error_predefined_errors,
     test_macro_sibling_errors].

test_macro_with_warnings(Config) ->
    Forms = astranaut_test_lib:test_module_forms(
              macro_with_warnings, Config),
    Basepos = astranaut_test_lib:get_baseline(yep, Forms),
    ErrorStruct = astranaut_return:run_error(
                    astranaut_test_lib:compile_test_forms(Forms)),
    {FileErrors, [{File, Warnings}]} =
        astranaut_test_lib:realize_with_baseline(Basepos, ErrorStruct),
    ?assertEqual([], FileErrors),
    ?assertEqual("macro_with_warnings.erl", filename:basename(File)),
    [{2, astranaut_macro, invalid_macro_attribute},
     {3, astranaut_macro, invalid_macro_attribute},
     {5, Local, noop_function},
     {12, Local, noop},
     {18, Local, noop},
     {20, Local, noop},
     {25, astranaut_quote,
      {unquote_splicing_pattern_non_empty_tail, [{atom, _, tail}]}}] =
        Warnings,
    assert_local_macro_module(macro_with_warnings, Local),
    Exports = Local:module_info(exports),
    ?assert(lists:member({format_error, 1}, Exports)),
    ?assertEqual("oops, noop",
                 Local:format_error(noop)),
    ?assertEqual(io_lib:write(noop_function),
                 Local:format_error(noop_function)),
    astranaut_test_lib:assert_formatted_messages(Warnings),
    ?assertEqual(ok, macro_with_warnings:test_attributes()),
    ok.

test_macro_with_error(Config) ->
    ct:pal("Verifying Test Code Integrity: Expecting Line 27 check to exist."),
    Forms = astranaut_test_lib:test_module_forms(macro_with_error, Config),
    Baseline = astranaut_test_lib:get_baseline(yep, Forms),
    Return = astranaut_test_lib:compile_test_forms(Forms),
    ErrorStruct = astranaut_return:run_error(Return),
    {[{_File, Errors}], []} =
        astranaut_test_lib:realize_with_baseline(Baseline, ErrorStruct),
    [{2, astranaut_macro,
      {invalid_import_macro_attr, {invalid_macro_tuple}}},
     {3, astranaut_macro,
      {import_macro_failed, non_exists_module}},
     {4, astranaut_macro,
      {unimported_macro_module, unimported_macro_module}},
     {6, astranaut_macro,
      {undefined_macro, undefined_macro_0, 0}},
     {7, astranaut_macro,
      {undefined_macro, undefined_macro_1, 0}},
     {8, astranaut_macro,
      {undefined_macro, undefined_macro_2, 0}},
     {9, astranaut_macro,
      {undefined_macro, undefined_macro_3, 0}},
     {13, Local,
      {macro_exception, _MFA, [], _StackTrace}},
     {16, Local, bar},
     {27, astranaut_macro,
      {max_macro_expansion_depth_exceeded,
       {macro_example, recursive_macro},
       [{integer, _Pos, 6}]}}] = Errors,
    assert_local_macro_module(macro_with_error, Local),
    Exports = Local:module_info(exports),
    ?assert(lists:member({format_error, 1}, Exports)),
    ?assertEqual("oops, bar",
                 Local:format_error(bar)),
    astranaut_test_lib:assert_formatted_messages(Errors),
    ok.

test_macro_local_formatter_legacy(Config) ->
    Forms = astranaut_test_lib:test_module_forms(
              macro_local_formatter_legacy_test, Config),
    Baseline = astranaut_test_lib:get_baseline(yep, Forms),
    ErrorStruct = astranaut_return:run_error(
                    astranaut_test_lib:compile_test_forms(Forms)),
    {[], [{File, Warnings}]} =
        astranaut_test_lib:realize_with_baseline(Baseline, ErrorStruct),
    ?assertEqual("macro_local_formatter_legacy_test.erl",
                 filename:basename(File)),
    [{8, Local, legacy_local_formatter_warning}] = Warnings,
    assert_local_macro_module(macro_local_formatter_legacy_test, Local),
    Exports = Local:module_info(exports),
    ?assert(lists:member({format_error, 1}, Exports)),
    ?assertNot(lists:member({format_error, 2}, Exports)),
    ?assertEqual("legacy local formatter warning",
                 Local:format_error(legacy_local_formatter_warning)),
    astranaut_test_lib:assert_formatted_messages(Warnings),
    ok.

test_macro_local_formatter_strict(Config) ->
    Forms = astranaut_test_lib:test_module_forms(
              macro_local_formatter_strict_test, Config),
    Baseline = astranaut_test_lib:get_baseline(yep, Forms),
    ErrorStruct = astranaut_return:run_error(
                    astranaut_test_lib:compile_test_forms(Forms)),
    {[], [{File, Warnings}]} =
        astranaut_test_lib:realize_with_baseline(Baseline, ErrorStruct),
    ?assertEqual("macro_local_formatter_strict_test.erl",
                 filename:basename(File)),
    [{11, Local, strict_local_formatter_warning}] = Warnings,
    assert_local_macro_module(macro_local_formatter_strict_test, Local),
    Exports = Local:module_info(exports),
    ?assert(lists:member({format_error, 1}, Exports)),
    ?assertNot(lists:member({strict_local_formatter_message, 0}, Exports)),
    ?assertEqual("strict local formatter warning",
                 Local:format_error(strict_local_formatter_warning)),
    Unknown = {strict_local_formatter_unknown, [term]},
    ?assertEqual(io_lib:write(Unknown),
                 astranaut_lib:format_error(
                   Unknown, fun Local:format_error/1)),
    astranaut_test_lib:assert_formatted_messages(Warnings),
    ok.

test_macro_local_formatter_only_v2(Config) ->
    Forms = astranaut_test_lib:test_module_forms(
              macro_local_formatter_only_v2_test, Config),
    Baseline = astranaut_test_lib:get_baseline(yep, Forms),
    ErrorStruct = astranaut_return:run_error(
                    astranaut_test_lib:compile_test_forms(Forms)),
    {[], [{File, Warnings}]} =
        astranaut_test_lib:realize_with_baseline(Baseline, ErrorStruct),
    ?assertEqual("macro_local_formatter_only_v2_test.erl",
                 filename:basename(File)),
    [{8, astranaut_macro, invalid_macro_attribute}] = Warnings,
    astranaut_test_lib:assert_formatted_messages(Warnings),
    ok.

test_macro_export_rejects_local_closure_options(Config) ->
    Forms = astranaut_test_lib:test_module_forms(
              macro_export_local_options_warning_test, Config),
    Baseline = astranaut_test_lib:get_baseline(yep, Forms),
    ErrorStruct = astranaut_return:run_error(
                    astranaut_test_lib:compile_test_forms(Forms)),
    {[], Warnings} =
        astranaut_test_lib:realize_with_baseline(Baseline, ErrorStruct),
    MacroWarnings =
        [Warning || {_File, FileWarnings} <- Warnings,
                    {_Line, astranaut_macro, Warning} <- FileWarnings],
    ?assertEqual(
       [[closure_roots, internal_function]],
       [lists:sort(Keys)
        || {unexpected_option_keys, Keys} <- MacroWarnings]),
    ok.

test_macro_options_rejects_local_closure_options(Config) ->
    Forms = astranaut_test_lib:test_module_forms(
              macro_options_local_options_warning_test, Config),
    Baseline = astranaut_test_lib:get_baseline(yep, Forms),
    ErrorStruct = astranaut_return:run_error(
                    astranaut_test_lib:compile_test_forms(Forms)),
    {[], Warnings} =
        astranaut_test_lib:realize_with_baseline(Baseline, ErrorStruct),
    MacroWarnings =
        [Warning || {_File, FileWarnings} <- Warnings,
                    {_Line, astranaut_macro, Warning} <- FileWarnings],
    ?assertEqual(
       [[closure_roots, internal_function]],
       [lists:sort(Keys)
        || {unexpected_option_keys, Keys} <- MacroWarnings]),
    ok.

test_macro_local_rejects_internal_function(Config) ->
    Forms = astranaut_test_lib:test_module_forms(
              macro_local_internal_option_warning_test, Config),
    Baseline = astranaut_test_lib:get_baseline(yep, Forms),
    ErrorStruct = astranaut_return:run_error(
                    astranaut_test_lib:compile_test_forms(Forms)),
    {[], Warnings} =
        astranaut_test_lib:realize_with_baseline(Baseline, ErrorStruct),
    MacroWarnings =
        [Warning || {_File, FileWarnings} <- Warnings,
                    {_Line, astranaut_macro, Warning} <- FileWarnings],
    ?assertEqual(
       [[internal_function]],
       [lists:sort(Keys)
        || {unexpected_option_keys, Keys} <- MacroWarnings]),
    ok.

test_macro_local_retain_warnings(Config) ->
    Forms = astranaut_test_lib:test_module_forms(
              macro_local_retain_warning_test, Config),
    Baseline = astranaut_test_lib:get_baseline(yep, Forms),
    ErrorStruct = astranaut_return:run_error(
                    astranaut_test_lib:compile_test_forms(Forms)),
    {[], Warnings} =
        astranaut_test_lib:realize_with_baseline(Baseline, ErrorStruct),
    MacroWarnings =
        [{Line, Warning} || {_File, FileWarnings} <- Warnings,
                            {Line, astranaut_macro, Warning} <- FileWarnings],
    ?assertEqual(
       lists:sort(
         [{4, {undefined_local_macro_retain, [{missing, 0}]}},
          {4, {ineffective_local_macro_retain, [{ordinary, 0}]}}]),
       lists:sort(MacroWarnings)),
    ok.

test_macro_local_declaration_single_diagnostic(Config) ->
    Forms = astranaut_test_lib:test_module_forms(
              macro_local_declaration_invalid_test, Config),
    Baseline = astranaut_test_lib:get_baseline(yep, Forms),
    ErrorStruct = astranaut_return:run_error(
                    astranaut_test_lib:compile_test_forms(Forms)),
    {[{_File, Errors}], []} =
        astranaut_test_lib:realize_with_baseline(Baseline, ErrorStruct),
    MacroErrors =
        [Error || {_Line, astranaut_macro, Error} <- Errors],
    ?assertMatch(
       [{invalid_function_with_arity, {bad, -1}}],
       MacroErrors),
    ok.

test_macro_local_declaration_preserves_prior_registration(Config) ->
    Forms = astranaut_test_lib:test_module_forms(
              macro_local_declaration_duplicate_test, Config),
    Baseline = astranaut_test_lib:get_baseline(yep, Forms),
    ErrorStruct = astranaut_return:run_error(
                    astranaut_test_lib:compile_test_forms(Forms)),
    {[{_File, Errors}], Warnings} =
        astranaut_test_lib:realize_with_baseline(Baseline, ErrorStruct),
    MacroErrors =
        [Error || {_Line, astranaut_macro, Error} <- Errors],
    ?assertMatch(
       [{duplicate_local_macro_declaration, {gen, 1}}],
       MacroErrors),
    ?assertNot(
       lists:any(
         fun({_File1, FileWarnings}) ->
                 lists:any(
                   fun({_Line, astranaut_macro,
                        invalid_macro_attribute}) ->
                           true;
                      (_) ->
                           false
                   end, FileWarnings)
         end, Warnings)),
    ok.

test_macro_invalid_attr_errors(Config) ->
    Forms = astranaut_test_lib:test_module_forms(
              macro_invalid_attr_test, Config),
    Baseline = astranaut_test_lib:get_baseline(yep, Forms),
    ErrorStruct = astranaut_return:run_error(
                    astranaut_test_lib:compile_test_forms(Forms)),
    {[{_File, Errors}], []} =
        astranaut_test_lib:realize_with_baseline(Baseline, ErrorStruct),
    ?assertMatch(
       [{2, astranaut_macro, {invalid_attr, use_macro, foo}},
        {3, astranaut_macro, {invalid_attr, local_macro, bar}}],
       Errors),
    astranaut_test_lib:assert_formatted_messages(Errors),
    ok.

test_use_macro_errors(Config) ->
    Forms = astranaut_test_lib:test_module_forms(
              macro_use_error_test, Config),
    Baseline = astranaut_test_lib:get_baseline(yep, Forms),
    ErrorStruct = astranaut_return:run_error(
                    astranaut_test_lib:compile_test_forms(Forms)),
    {[{_File, Errors}], []} =
        astranaut_test_lib:realize_with_baseline(Baseline, ErrorStruct),
    ?assertMatch(
       [{3, astranaut_macro,
         {unexported_macro, macro_uniform_a, missing_export, 1}},
        {4, astranaut_macro,
         {undefined_macro, missing_local, 0}},
        {5, astranaut_macro,
         {invalid_function_with_arity, {bad_arity, -1}}}],
       Errors),
    astranaut_test_lib:assert_formatted_messages(Errors),
    ok.

test_macro_format_error_predefined_errors(_Config) ->
    ExistingMacro =
        #{macro_module => macro_a, function => to_a, arity => 1},
    OverridingMacro =
        #{macro_module => macro_b, function => to_b, arity => 1},
    DirectInvalidReturn =
        #{macro =>
              #{mfa =>
                    #{module => macro_a, function => bad, arity => 1}},
          reason => invalid_role,
          expected_role => expression,
          actual_type => function},
    NestedInvalidReturn =
        #{origin_macro =>
              #{mfa =>
                    #{module => macro_a, function => outer, arity => 0}},
          current_macro =>
              #{mfa =>
                    #{module => macro_a, function => inner, arity => 0}},
          reason => invalid_role,
          expected_role => guard,
          actual_type => application},
    Errors =
        [{import_macro_failed, missing_macro_module},
         {invalid_import_macro_attr, {invalid_macro_tuple}},
         {unimported_macro_module, macro_a},
         {unexported_macro, macro_a, missing, 1},
         {undefined_macro, missing, 0},
         {invalid_use_macro,
          #{macro_module => macro_a, function => to_a, arity => 1}},
         {macro_override,
          {macro_a, to_a, 1}, ExistingMacro, OverridingMacro},
         {non_exported_formatter, macro_formatter},
         {unloaded_formatter_module, missing_formatter},
         invalid_macro_attribute,
         {max_macro_expansion_depth_exceeded,
          {macro_a, recurse}, [{integer, 1, 3}]},
         {max_macro_expansion_depth_exceeded,
          recurse, [{integer, 1, 3}]},
         {macro_exception,
          #{module => macro_a, function => explode, arity => 1},
          [{atom, 1, ok}],
          {error, bad_macro, []}},
         {invalid_macro_return, DirectInvalidReturn},
         {invalid_macro_return, NestedInvalidReturn},
         {undefined_local_macro_retain, [{missing, 0}]},
         {ineffective_local_macro_retain, [{ordinary, 0}]}],
    lists:foreach(fun assert_macro_format_error/1, Errors),
    Unknown = {unknown_macro_format_error, [term]},
    ?assertEqual(io_lib:write(Unknown),
                 astranaut_lib:format_error(
                   Unknown, fun astranaut_macro:format_error/1)).

test_macro_sibling_errors(Config) ->
    Forms = astranaut_test_lib:test_module_forms(
              macro_sibling_errors_test, Config),
    ErrorStruct = astranaut_return:run_error(
                    astranaut_test_lib:compile_test_forms(Forms)),
    {[{_File, Errors}], []} =
        astranaut_test_lib:realize_with_baseline(0, ErrorStruct),
    ?assertEqual(3, length(Errors)),
    ?assert(
       lists:any(
         fun({_Pos, _Formatter,
              {macro_exception,
               #{function := raise_macro}, [], _Exception}}) ->
                 true;
            ({_Pos, astranaut_macro,
              {local_macro_diagnostic, _Local,
               {macro_exception,
                #{function := raise_macro}, [], _Exception}, _Message}}) ->
                 true;
            (_) ->
                 false
         end, Errors)),
    ?assert(
       lists:any(
         fun({_Pos, _Formatter, sibling_return_error}) ->
                 true;
            ({_Pos, astranaut_macro,
              {local_macro_diagnostic, _Local,
               sibling_return_error, _Message}}) ->
                 true;
            (_) ->
                 false
         end, Errors)),
    ?assert(
       lists:any(
         fun({_Pos, astranaut_macro,
              {invalid_macro_return,
               #{current_macro :=
                     #{mfa :=
                           #{function := invalid_return_macro}}}}}) ->
                 true;
            (_) ->
                 false
         end, Errors)),
    Local = sibling_local_formatter(Errors),
    Exports = Local:module_info(exports),
    ?assert(lists:member({format_error, 1}, Exports)),
    ?assertEqual(io_lib:write(sibling_return_error),
                 Local:format_error(sibling_return_error)),
    astranaut_test_lib:assert_formatted_messages(Errors),
    ok.

assert_macro_format_error(Error) ->
    Message = astranaut_macro:format_error(Error),
    ?assert(io_lib:deep_char_list(Message)),
    ?assertNotEqual([], lists:flatten(Message)).

assert_local_macro_module(SourceModule, LocalModule) ->
    Prefix = atom_to_list(SourceModule) ++ "__local_macro__",
    ?assert(lists:prefix(Prefix, atom_to_list(LocalModule))),
    ?assertMatch({file, _}, code:is_loaded(LocalModule)).

sibling_local_formatter(Errors) ->
    Prefix = atom_to_list(macro_sibling_errors_test) ++ "__local_macro__",
    Candidates =
        lists:flatmap(
          fun({_Pos, Formatter, _Error}) when is_atom(Formatter) ->
                  case lists:prefix(Prefix, atom_to_list(Formatter)) of
                      true -> [Formatter];
                      false -> []
                  end;
             ({_Pos, astranaut_macro,
               {local_macro_diagnostic, Local, _Error, _Message}}) ->
                  [Local];
             (_) ->
                  []
          end, Errors),
    case Candidates of
        [Local | _] ->
            assert_local_macro_module(macro_sibling_errors_test, Local),
            Local;
        [] ->
            ct:fail({missing_sibling_local_formatter, Errors})
    end.
