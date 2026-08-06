%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2018, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  8 Dec 2018 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_SUITE).

-compile(nowarn_export_all).
-compile(export_all).

-include("astranaut_struct_name.hrl").
-include_lib("syntax_tools/include/merl.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

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
    erlang:system_flag(backtrace_depth, 20),
    Config1 = astranaut_test_lib:load_data_modules(Config, [sample_transformer_1]),
    Forms = astranaut_test_lib:test_module_forms(sample_1, Config1),
    Forms1 = astranaut_test_lib:test_module_forms(sample_expressions, Config1),
    Expressions =
        lists:foldl(
          fun({function, _Pos, Name, _Arity, [{clause, _Pos1, _Pattern, [], Expressions}]}, Acc) ->
                  maps:put(Name, Expressions, Acc);
             (_Form, Acc) ->
                  Acc
          end, #{}, Forms1),
    [{forms, Forms}, {expressions, Expressions}|Config1].

%%--------------------------------------------------------------------
%% @spec end_per_suite(Config0) -> term() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

format_error(Message) ->
    io_lib:format("~p", [Message]).

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
    [test_simple_map, test_traverse_m_error,
     test_uniplate_reduce, test_reduce, test_map_with_state_node,
     test_state_mapping_wrappers, test_mapfold_invalid_return,
     test_map_with_state, test_map_spec, test_map_type,
     test_reduce_attr, test_with_formatter, 
     test_options, test_validator, test_validator_failure_contracts,
     test_invalid_validator_return_format,
     test_format_error_unknown_default, test_format_error_unknown_throw,
     test_format_error_known_throw_option,
     test_with_attribute, test_forms_with_attribute,
     test_traverse_m_updated, test_map_m_preserves_form_order,
     test_map_forms, test_sequence_nodes,
     test_continue_sequence_children, test_record, test_typed_record_field,
     test_map, test_if_expr, test_case_expr, test_try_catch_expr,
     test_subtree_callback_without_tree
    ].

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
test_simple_map(_Config) ->
    Node0 = {atom, 1, ok},
    Monad =
        astranaut:map_m(
          fun(Node) ->
                  astranaut_traverse:return(Node)
          end, Node0, #{traverse => pre, role => expression}),
    astranaut_traverse:bind(
      astranaut_traverse:listen_updated(Monad),
      fun(Updated) ->
              ?assertEqual(false, Updated)
      end),
    Return = astranaut_return:simplify(astranaut_traverse:eval(Monad, astranaut, #{}, ok)),
    ?assertEqual(Node0, Return),
    ok.

test_traverse_m_error(_Config) ->
    M1 = astranaut_traverse:update_file("file0"),
    M2 = astranaut_traverse:formatted_errors([{10, astranaut, error_0}]),
    M3 = astranaut_traverse:formatted_errors([{10, astranaut, error_1}]),
    M4 = astranaut_traverse:update_file("file1"),
    M5 = astranaut_traverse:formatted_errors([{10, astranaut, error_2}]),
    M6 = astranaut_traverse:eof(),
    Ms = astranaut_traverse:sequence_m([M1, M2, M3, M4, M5, M6]),
    ErrorStruct = astranaut_return:run_error(astranaut_traverse:eval(Ms, astranaut_1, #{}, ok)),
    ?assertEqual(#{file_errors =>
                       #{"file0" =>
                             [{10,astranaut,error_0},
                              {10,astranaut,error_1}],
                         "file1" => [{10,astranaut,error_2}]}},
                 astranaut_error:printable(ErrorStruct)),
    ok.

test_uniplate_reduce(Config) ->
    Forms = proplists:get_value(forms, Config),
    Return =
        astranaut:sreduce(
          fun({atom, _Pos, mark_1}, Acc) ->
                  Acc + 1;
             (_Node, Acc) ->
                  Acc
          end, 0, Forms, #{}),
    ?assertEqual(1, Return),
    ok.

test_reduce(Config) ->
    Forms = proplists:get_value(forms, Config),
    Baseline = astranaut_test_lib:get_baseline(function_base, Forms),
    File = astranaut_lib:analyze_forms_file(Forms),
    Return =
        astranaut:reduce(
          fun({atom, _Pos, mark_1}, Acc, #{}) ->
                  astranaut:walk_return(#{warning => mark_1, state => Acc + 1});
             ({atom, _Pos, mark_error_1}, _Acc, #{}) ->
                  {error, mark_error_1};
             (_Node, Acc, #{}) ->
                  Acc
          end, 0, Forms, #{formatter => ?MODULE}),
    ErrorStruct = astranaut_return:run_error(Return),
    io:format("get printable errors ~p~n", [ astranaut_error:printable(ErrorStruct)]),
    ?assertEqual(#{}, maps:without([file_errors, file_warnings], astranaut_error:printable(ErrorStruct))),
    {FileErrors, FileWarnings} = astranaut_test_lib:realize_with_baseline(Baseline, ErrorStruct),
    ?assertMatch({[{File, [{2, ?MODULE, mark_error_1}]}], [{File, [{5, ?MODULE, mark_1}]}]},
                 {FileErrors, FileWarnings}),
    astranaut_test_lib:assert_formatted_messages([{Line, Formatter, Error}
                                                  || {_File, Errors} <- FileErrors,
                                                     {Line, Formatter, Error} <- Errors]),
    astranaut_test_lib:assert_formatted_messages([{Line, Formatter, Warning}
                                                  || {_File, Warnings} <- FileWarnings,
                                                     {Line, Formatter, Warning} <- Warnings]),
    ?assertEqual({just, 1}, astranaut_return:run(Return)),
    ok.

test_map_with_state_node(_Config) ->
    NodeA = {match, 10, {var, 10, 'A'}, {atom, 10, a}},
    {Return, _} =
        astranaut:smapfold(
          fun({var, Pos, 'A'}, Acc, #{}) ->
                  Node1 = {var, Pos, 'B'},
                  {Node1, Acc + 1};
             (Node, Acc, #{}) ->
                  {Node, Acc}
          end, 0, NodeA, #{role => expression}),
    ?assertEqual({match, 10, {var, 10, 'B'}, {atom, 10, a}}, Return),
    ok.

test_state_mapping_wrappers(_Config) ->
    Node0 = {atom, 10, old_value},
    Node1 = {atom, 10, new_value},
    Mapper =
        fun({atom, Pos, old_value}, State) ->
                {{atom, Pos, new_value}, State + 1};
           (Node, State) ->
                {Node, State}
        end,
    Opts = #{traverse => pre, role => expression},
    ?assertEqual(
       Node1,
       astranaut:smap_with_state(Mapper, 0, Node0, Opts)),
    ?assertEqual(
       {just, Node1},
       astranaut_return:run(
         astranaut:map_with_state(Mapper, 0, Node0, Opts))),
    ok.

test_mapfold_invalid_return(_Config) ->
    Node = {atom, 10, value},
    ?assertError(
       {invalid_mapfold_return, invalid, Node},
       astranaut:mapfold(
         fun(_Node, _State) ->
                 invalid
         end, 0, Node, #{traverse => pre, role => expression})),
    ok.

test_map_with_state(Config) ->
    Forms = proplists:get_value(forms, Config),
    Baseline = astranaut_test_lib:get_baseline(function_base, Forms),
    File = astranaut_lib:analyze_forms_file(Forms),
    ReturnM =
        astranaut:mapfold(
          fun({atom, _Pos, mark_1} = Node, Acc, #{}) ->
                  astranaut:walk_return(#{warning => mark_1, state => Acc + 1, return => Node});
             ({atom, _Pos, mark_error_1}, Acc, #{}) ->
                  {{atom, _Pos, mark_error_2}, Acc};
             (Node, Acc, #{}) ->
                  {Node, Acc}
          end, 0, Forms, #{formatter => ?MODULE, traverse => pre, simplify_return => false}),
    FileWarnings = [{File, [{5, ?MODULE, mark_1}]}],
    #{'__struct__' := ?RETURN_OK, error := Error, return := _Return} = ReturnM,
    {[], ActualFileWarnings} = astranaut_test_lib:realize_with_baseline(Baseline, Error),
    ?assertMatch(FileWarnings, ActualFileWarnings),
    astranaut_test_lib:assert_formatted_messages([{Line, Formatter, Warning}
                                                  || {_File, Warnings} <- ActualFileWarnings,
                                                     {Line, Formatter, Warning} <- Warnings]),
    ok.

test_map_spec(_Config) ->
    Nodes = {attribute,56,spec,
             {{test_ok,0},[{type,56,'fun',[{type,56,product,[]},{atom,56,ok}]}]}},
    Nodes1 =
        astranaut:smap(
          fun(Node, #{}) ->
                  Node
          end, Nodes, #{traverse => post}),
    ?assertEqual(Nodes, Nodes1),
    ok.

test_map_type(_Config) ->
    Nodes = {attribute,21,type,{test,{type,21,record,[{atom,21,test}]},[{var, 21, 'A'}]}},
    Nodes1 =
        astranaut:smap(
          fun(Node) ->
                  Node
          end, Nodes, #{traverse => post}),
    ?assertEqual(Nodes, Nodes1),
    ok.

test_reduce_attr(Config) ->
    Forms = proplists:get_value(forms, Config),
    Baseline = astranaut_test_lib:get_baseline(mark_base, Forms),
    File = astranaut_lib:analyze_forms_file(Forms),
    ReturnM =
        astranaut:reduce(
          fun({attribute, _Pos, mark, mark_0}, Acc, #{}) ->
                  astranaut:walk_return(#{warning => mark_0, state => Acc + 1});
             ({attribute, _Pos, mark, mark_error_0}, _Acc, #{}) ->
                  {error, mark_error_0};
             (_Node, Acc, #{}) ->
                  Acc
          end, 0, Forms, #{formatter => ?MODULE, traverse => none}),
    #{'__struct__' := ?RETURN_OK, error := Error} = ReturnM,
    io:format("get printable errors ~p~n", [astranaut_error:printable(Error)]),
    FileWarnings = [{File, [{2, ?MODULE, mark_0}]}],
    FileErrors = [{File, [{1, ?MODULE, mark_error_0}]}],
    {ActualFileErrors, ActualFileWarnings} = astranaut_test_lib:realize_with_baseline(Baseline, Error),
    ?assertMatch({FileErrors, FileWarnings}, {ActualFileErrors, ActualFileWarnings}),
    astranaut_test_lib:assert_formatted_messages([{Line, Formatter, Error1}
                                                  || {_File, Errors} <- ActualFileErrors,
                                                     {Line, Formatter, Error1} <- Errors]),
    astranaut_test_lib:assert_formatted_messages([{Line, Formatter, Warning}
                                                  || {_File, Warnings} <- ActualFileWarnings,
                                                     {Line, Formatter, Warning} <- Warnings]),
    ok.

test_with_formatter(_Config) ->
    MA =
        astranaut_traverse:with_formatter(
          sample_transformer_1,
          astranaut_traverse:update_pos(
            10,
            astranaut_traverse:astranaut_traverse(
              astranaut:walk_return(#{return => 10, error => error_0})
             ))),
    #{error := Error} = astranaut_traverse:run(MA, formatter_0, #{}, ok),
    FormattedErrors = astranaut_error:formatted_errors(Error),
    ?assertMatch([{10, sample_transformer_1, error_0}], FormattedErrors),
    astranaut_test_lib:assert_formatted_messages(FormattedErrors),
    ok.

test_options(_Config) ->
    Return = #{a => true, e => true},
    Warnings = [{invalid_option_value, {b, c, d}}],
    OptionsMap = astranaut_lib:option_map([a, {b, c, d}, e]),
    ?assertMatch({just, Return}, astranaut_return:run(OptionsMap)),
    ?assertMatch(Warnings, astranaut_error:warnings(astranaut_return:run_error(OptionsMap))),
    ?assertMatch([], astranaut_error:errors(astranaut_return:run_error(OptionsMap))),
    ok.

test_validator(_Config) ->
    Validator = #{a => boolean,
                  b => {list_of, atom},
                  c => fun is_boolean/1,
                  d => {default, 10},
                  f => [boolean, {default_key, g}],
                  g => [boolean, {default, false}]
                 },
    Validated = astranaut_lib:validate(Validator, [a, {b,[c,d]}, {b, c, d}, e]),
    Return = #{a => true, b => [c, d], d => 10, f => false, g => false},
    Warnings = [{invalid_option_value, {b, c, d}}, {unexpected_option_keys, [e]}],
    ?assertMatch(Warnings, astranaut_error:warnings(astranaut_return:run_error(Validated))),
    ?assertMatch([], astranaut_error:errors(astranaut_return:run_error(Validated))),
    ?assertMatch({just, Return}, astranaut_return:run(Validated)),
    ok.

test_validator_failure_contracts(_Config) ->
    InvalidReturnValidator = fun(_Value) -> bad_return end,
    FailureCases =
        [
         {unknown_validator, ok,
          {invalid_validator, unknown_validator}},
         {InvalidReturnValidator, ok,
          {invalid_validator_return, InvalidReturnValidator, bad_return}},
         {uinteger, -1,
          {invalid_value, uinteger}},
         {{one_of, [one, two]}, three,
          {invalid_value, {one_of, [one, two]}}},
         {{one_of, not_a_list}, one,
          {invalid_validator_arg, {one_of, not_a_list}}},
         {{list_of, atom}, [one, 2],
          {invalid_value, atom}},
         {{list_of, atom}, not_a_list,
          {invalid_value, {list_of, atom}}},
         {paired, not_boolean,
          {invalid_value, paired}},
         {{paired, 42}, true,
          {invalid_validator_arg, {paired, 42}}},
         {{'or', [integer, boolean]}, atom_value,
          {all_validator_failed, [integer, boolean]}}
        ],
    lists:foreach(
      fun({Validator, Value, ExpectedReason}) ->
              Return = astranaut_lib:validate(#{value => Validator},
                                              [{value, Value}]),
              ?assertEqual({just, #{}},
                           astranaut_return:run(Return)),
              ?assertEqual(
                 [{validate_key_failure, ExpectedReason, value, Value}],
                 astranaut_error:errors(
                   astranaut_return:run_error(Return)))
      end, FailureCases),

    Required = astranaut_lib:validate(#{value => required}, []),
    ?assertEqual(
       [{validate_key_failure, required, value, undefined}],
       astranaut_error:errors(astranaut_return:run_error(Required))),

    InvalidReverse =
        astranaut_lib:validate(#{value => paired},
                               [{no_value, not_boolean}]),
    ?assertEqual(
       [{validate_key_failure, {invalid_value, paired}, value, undefined}],
       astranaut_error:errors(
         astranaut_return:run_error(InvalidReverse))),
    ?assertEqual(
       {just, #{value => false}},
       astranaut_return:run(
         astranaut_lib:validate(#{value => paired}, [no_value]))),
    ?assertEqual(
       {just, #{value => true}},
       astranaut_return:run(
         astranaut_lib:validate(
           #{value => {'or', [integer, boolean]}},
           [{value, true}]))),

    KeepWarning = fun(_Value) -> {warning, kept_value_warning} end,
    ChangeWarning =
        fun(_Value) ->
                {warning, normalized, normalized_value_warning}
        end,
    KeepWarned =
        astranaut_lib:validate(
          #{keep => KeepWarning}, [{keep, original}]),
    ?assertEqual(
       {just, #{keep => original}},
       astranaut_return:run(KeepWarned)),
    ?assertEqual(
       [{validate_key_failure, kept_value_warning, keep, original}],
       astranaut_error:warnings(
         astranaut_return:run_error(KeepWarned))),
    ChangeWarned =
        astranaut_lib:validate(
          #{change => ChangeWarning}, [{change, original}]),
    ?assertEqual(
       {just, #{change => normalized}},
       astranaut_return:run(ChangeWarned)),
    ?assertEqual(
       [{validate_key_failure, normalized_value_warning,
         change, original}],
       astranaut_error:warnings(
         astranaut_return:run_error(ChangeWarned))),

    ?assertException(
       exit, {deps_key_not_exists, missing},
       astranaut_lib:validate(
         #{value => {default_key, missing}}, [])),
    ?assertException(
       exit, {cycle_deps_detected, _},
       astranaut_lib:validate(
         #{first => {default_key, second},
           second => {default_key, first}}, [])),
    ok.

test_invalid_validator_return_format(_Config) ->
    Message =
        astranaut:format_error(
          {validate_key_failure,
           {invalid_validator_return, my_validator, bad_return},
           my_key, my_value}),
    ?assertEqual(
       "validator my_validator for option key my_key returns a invalid_value bad_return",
       lists:flatten(Message)),
    ok.

test_format_error_unknown_default(_Config) ->
    Unknown = {unknown_format_error, [term]},
    ?assertEqual(io_lib:write(Unknown), astranaut:format_error(Unknown)),
    ok.

test_format_error_unknown_throw(_Config) ->
    Unknown = {unknown_format_error, [term]},
    ?assertException(throw, Unknown,
                     astranaut:format_error(Unknown, #{default => throw})),
    ok.

test_format_error_known_throw_option(_Config) ->
    Error =
        {validate_key_failure,
         {invalid_validator_return, my_validator, bad_return},
         my_key, my_value},
    Message = astranaut:format_error(Error, #{default => throw}),
    ?assertEqual(
       "validator my_validator for option key my_key returns a invalid_value bad_return",
       lists:flatten(Message)),
    ok.


test_with_attribute(Config) ->
    Forms = proplists:get_value(forms, Config),
    Marks =
        astranaut_return:run(
          astranaut_lib:with_attribute(
            fun(Attr, Acc) ->
                    [Attr|Acc]
            end, [], Forms, mark, #{})),
    ?assertEqual({just, [mark_0, mark_error_0]}, Marks).

test_forms_with_attribute(Config) ->
    Forms = proplists:get_value(forms, Config),
    {just, {Forms1, Marks}} =
        astranaut_return:run(
          astranaut_lib:forms_with_attribute(
            fun(Attr, Acc, #{pos := Pos}) ->
                    Node = astranaut_lib:gen_attribute_node(mark_1, Pos, Attr),
                    {[Node], [Attr|Acc]}
            end, [], Forms, mark, #{})),
    ?assertEqual([mark_0, mark_error_0], Marks),
    {just, Marks1} =
        astranaut_return:run(
          astranaut_lib:with_attribute(
            fun(Attr, Acc) ->
                    [Attr|Acc]
            end, [], Forms1, mark_1, #{})),
    ?assertEqual([mark_0, mark_error_0], Marks1),
    ok.

test_traverse_m_updated(Config) ->
    Forms = proplists:get_value(forms, Config),
    TraverseM =
        astranaut:map_m(
          fun(Node) ->
                  astranaut_traverse:return(Node)
          end, Forms, #{traverse => post}),
    TraverseM1 =
        astranaut_traverse:lift_m(
          fun({Return, Updated}) ->
                  ?assertEqual({Return, false}, {Forms, Updated}),
                  Return
          end, astranaut_traverse:listen_updated(TraverseM)),
    astranaut_traverse:eval(TraverseM1, astranaut, #{}, #{}).

test_map_m_preserves_form_order(_Config) ->
    Forms = [astranaut_lib:gen_function(test, ?Q(["fun() -> ok end"])),
             {attribute, 1, custom, value}],
    Identity = fun(Form) -> astranaut_traverse:return(Form) end,
    Forms1 = astranaut_return:simplify(
               astranaut_traverse:eval(
                 astranaut:map_m(
                   Identity, Forms, #{traverse => none}),
                 astranaut, #{}, ok)),
    ?assertEqual(Forms, Forms1),
    ok.

test_map_forms(Config) ->
    Forms = astranaut_test_lib:test_module_forms(sample_2, Config),
    Forms1M = 
        astranaut:map_m_forms(
          fun({attribute, _Pos, mark, mark_01}) ->
                  astranaut_traverse:return(
                    astranaut_lib:gen_function(
                      test,
                      ?Q(["fun(ok_1) ->",
                          "   ok_1;"
                          "(Other) ->",
                          "   '__original__'(Other)",
                          "end"])));
             (Node) ->
                  astranaut_traverse:return(Node)
          end, Forms, #{traverse => none}),
    Forms1 = astranaut_return:simplify(astranaut_traverse:eval(Forms1M, astranaut, #{}, ok)),
    io:format("~s~n", [astranaut_lib:ast_to_string(Forms1)]),
    Result = astranaut_test_lib:compile_test_forms(Forms1),
    astranaut_return:with_error(
      fun(ErrorState) ->
              ?assertEqual(#{}, astranaut_error:printable(ErrorState))
      end, Result),
    Value1 = sample_2:test(ok_1),
    Value2 = sample_2:test(ok_2),
    Value3 = sample_2:test(ok_3),
    Value4 = sample_2:test(ok_4),
    ?assertEqual({ok_1, ok_2, ok_3, ok_4}, {Value1, Value2, Value3, Value4}),
    ok.

test_sequence_nodes(_Config) ->
    Nodes = [{atom, 1, a}, {atom, 1, b}, {atom, 1, c}, {atom, 1, d}],
    NodeMs = lists:map(
               fun({_Type, _Pos, a} = Node) ->
                       astranaut_traverse:return(Node);
                  ({_Type, _Pos, b} = Node) ->
                       astranaut_traverse:return([Node, Node]);
                  ({_Type, _Pos, c}) ->
                       astranaut_traverse:then(
                         astranaut_traverse:error({invalid, c}),
                         astranaut_traverse:return([]));
                  ({_Type, _Pos, d} = Node) ->
                       astranaut_traverse:then(
                         astranaut_traverse:warning({suspecious, d}),
                         astranaut_traverse:return(Node))
               end, Nodes),
    NodesM = astranaut_traverse:sequence_m(NodeMs),
    Return = astranaut_traverse:eval(NodesM, astranaut, #{}, ok),
    astranaut_return:with_error(
      fun(ErrorStruct) ->
              ?assertEqual(#{errors => [{invalid, c}], warnings => [{suspecious, d}]}, astranaut_error:printable(ErrorStruct))
      end, Return),
    ?assertEqual({just, [{atom, 1, a}, [{atom, 1, b}, {atom, 1, b}], [], {atom, 1, d}]}, astranaut_return:run(Return)),
    ok.

test_continue_sequence_children(_Config) ->
    TopNode = {tuple, 1, [{match, 1, {var, 1, 'Var'}, {tuple, 1, [{atom, 1, a}, {atom, 1, b}]}}, {atom, 1, c}]},
    Monad =
        astranaut:map_m(
          fun({match, _Pos, _Left, _Right} = Match) ->
                  Sequence = fun lists:reverse/1,
                  Reduce = fun lists:reverse/1,
                  astranaut_traverse:return(astranaut_uniplate:with_subtrees(Sequence, Reduce, Match));
             ({atom, _Pos, AtomValue} = Atom) ->
                  astranaut_traverse:then(
                    astranaut_traverse:modify(
                      fun(Acc) ->
                              [AtomValue|Acc]
                      end),
                    astranaut_traverse:return(Atom));
             ({var, _Pos, VarName} = Var) ->
                  astranaut_traverse:then(
                    astranaut_traverse:modify(
                      fun(Acc) ->
                              [VarName|Acc]
                      end),
                    astranaut_traverse:return(Var));
             (Node) ->
                  astranaut_traverse:return(Node)
          end, TopNode, #{traverse => pre, role => expression}),
    Monad1 =
        astranaut:map_m(
          fun({atom, _Pos, AtomValue} = Atom) ->
                  astranaut_traverse:then(
                    astranaut_traverse:modify(
                      fun(Acc) ->
                              [AtomValue|Acc]
                      end),
                    astranaut_traverse:return(Atom));
             ({var, _Pos, VarName} = Var) ->
                  astranaut_traverse:then(
                    astranaut_traverse:modify(
                      fun(Acc) ->
                              [VarName|Acc]
                      end),
                    astranaut_traverse:return(Var));

             (Node) ->
                  astranaut_traverse:return(Node)
          end, TopNode, #{traverse => pre, role => expression}),
    Return = astranaut_traverse:exec(Monad, astranaut, #{}, []),
    Return1 = astranaut_traverse:exec(Monad1, astranaut, #{}, []),
    ?assertEqual({just, [c, 'Var', b, a]}, astranaut_return:run(Return)),
    ?assertEqual({just, [c, b, a, 'Var']}, astranaut_return:run(Return1)),
    ok.

test_record(Config) ->
    Expressions = proplists:get_value(expressions, Config),
    Records = maps:get(record, Expressions),
    {match, _, _, Record1} = lists:nth(1, Records),
    Record1_0 = astranaut_lib:replace_pos(Record1, 0),
    ?assertEqual({record,0, test, [{record_field, 0, {atom,0,a},{integer,0,1}}, {record_field, 0, {atom,0,b},{integer,0,2}}]}, Record1_0),
    {match, _, _, Record2} = lists:nth(2, Records),
    Record2_0 = astranaut_lib:replace_pos(Record2, 0),
    ?assertEqual({record,0, {var,0,'A'}, test, [{record_field, 0, {atom,0,a},{integer,0,2}}, {record_field, 0, {atom,0,b},{integer,0,3}}]}, Record2_0),
    {match, _, _, Record3} = lists:nth(3, Records),
    Record3_0 = astranaut_lib:replace_pos(Record3, 0),
    ?assertEqual({record_index, 0, test, {atom, 0, a}}, Record3_0),
    {match, _, _, Record4} = lists:nth(4, Records),
    Record4_0 = astranaut_lib:replace_pos(Record4, 0),
    ?assertEqual({record_field, 0, {var, 0, 'A'}, test, {atom, 0, a}}, Record4_0),
    ok.

test_typed_record_field(_Config) ->
    TypedField =
        {typed_record_field,
         {record_field, 1, {atom, 1, field}},
         {type, 1, integer, []}},
    check_node_without_tree(TypedField),
    ?assertEqual(
       {typed_record_field,
        {record_field, 0, {atom, 0, field}},
        {type, 0, integer, []}},
       astranaut_lib:replace_pos(TypedField, 0)),
    ok.

test_map(Config) ->
    Expressions = proplists:get_value(expressions, Config),
    Maps = maps:get(map, Expressions),
    {match, _, _, Map1} = lists:nth(1, Maps),
    Map1_0 = astranaut_lib:replace_pos(Map1, 0),
    ?assertEqual({map, 0 ,[{map_field_assoc, 0, {atom, 0, a}, {integer, 0, 1}}, {map_field_assoc, 0, {atom, 0, b}, {integer, 0, 2}}] }, Map1_0),
    ok.

test_if_expr(Config) ->
    Expressions = proplists:get_value(expressions, Config),
    IfExpr = lists:nth(1, maps:get(if_expr, Expressions)),
    ?assertEqual(if_expr, erl_syntax:type(IfExpr)),
    check_node_without_tree(IfExpr),
    ok.

test_case_expr(Config) ->
    Expressions = proplists:get_value(expressions, Config),
    CaseExpr = lists:nth(1, maps:get(case_expr, Expressions)),
    ?assertEqual(case_expr, erl_syntax:type(CaseExpr)),
    check_node_without_tree(CaseExpr),
    ok.

test_try_catch_expr(Config) ->
    Expressions = proplists:get_value(expressions, Config),
    TryCatchExpr = lists:nth(1, maps:get(try_catch_expr, Expressions)),
    ?assertEqual(try_expr, erl_syntax:type(TryCatchExpr)),
    check_node_without_tree(TryCatchExpr),
    ok.

test_subtree_callback_without_tree(_Config) ->
    Clause =
        {clause, 1, [],
         [[{op, 1, '>', {integer, 1, 2}, {integer, 1, 1}}]],
         [{atom, 1, ok}]},
    ?assertEqual(
       Clause,
       astranaut:smap(
         fun(Node) ->
                 case element(1, Node) of
                     tree -> exit({unexpected_tree_node, Node});
                     _ -> Node
                 end
         end, Clause, #{traverse => subtree, role => clause})),
    ok.

check_node_without_tree(TopAst) ->
    astranaut:mapfold(
      fun(Node, [Parent|T], #{step := pre}) ->
              case element(1, Node) of
                  tree ->
                      exit({unexpected_tree_node, Node, Parent});
                  _ ->
                      {Node, [Node, Parent|T]}
              end;
         (Node, [], #{step := pre}) ->
              {Node, [Node]};
         (Node, [Node|T], #{step := post}) ->
              {Node, T};
         (Leaf, State, #{step := leaf}) ->
              {Leaf, State}
      end, [], TopAst, #{traverse => all}).
