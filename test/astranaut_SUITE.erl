%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2018, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  8 Dec 2018 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("astranaut/include/astranaut_struct_name.hrl").

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
    [test_reduce, test_map_with_state_node, test_map_with_state, test_map_spec, test_map_type,
     test_reduce_attr, test_with_formatter, 
     test_options, test_validator, test_with_attribute, test_forms_with_attribute].

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
test_reduce(_Config) ->
    Forms = astranaut_sample_0:forms(),
    File = astranaut:file(Forms),
    ReturnM =
        astranaut_traverse:reduce(
          fun({atom, _Line, mark_1} = Node, Acc, #{}) ->
                  astranaut_walk_return:new(#{warning => mark_1, state => Acc + 1, node => Node});
             ({atom, _Line, mark_error_1}, _Acc, #{}) ->
                  {error, mark_error_1};
             (_Node, Acc, #{}) ->
                  Acc
          end, 0, Forms, #{formatter => ?MODULE, traverse => pre}),
    #{'__struct__' := astranaut_return_fail, error := Error} = ReturnM,
    FileWarnings = [{File, [{26, ?MODULE, mark_1}]}],
    FileErrors = [{File, [{23, ?MODULE, mark_error_1}]}],
    io:format("errors is ~p~n", [astranaut_error_state:run_endo(Error)]),
    ?assertMatch({FileErrors, FileWarnings},
                 astranaut_error_state:realize(Error)),
    ok.

test_map_with_state_node(_Config) ->
    NodeA = {match, 10, {var, 10, 'A'}, {atom, 10, a}},
    Return =
        astranaut_traverse:map_with_state(
          fun({var, Line, 'A'}, Acc, #{}) ->
                  Node1 = {var, Line, 'B'},
                  astranaut_walk_return:new(#{state => Acc + 1, node => Node1});
             (Node, Acc, #{}) ->
                  {Node, Acc}
          end, 0, NodeA, #{formatter => ?MODULE, traverse => pre}),
    ?assertEqual({match, 10, {var, 10, 'B'}, {atom, 10, a}}, Return),
    ok.

test_map_with_state(_Config) ->
    Forms = astranaut_sample_0:forms(),
    File = astranaut:file(Forms),
    ReturnM =
        astranaut_traverse:map_with_state(
          fun({atom, _Line, mark_1} = Node, Acc, #{}) ->
                  io:format("Acc is ~p~n", [Acc]),
                  astranaut_walk_return:new(#{warning => mark_1, state => Acc + 1, node => Node});
             ({atom, _Line, mark_error_1}, Acc, #{}) ->
                  {{atom, _Line, mark_error_2}, Acc};
             (Node, Acc, #{}) ->
                  {Node, Acc}
          end, 0, Forms, #{formatter => ?MODULE, traverse => pre}),
    FileWarnings = [{File, [{26, ?MODULE, mark_1}]}],
    #{'__struct__' := astranaut_return_ok, error := Error, return := Return} = ReturnM,
    io:format("return is ~p~n", [Return]),
    ?assertMatch({[], FileWarnings}, astranaut_error_state:realize(Error)),
    ok.

test_map_spec(_Config) ->
    Nodes = {attribute,56,spec,
             {{test_ok,0},[{type,56,'fun',[{type,56,product,[]},{atom,56,ok}]}]}},
    Nodes1 =
        astranaut_traverse:map(
          fun(Node, #{}) ->
                  Node
          end, Nodes, #{traverse => post}),
    ?assertEqual(Nodes, Nodes1),
    ok.

test_map_type(_Config) ->
    Nodes = {attribute,21,type,{test,{type,21,record,[{atom,21,test}]},[{var, 21, 'A'}]}},
    Nodes1 =
        astranaut_traverse:map(
          fun(Node, #{} = Attr) ->
                  io:format("node is ~p ~p~n", [Node, Attr]),
                  Node
          end, Nodes, #{traverse => post}),
    ?assertEqual(Nodes, Nodes1),
    ok.

test_reduce_attr(_Config) ->
    Forms = astranaut_sample_0:forms(),
    File = astranaut:file(Forms),
    ReturnM =
        astranaut_traverse:reduce(
          fun({attribute, _Line, mark, mark_0} = Node, Acc, #{}) ->
                  astranaut_walk_return:new(#{warning => mark_0, state => Acc + 1, node => Node});
             ({attribute, _Line, mark, mark_error_0}, _Acc, #{}) ->
                  {error, mark_error_0};
             (_Node, Acc, #{}) ->
                  Acc
          end, 0, Forms, #{formatter => ?MODULE, traverse => list}),
    #{'__struct__' := ?RETURN_OK, error := Error} = ReturnM,
    FileWarnings = [{File, [{17, ?MODULE, mark_0}]}],
    FileErrors = [{File, [{16, ?MODULE, mark_error_0}]}],
    ?assertMatch({FileErrors, FileWarnings}, astranaut_error_state:realize(Error)),
    ok.

test_with_formatter(_Config) ->
    MA =
        astranaut_traverse_m:with_formatter(
          formatter_1,
          astranaut_traverse_m:update_line(
            10,
            astranaut_traverse_m:astranaut_traverse_m(
              astranaut_walk_return:new(#{return => 10, error => error_0})
             ))),
    #{error := Error} = astranaut_traverse_m:run(MA, formatter_0, ok),
    ?assertMatch([{10, formatter_1, error_0}], astranaut_error_state:formatted_errors(Error)),
    ok.

test_options(_Config) ->
    Return = #{a => true, e => true},
    Warnings = [{invalid_option_value, {b, c, d}}],
    ?assertMatch(#{return := Return, warnings := Warnings}, astranaut_options:options([a, {b, c, d}, e])),
    ok.

test_validator(_Config) ->
    Validator = #{a => boolean,
                  b => {list_of, atom},
                  c => fun is_boolean/1,
                  d => {default, 10}},
    BaseM = astranaut_options:validate(Validator, [a, {b,[c,d]}, {b, c, d}, e], #{}),
    Return = #{a => true, b => [c, d], d => 10},
    Warnings = [{invalid_option_value, {b, c, d}}],
    ?assertMatch(#{return := Return, warnings := Warnings}, BaseM),
    ok.

test_with_attribute(_Config) ->
    Forms = astranaut_sample_0:forms(),
    Marks =
        astranaut_options:with_attribute(
          fun(Attr, Acc) ->
                  [Attr|Acc]
          end, [], Forms, mark, #{simplify_return => true}),
    ?assertEqual([mark_0, mark_error_0], Marks).

test_forms_with_attribute(_Config) ->
    Forms = astranaut_sample_0:forms(),
    {Forms1, Marks} =
        astranaut_options:forms_with_attribute(
          fun(Attr, Acc, #{line := Line}) ->
                  Node = astranaut:attribute_node(mark_1, Line, Attr),
                  {[Node], [Attr|Acc]}
          end, [], Forms, mark, #{simplify_return => true}),
    Marks1 =
        astranaut_options:with_attribute(
          fun(Attr, Acc) ->
                  [Attr|Acc]
          end, [], Forms1, mark_1, #{simplify_return => true}),
    ?assertEqual([mark_0, mark_error_0], Marks1),
    ?assertEqual([mark_0, mark_error_0], Marks),
    ok.
