%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2021, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  9 Apr 2021 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_uniplate_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

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
    [test_writer_or, test_map, test_map_attr,
     test_reduce, test_reduce_attr, test_reduce_traverse_all,
     test_mapfold_attr, test_f_return_list, test_all_return_list].

%%--------------------------------------------------------------------
%% @spec TestCase() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
my_test_case() -> 
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
test_writer_or(_Config) ->
    Monad = identity,
    Mempty = astranaut_monad:mempty('or'),
    Mappend = astranaut_monad:mappend('or'),
    Bind = astranaut_monad:monad_bind(Monad),
    Return = astranaut_monad:monad_return(Monad),
    BindW = astranaut_monad:writer_bind(Bind, Return, Mappend),
    ReturnW = astranaut_monad:writer_return(Return, Mempty),
    Writer = astranaut_monad:writer_writer(Return),
    WA =
        BindW(
          ReturnW(1),
          fun(A) ->
                  Writer({A + 1, true})
          end),
    ?assertEqual({2, true}, WA),
    Monad1 = reader,
    Bind1 = astranaut_monad:monad_bind(Monad1),
    Return1 = astranaut_monad:monad_return(Monad1),
    BindW1 = astranaut_monad:writer_bind(Bind1, Return1, Mappend),
    ReturnW1 = astranaut_monad:writer_return(Return1, Mempty),
    Writer1 = astranaut_monad:writer_writer(Return1),
    Lift = astranaut_monad:writer_lift(Bind1, Return1, Mempty),
    Ask1 = astranaut_monad:writer_ask(Lift, astranaut_monad:monad_ask(Monad1)),
    WA1 =
        BindW1(
          ReturnW1(1),
          fun(A) ->
                  BindW1(
                    Ask1(),
                    fun(B) ->
                            Writer1({A + B, true})
                    end)
          end),
    ?assertEqual({4, true}, WA1(3)),
    ok.

test_map(_Config) ->
    Ast = merl:quote("A + (B + C)"),
    Ast1 =
        astranaut:smap(
          fun({var, _Pos, Varname}) ->
                  {var, _Pos, list_to_atom(atom_to_list(Varname) ++ "_1")};
             (_Node) ->
                  keep
          end, Ast, #{traverse => pre}),
    Ast2 = merl:quote("A_1 + (B_1 + C_1)"),
    ?assertEqual(Ast2, Ast1),
    ok.

test_map_attr(_Config) ->
    Ast = merl:quote("E = A + (D = B + C)"),
    Ast1 =
        astranaut:smap(
          fun({var, _Pos, Varname}, #{pattern := true}) ->
                  {var, _Pos, list_to_atom(atom_to_list(Varname) ++ "_1")};
             (Node, #{}) ->
                  case erl_syntax:type(Node) of
                      match_expr ->
                          astranaut_uniplate:with_subtrees(
                            fun([MatchLeft, MatchRight]) ->
                                    [MatchRight, astranaut_uniplate:up_attr(#{pattern => true}, MatchLeft)]
                            end, fun lists:reverse/1);
                      _Type ->
                          keep
                  end
          end, Ast, #{traverse => pre}),
    Ast2 = merl:quote("E_1 = A + (D_1 = B + C)"),
    ?assertEqual(Ast2, Ast1),
    ok.

test_reduce(_Config) ->
    Ast = merl:quote("E = A + (D = B + C)"),
    Acc1 =
        astranaut:sreduce(
          fun({var, _Pos, Varname}, Acc) ->
                  [Varname|Acc];
             (_Node, Acc) ->
                  Acc
          end, [], Ast, #{traverse => pre}),
    ?assertEqual(['E', 'A', 'D', 'B', 'C'], lists:reverse(Acc1)),
    ok.

test_reduce_attr(_Config) ->
    Ast = merl:quote("E = A + (D = B + C)"),
    Acc1 =
        astranaut_uniplate:reduce(
          fun(#{node := {var, _Pos, Varname}, pattern := true}, Acc) ->
                  [Varname|Acc];
             (_Node, Acc) ->
                  Acc
          end, [], Ast, fun uniplate_node_attr/1, #{traverse => pre}),
    ?assertEqual(['E', 'D'], lists:reverse(Acc1)),
    ok.

uniplate_node_attr(#{node := Node}) ->
    uniplate_node_attr(Node);
uniplate_node_attr(Node) ->
    case erl_syntax:type(Node) of
        match_expr ->
            {[MatchLefts, MatchRights], Context} = astranaut:uniplate(Node),
            MatchLefts1 =
                lists:map(
                  fun(MatchLeft) ->
                          #{node => MatchLeft, pattern => true}
                  end, MatchLefts),
            {[MatchLefts1, MatchRights], Context};
        _ ->
            astranaut:uniplate(Node)
    end.

test_reduce_traverse_all(_Config) ->
    Ast = merl:quote("E = A + (D = B + C)"),
    Acc1 =
        astranaut:sreduce(
          fun(Node, Acc, #{step := Step}) ->
                  case erl_syntax:type(Node) of
                      match_expr ->
                          {var, _Pos, Var} = erl_syntax:match_expr_pattern(Node),
                          [{Step, Var}|Acc];
                      _ ->
                          Acc
                  end
          end, [], Ast, #{traverse => all}),
    ?assertEqual([{pre, 'E'}, {pre, 'D'}, {post, 'D'}, {post, 'E'}], lists:reverse(Acc1)),
    ok.

test_mapfold(_Config) ->
    Ast = merl:quote("E = A + (D = B + C)"),
    {Ast1, Acc1} =
        astranaut:smapfold(
          fun({var, Pos, Varname}, Acc) ->
                  {{var, Pos, list_to_atom(atom_to_list(Varname) ++ "_1")}, Acc};
             (Node, Acc) ->
                  case erl_syntax:type(Node) of
                      match_expr ->
                          {var, _Pos, Var} = erl_syntax:match_expr_pattern(Node),
                          {keep, [Var|Acc]};
                      _ ->
                          {keep, Acc}
                  end
          end, [], Ast, #{traverse => post}),
    Ast2 = merl:quote("E_1 = A_1 + (D_1 = B_1 + C_1)"),
    ?assertEqual(Ast2, Ast1),
    ?assertEqual(['D_1', 'E_1'], lists:reverse(Acc1)),
    ok.

test_mapfold_attr(_Config) ->
    Ast = merl:quote("E = A + (D = B + C)"),
    {Ast1, Acc1} =
        astranaut:smapfold(
          fun({var, Pos, Varname}, Acc, #{step := pre}) ->
                  {{var, Pos, list_to_atom(atom_to_list(Varname) ++ "_1")}, Acc};
             (Node, Acc, #{step := Step}) ->
                  case erl_syntax:type(Node) of
                      match_expr ->
                          {var, _Pos, Var} = erl_syntax:match_expr_pattern(Node),
                          {keep, [{Step, Var}|Acc]};
                      _ ->
                          {keep, Acc}
                  end;
             (_Node, Acc, #{})->
                  {keep, Acc}
          end, [], Ast, #{traverse => all}),
    Ast2 = merl:quote("E_1 = A_1 + (D_1 = B_1 + C_1)"),
    ?assertEqual(Ast2, Ast1),
    ?assertEqual([{pre, 'E'}, {pre, 'D'}, {post, 'D_1'}, {post, 'E_1'}], lists:reverse(Acc1)),
    ok.

test_f_return_list(_Config) ->
    Ast = merl:quote("hello(A, B, world(C))"),
    Ast1 =
        astranaut:smap(
          fun({var, Pos, Varname}) ->
                  [{var, Pos, list_to_atom(atom_to_list(Varname) ++ "_1")},
                   {var, Pos, list_to_atom(atom_to_list(Varname) ++ "_2")}];
             (_Node) ->
                  keep
          end, Ast, #{traverse => post}),
    Ast2 = merl:quote("hello(A_1, A_2, B_1, B_2, world(C_1, C_2))"),
    ?assertEqual(Ast2, Ast1),
    ok.

test_all_return_list(_Config) ->
    Ast = merl:quote("hello(A, B, world(C))"),
    Ast1 =
        astranaut:smap(
          fun({var, Pos, Varname}) ->
                  [{var, Pos, list_to_atom(atom_to_list(Varname) ++ "_1")},
                   {var, Pos, list_to_atom(atom_to_list(Varname) ++ "_2")}];
             (_Node) ->
                  keep
          end, Ast, #{traverse => all}),
    Ast2 = merl:quote("hello(A_1_1, A_1_2, A_2_1, A_2_2, B_1_1, B_1_2, B_2_1, B_2_2, world(C_1_1, C_1_2, C_2_1, C_2_2))"),
    ?assertEqual(Ast2, Ast1),
    ok.
