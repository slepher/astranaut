%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2021, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  2 Apr 2021 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut).

-include("astranaut_struct_name.hrl").

%% API
-export([smap/3, sreduce/4, smapfold/4]).
-export([map/3, reduce/4, mapfold/4]).
-export([map_m/3]).
-export([walk_return/1, traverse_return/1]).
-export([uniplate/1]).
-export([format_error/1]).

-type tree()   :: erl_syntax:syntaxTree().
-type trees()  :: tree() | [erl_syntax:syntaxTree()].
-type rtrees() :: astranaut_uniplate:node_context(tree()) | [astranaut_uniplate:node_context(tree())] | ok.

-type traverse_opts() :: #{traverse => traverse_style(),
                           formatter => module(),
                           attr => traverse_attr()}.

-type straverse_opts() :: #{traverse => traverse_style(),
                           attr => traverse_attr()}.

-type traverse_attr() :: map().
-type traverse_style() :: traverse_step() | all | subtree.
-type traverse_step() :: pre | post | leaf.

-type common_walk_return(State, Value) :: astranaut_traverse:struct(State, Value) | astranaut_return:struct(Value) | walk_return(State, Value) | walk_return_tuple(Value).

-type map_walk_return() :: common_walk_return(ok, rtrees()) | rtrees().
-type map_walk() :: map_walk_1() | map_walk_2().
-type map_walk_1() :: fun((tree()) -> map_walk_return()).
-type map_walk_2() :: fun((tree(), traverse_attr()) -> map_walk_return()).

-type reduce_walk_return(State) :: common_walk_return(State, State) | State.
-type reduce_walk(State) :: reduce_walk_2(State) | reduce_walk_3(State).
-type reduce_walk_2(State) :: fun((tree(), State) -> reduce_walk_return(State)).
-type reduce_walk_3(State) :: fun((tree(), State, traverse_attr()) -> reduce_walk_return(State)).

-type mapfold_walk_return(State) :: common_walk_return(State, rtrees()) | {rtrees() | State}.
-type mapfold_walk(State) :: mapfold_walk_2(State) | mapfold_walk_3(State).
-type mapfold_walk_2(State) :: fun((tree(), State) -> mapfold_walk_return(State)).
-type mapfold_walk_3(State) :: fun((tree(), State, traverse_attr()) -> mapfold_walk_return(State)).

-type walk_return(S, A) :: #{?STRUCT_KEY => ?WALK_RETURN,
                             return => A,
                             state => S,
                             node => trees(),
                             errors => [any()],
                             warnings => [any()],
                             continue => boolean()}.

-type walk_return_map(S, A) :: #{return => A,
                                 state => S,
                                 node => trees(),
                                 error => error_term(),
                                 warning => error_term(),
                                 errors => [error_term()],
                                 warnings => [error_term()],
                                 continue => boolean()}.

-type error_term() :: term().

-type walk_return_tuple(A) :: convertable_warning(A) | convertable_error(A) |
                              continue | {continue, A} | {node, A | [A]}.

-type convertable_warning(A) :: {warning, error_term()} | {warnings, [error_term()]} |
                                {warning, A, error_term()} | {warnings, A, [error_term()]}.

-type convertable_error(A) :: {error, error_term()} | {errors, [error_term()]} |
                              {error, A, error_term()} | {errors, A, [error_term()]}.

%%%===================================================================
%%% API
%%%===================================================================
%% @doc
%% works same as map/3 and returns trees(), not astranant_return:struct(trees()).
-spec smap(fun((tree()) -> rtrees()) | fun((tree(), #{}) -> rtrees()), trees(), straverse_opts()) -> trees().
smap(F, Nodes, Opts) ->
    astranaut_uniplate:map(F, Nodes, fun uniplate/1, Opts).

%% @doc
%% works same as reduce/4 and returns S, not astranant_return:struct(S).
-spec sreduce(fun((tree(), S) -> S) | fun((tree(), S, #{}) -> S), S, trees(), straverse_opts()) -> S.
sreduce(F, Init, Nodes, Opts) ->
    astranaut_uniplate:reduce(F, Init, Nodes, fun uniplate/1, Opts).

%% @doc
%% works same as mapfold/4 and returns {trees(), S}, not astranant_return:struct({trees(), S}).
-spec smapfold(fun((tree(), S) -> {rtrees(), S}) | fun((tree(), S, #{}) -> {rtrees(), S}),
               S, trees(), straverse_opts()) -> {trees(), S}.
smapfold(F, Init, Nodes, Opts) ->
    astranaut_uniplate:mapfold(F, Init, Nodes, fun uniplate/1, Opts).

-spec map(map_walk(), rtrees(), traverse_opts()) -> astranant_return:struct(trees()).
%% @doc
%% Takes a function from AstNodeA, {@link traverse_attr()} to AstNodeB, and a TopNodeA and produces a TopNodeB by applying the function to every subtree in the AST. This function is used to obtain the return values.
%% @see mapfold/4
map(F, TopNode, Opts) ->
    WithReturn = fun(_Node, Node1) -> #{return => Node1} end,
    F1 = fun(Node, _State, Attr) ->
                 apply_f(F, Node, Attr)
         end,
    astranaut_return:lift_m(
      fun({TopNode1, _State}) ->
              TopNode1
      end,
      mapfold_1(F1, ok, TopNode, Opts#{with_return => WithReturn})).

-spec reduce(reduce_walk(S), S, trees(), traverse_opts()) -> astranant_return:struct(S).
%% @doc Calls F(AstNode, AccIn, Attr) on successive subtree AstNode of TopNode, starting with AccIn =:= Acc0. F/3 must return a new accumulator, which is passed to the next call. The function returns the final value of the accumulator. Acc0 is returned if the TopNode is empty.
%% @see mapfold/4
reduce(F, Init, TopNode, Opts) ->
    WithReturn = fun(Node, State) -> #{return => Node, state => State} end,
    F1 = fun(Node, State, Attr) ->
                   apply_f_with_state(F, Node, State, Attr)
         end,
    astranaut_return:lift_m(
      fun({_TopNode1, State}) ->
              State
      end,
      mapfold_1(F1, Init, TopNode, Opts#{static => true, with_return => WithReturn})).

-spec mapfold(mapfold_walk(S), S, trees(), traverse_opts()) -> astranant_return:struct({trees(), S}).
%% @doc Combines the operations of map/3 and reduce/4 into one pass.
mapfold(F, Init, TopNode, Opts) ->
    WithReturn =
        fun(_Node, {Node1, State1}) ->
                #{return => Node1, state => State1};
           (_Node, Return) ->
                %% when return other value, we dont know which part is node and which part is state
                %% just throw exception.
                exit({invalid_mapfold_return, Return})
        end,
    mapfold_1(F, Init, TopNode, Opts#{with_return => WithReturn}).

mapfold_1(F, Init, TopNode, #{with_return := WithReturn} = Opts) ->
    Formatter = maps:get(formatter, Opts, ?MODULE),
    InitAttr = maps:get(attr, Opts, #{}),
    Opts1 = maps:without([formatter, attr, uniplate], Opts),
    F1 = fun(Node) ->
                 astranaut_traverse:with_state_attr(
                   fun(State, Attr) ->
                           bind_return(Node, apply_f_with_state(F, Node, State, Attr), WithReturn)
                   end)
         end,
    TopNodeM = map_m(F1, TopNode, Opts1),
    astranaut_traverse:run(TopNodeM, Formatter, InitAttr, Init).

apply_f(F, Node, _Attr) when is_function(F, 1) ->
    F(Node);
apply_f(F, Node, Attr) when is_function(F, 2) ->
    F(Node, Attr).

apply_f_with_state(F, Node, State, _Attr) when is_function(F, 2) ->
    F(Node, State);
apply_f_with_state(F, Node, State, Attr) when is_function(F, 3) ->
    F(Node, State, Attr).

bind_return(_Node, #{?STRUCT_KEY := ?TRAVERSE_M}, _Fun) ->
    exit(unsupported_traverse_struct);
bind_return(Node, #{?STRUCT_KEY := ?RETURN_OK, return := Return} = Struct, Fun) ->
    astranaut_traverse:then(
      astranaut_traverse:astranaut_traverse(Struct),
      bind_return(Node, Return, Fun));
bind_return(_Node, #{?STRUCT_KEY := ?RETURN_FAIL} = Struct, _Fun) ->
    astranaut_traverse:astranaut_traverse(Struct);
bind_return(Node, #{?STRUCT_KEY := ?WALK_RETURN} = WalkReturn, _Fun) ->
    WalkReturn1 = walk_return_up_map(Node, WalkReturn),
    astranaut_traverse:astranaut_traverse(WalkReturn1);
bind_return(Node, Return, Fun) ->
    WalkReturn1 = walk_return(walk_return_map(Return)),
    WalkReturn2 =
        case maps:find(return, WalkReturn1) of
            {ok, Return} ->
                maps:merge(WalkReturn1, Fun(Node, Return));
            error ->
                WalkReturn1
        end,
    bind_return(Node, WalkReturn2, Fun).

%%===================================================================
%% walk return functions
%%===================================================================
-spec walk_return(walk_return_map(S, A) | walk_return_tuple(A) | term()) -> walk_return(S, A).
%% @doc generate walk_return(S, A) from walk function return in
%% {@link map/3} {@link reduce/4}, {@link map_with_state/4}, {@link mapfold/4}
%% @end
walk_return(#{?STRUCT_KEY := ?WALK_RETURN} = Return) ->
    Return;
walk_return(#{} = Map) ->
    Keys = [return, state, error, warning, errors, warnings, continue],
    Map1 = walk_return_up_map(maps:with(Keys, Map)),
    maps:merge(#{?STRUCT_KEY => ?WALK_RETURN, errors => [], warnings => []}, Map1);
walk_return(Return) ->
    walk_return(walk_return_map(Return)).

walk_return_map(#{?STRUCT_KEY := ?WALK_RETURN} = Return) ->
    Return;
walk_return_map({warning, Warning}) ->
    #{warnings => [Warning]};
walk_return_map({warnings, Warnings}) ->
    #{warnings => Warnings};
walk_return_map({warning, A, Warning}) ->
    #{return => A, warnings => [Warning]};
walk_return_map({warnings, A, Warnings}) ->
    #{return => A, warnings => Warnings};
walk_return_map({error, Error}) ->
    #{errors => [Error]};
walk_return_map({errors, Errors}) when is_list(Errors) ->
    #{errors => Errors};
walk_return_map({error, A, Error}) ->
    #{return => A, errors => [Error]};
walk_return_map({errors, A, Errors}) when is_list(Errors) ->
    #{return => A, errors => Errors};
walk_return_map(continue) ->
    #{continue => true};
walk_return_map({continue, A}) ->
    #{continue => true, return => A};
walk_return_map(A) ->
    #{return => A}.

walk_return_up_map(Map) ->
    walk_return_up_map(undefined, Map).

%% update convertable_map to struct map.
walk_return_up_map(_Node, #{errors := Errors}) when not is_list(Errors) ->
    exit({errors_should_be_list, Errors});
walk_return_up_map(_Node, #{warnings := Warnings}) when not is_list(Warnings) ->
    exit({warnings_should_be_list, Warnings});
walk_return_up_map(Node, #{warning := Warning} = Map) ->
    Map1 = maps:remove(warning, Map),
    Warnings = maps:get(warnings, Map, []),
    walk_return_up_map(Node, Map1#{warnings => [Warning|Warnings]});
walk_return_up_map(Node, #{error := Error} = Map) ->
    Map1 = maps:remove(error, Map),
    Errors = maps:get(errors, Map, []),
    walk_return_up_map(Node, Map1#{errors => [Error|Errors]});
walk_return_up_map(_Node, #{continue := true, return := Return} = Map) ->
    maps:remove(continue, Map#{return => astranaut_uniplate:skip(Return)});
walk_return_up_map(Node, #{continue := true} = Map) ->
    maps:remove(continue, Map#{return => astranaut_uniplate:skip(Node)});
walk_return_up_map(undefined, #{} = Map) ->
    Map;
walk_return_up_map(Node, #{} = Map) ->
    case maps:is_key(return, Map) of
        true ->
            Map;
        false ->
            Map#{return => Node}
    end.

traverse_return(#{?STRUCT_KEY := ?RETURN_OK} = Return) ->
    astranaut_traverse:astranaut_traverse(Return);
traverse_return(#{?STRUCT_KEY := ?RETURN_FAIL} = Return) ->
    astranaut_traverse:astranaut_traverse(Return);
traverse_return(#{?STRUCT_KEY := ?TRAVERSE_M} = Traverse) ->
    Traverse;
traverse_return(Return) ->
    astranaut_traverse:astranaut_traverse(walk_return(Return)).

map_m(F, [Node|_T] = Nodes, Opts) ->
    case erl_syntax:is_form(Node) of
        true ->
            astranaut_traverse:lift_m(
              fun astranaut_syntax:reorder_updated_forms/1,
              astranaut_traverse:map_m(
                fun(Form) ->
                        astranaut_traverse:lift_m(
                          fun({Form1, true}) ->
                                  {updated, Form, to_list(Form1)};
                             ({_Form1, false}) ->
                                  Form
                          end, astranaut_traverse:listen_updated(map_form(F, Form, Opts)))
                end, Nodes));
        false ->
            map_m_1(F, Nodes, Opts)
    end;
map_m(F, Node, Opts) ->
    map_m_1(F, Node, Opts).

to_list(Form1) when is_list(Form1) ->
    Form1;
to_list(Form1) ->
    [Form1].

map_form(F, Form, #{traverse := subtree}) ->
    astranaut_traverse:bind(
      traverse_map_node(F, Form),
      fun(ok) ->
              astranaut_traverse:return(Form);
         (Form1) ->
              astranaut_traverse:writer_updated({Form1, Form =/= Form1})
      end);
map_form(F, Form, Opts) ->
    map_m_1(F, Form, Opts).

map_m_1(F, Node, Opts) ->
    Uniplate = maps:get(uniplate, Opts, fun uniplate/1),
    astranaut_uniplate:map_m(
      fun(Node1) ->
              traverse_map_node(F, Node1)
      end, Node, Uniplate, traverse, Opts).

traverse_map_node(F, Node) ->
    Type = erl_syntax:type(Node),
    case Type of
	attribute ->
            Name = erl_syntax:concrete(erl_syntax:attribute_name(Node)),
            case Name of
                file ->
                    [FileTree, _PosTree] = erl_syntax:attribute_arguments(Node),
                    File = erl_syntax:concrete(FileTree),
                    astranaut_traverse:then(
                      astranaut_traverse:update_file(File),
                      astranaut_traverse:return(Node));
                _ ->
                    Pos = erl_syntax:get_pos(Node),
                    astranaut_traverse:update_pos(Pos, F(Node))
            end;
        function ->
            Pos = erl_syntax:get_pos(Node),
            astranaut_traverse:update_pos(Pos, F(Node));
	eof_marker ->
            astranaut_traverse:then(
              astranaut_traverse:eof(),
              astranaut_traverse:return(Node));
        error_marker ->
            astranaut_traverse:then(
              astranaut_traverse:formatted_errors([erl_syntax:error_marker_info(Node)]),
              astranaut_traverse:return([]));
        warning_marker ->
            astranaut_traverse:then(
              astranaut_traverse:formatted_warnings([erl_syntax:warning_marker_info(Node)]),
              astranaut_traverse:return([]));
        _ ->
            Pos = erl_syntax:get_pos(Node),
            astranaut_traverse:update_pos(Pos, F(Node))
    end.

uniplate(Node) ->
    case subtrees(Node) of
        [] ->
            {[], fun(_) -> Node end};
        Subtrees ->
            {Subtrees, fun(Subtrees1) ->
                               update_tree(Node, Subtrees1)
                       end}
    end.

subtrees(Node) ->
    with_badarg(fun() -> astranaut_syntax:subtrees(Node) end, Node).

update_tree(Node, Subtrees) ->
    try astranaut_syntax:revert(astranaut_syntax:update_tree(Node, Subtrees)) of
        Node1 ->
            Node1
    catch
        EType:Exception:StackTrace ->
            erlang:raise(EType, {update_tree_failed, Node, Subtrees, Exception}, StackTrace)
    end.

with_badarg(Fun, Node) ->
    try Fun() of
        Value ->
            Value
    catch
        EType:{badarg, _}:StackTrace ->
            erlang:raise(EType, {invalid_node, Node}, StackTrace)
    end.

format_error({validate_key_failure, required, Key, _Value}) ->
    io_lib:format("option key ~p is required", [Key]);
format_error({validate_key_failure, {invalid_validator, Validator}, Key, _Value}) ->
    io_lib:format("validator ~p for option key ~p is invalid", [Validator, Key]);
format_error({validate_key_failure, {invalid_validator_arg, {Validator, Arg}}, Key, _Value}) ->
    io_lib:format("argument ~p of validator ~p for option key ~p is invalid", [Arg, Validator, Key]);
format_error({validate_key_failure, {invalid_validator_arg, Validator}, Key, _Value}) when is_atom(Validator) ->
    io_lib:format("argument of validator ~p for option key ~p is empty", [Validator, Key]);
format_error({validate_key_failure, {invalid_value, Validator}, Key, Value}) ->
    io_lib:format("validator ~p for option key ~p's value ~p failed", [Validator, Key, Value]);
format_error({validate_key_failuer, {invalid_validator_return, Validator, Return}, Key, _Value}) ->
    io_lib:format("validator ~p for option key ~p returns a invalid_value ~p", [Validator, Key, Return]);
format_error({invalid_option_value, Value}) ->
    io_lib:format("~p is not a valid option value", [Value]);
format_error(Message) ->
    case io_lib:deep_char_list(Message) of
        true -> Message;
        _    -> io_lib:write(Message)
    end.
