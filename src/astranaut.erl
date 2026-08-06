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
-export([smap/3, sreduce/4, smap_with_state/4, smapfold/4, search/3]).
-export([map/3, reduce/4, map_with_state/4, mapfold/4]).
-export([map_m/3, map_m_forms/3, traverse_form/2]).
-export([walk_return/1, traverse_return/1]).
-export([uniplate/1]).
-export([format_error/1, format_error/2]).

-export_type([tree/0, trees/0, form/0, forms/0,
              traverse_opts/0, straverse_opts/0, traverse_attr/0,
              traverse_style/0, traverse_step/0, walk_return/2,
              parse_transform_return/0]).

-type tree()   :: tuple().
-type trees()  :: tree() | [tree()].
-type form()   :: erl_parse:abstract_form() |
                  {eof, erl_anno:location()} |
                  {error, erl_parse:error_info()} |
                  {warning, erl_parse:error_info()}.
-type forms()  :: [form()].
-type parse_transform_return() ::
        forms() |
        {warning, forms(), astranaut_error:compiler_error()} |
        {error, astranaut_error:compiler_error(),
         astranaut_error:compiler_error()}.
-type rtrees() :: astranaut_uniplate:node_context(tree()) | [astranaut_uniplate:node_context(tree())].

-type traverse_opts() :: #{traverse => traverse_style(),
                           formatter => module(),
                           attr => traverse_attr(),
                           validate => false | true | input | output | both,
                           validate_opts => map(),
                           uniplate => astranaut_uniplate:uniplate(tree()),
                           normalize => boolean(),
                           role => astranaut_syntax:node_role(),
                           validator => astranaut_syntax:validator(),
                           term() => term()
                          }.

-type straverse_opts() :: #{traverse => traverse_style(),
                            formatter => module(),
                            attr => traverse_attr(),
                            validate => false | true | input | output | both,
                            validate_opts => map(),
                            uniplate => astranaut_uniplate:uniplate(tree()),
                            normalize => boolean(),
                            role => astranaut_syntax:node_role(),
                            validator => astranaut_syntax:validator(),
                            term() => term()
                           }.

-type traverse_attr() :: map().
-type traverse_style() :: traverse_step() | all | subtree | none.
-type traverse_step() :: pre | post.

-type common_walk_return(State, Value) :: astranaut_traverse:struct(State, Value) | astranaut_return:struct(Value) | walk_return(State, Value) | walk_return_tuple(Value).

-type map_walk_return() :: common_walk_return(undefined, rtrees()) | rtrees().
-type map_walk() :: map_walk_1() | map_walk_2().
-type map_walk_1() :: fun((tree()) -> map_walk_return()).
-type map_walk_2() :: fun((tree(), traverse_attr()) -> map_walk_return()).

-type reduce_walk_return(State) :: common_walk_return(State, State) | State.
-type reduce_walk(State) :: reduce_walk_2(State) | reduce_walk_3(State).
-type reduce_walk_2(State) :: fun((tree(), State) -> reduce_walk_return(State)).
-type reduce_walk_3(State) :: fun((tree(), State, traverse_attr()) -> reduce_walk_return(State)).

-type mapfold_walk_return(State) :: common_walk_return(State, rtrees()) | {rtrees(), State}.
-type mapfold_walk(State) :: mapfold_walk_2(State) | mapfold_walk_3(State).
-type mapfold_walk_2(State) :: fun((tree(), State) -> mapfold_walk_return(State)).
-type mapfold_walk_3(State) :: fun((tree(), State, traverse_attr()) -> mapfold_walk_return(State)).

-type walk_return(S, A) :: #{?STRUCT_KEY => ?WALK_RETURN,
                             return => A,
                             state => S,
                             errors => [any()],
                             warnings => [any()],
                             continue => boolean()}.

-type walk_return_map(S, A) :: #{return => A,
                                 state => S,
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
%% works same as map/3 and returns trees(), not astranaut_return:struct(trees()).
-spec smap(fun((tree()) -> rtrees()) | fun((tree(), #{}) -> rtrees()), trees(), straverse_opts()) -> trees().
smap(F, Node, Opts) ->
    F1 = fun(N, A) -> apply_fun(F, [N, A]) end,
    map_m_with_attr(F1, Node, identity, Opts, is_function(F, 2)).

%% @doc
%% works same as reduce/4 and returns S, not astranaut_return:struct(S).
-spec sreduce(fun((tree(), S) -> S) | fun((tree(), S, #{}) -> S), S, trees(), straverse_opts()) -> S.
sreduce(F, Init, Node, Opts) ->
    F1 = fun(N, A) -> fun(S) -> {N, apply_fun(F, [N, S, A])} end end,
    StateM = map_m_with_attr(F1, Node, state, Opts#{static => true, normalize => false}, is_function(F, 3)),
    %% Node is never changed if static is true
    {_Node, Acc} = StateM(Init),
    Acc.

%% @doc
%% works same as map_with_state/4 and returns trees(), not astranaut_return:struct(trees()).
-spec smap_with_state(fun((tree(), S) -> {rtrees(), S}) | fun((tree(), S, #{}) -> {rtrees(), S}),
               S, trees(), straverse_opts()) -> trees().
smap_with_state(F, Init, Node, Opts) ->
    {Node1, _Acc} = smapfold(F, Init, Node, Opts),
    Node1.

%% @doc
%% works same as mapfold/4 and returns {trees(), S}, not astranaut_return:struct({trees(), S}).
-spec smapfold(fun((tree(), S) -> {rtrees(), S}) | fun((tree(), S, #{}) -> {rtrees(), S}),
               S, trees(), straverse_opts()) -> {trees(), S}.
smapfold(F, Init, Node, Opts) ->
    %% for more clear to read
    %% this is the old version to get StateM
    %% while is_function(F, 2)
    %% =============================================
    %% AFB :: fun((A) -> monad(state, B))
    %% AFB =
    %%     fun(N) ->         %% N is Node
    %%             fun(S) -> %% S is State
    %%                     F(N, S)
    %%             end
    %%     end,
    %% StateM :: monad(state, B)
    %% StateM = map_m(AFB, Node, state, Opts),
    %% =============================================
    %% while is_function(F, 3)
    %% =============================================
    %% AFB :: fun((A) -> monad({reader, state}, B))
    %% AFB =
    %%     fun(N) ->                 %% N is Node
    %%             fun(A) ->         %% N is Attr
    %%                     fun(S) -> %% S is State
    %%                             F(N, S, A)
    %%                     end
    %%             end
    %%     end,
    %% Monad = {reader, state},
    %% ReaderTStateM :: monad({reader, state}, B)
    %% ReaderTStateM = map_m(AFB, Node, Monad, Opts),
    %% StateM = ReaderTStateM(Attr),
    %% =============================================
    F1 = fun(N, A) -> fun(S) -> apply_fun(F, [N, S, A]) end end,
    StateM = map_m_with_attr(F1, Node, state, Opts, is_function(F, 3)),
    (StateM)(Init).

%% @doc traverse node with F, if F(Node, Attr) is true, traverse is stopped immediately and return true, else return false.
-spec search(fun((N) -> boolean()) | fun((N, map()) -> boolean()), N, straverse_opts()) -> boolean().
search(F, Node, Opts) ->
    F1 = fun(N, A) -> case apply_fun(F, [N, A]) of true -> {left, match}; false -> {right, N} end end,
    Either = map_m_with_attr(F1, Node, either, Opts#{static => true, normalize => false}, is_function(F, 2)),
    case Either of {left, match} -> true; {right, _Node} -> false end.

map_m_with_attr(F, Node, Monad, Opts, WithAttr) ->
    Uniplate = maps:get(uniplate, Opts, fun uniplate/1),
    Opts1 = maps:remove(uniplate, Opts),
    map_m_with_attr(F, Node, Uniplate, Monad, Opts1, WithAttr).

map_m_with_attr(F, Node, Uniplate, Monad, Opts, false) ->
    %% N is Node and #{} is empty Attr
    F1 = fun(N) -> F(N, #{}) end,
    case validation_attr_needed(Node, Opts) of
        true ->
            InitAttr = root_traverse_attr(Node, Opts, maps:get(attr, Opts, #{})),
            Opts1 = maps:remove(attr, Opts),
            F2 = fun(N) -> fun(_A) -> F1(N) end end,
            ReaderT = astranaut_uniplate:map_m(F2, Node, Uniplate, {reader, Monad}, Opts1),
            ReaderT(InitAttr);
        false ->
            astranaut_uniplate:map_m(F1, Node, Uniplate, Monad, Opts)
    end;
map_m_with_attr(F, Node, Uniplate, Monad, Opts, true) ->
    %% to take benefit of attribute access, add a ReaderT monad transformer.
    Attr = root_traverse_attr(Node, Opts, maps:get(attr, Opts, #{})),
    Opts1 = maps:remove(attr, Opts),
    %% N is Node and A is Attr
    F1 = fun(N) -> fun(A) -> F(N, A) end end,
    ReaderT = astranaut_uniplate:map_m(F1, Node, Uniplate, {reader, Monad}, Opts1),
    ReaderT(Attr).

-spec map(map_walk(), rtrees(), traverse_opts()) -> astranaut_return:struct(trees()).
%% @doc
%% Takes a function from AstNodeA, {@link traverse_attr()} to AstNodeB, and a TopNodeA and produces a TopNodeB by applying the function to every subtree in the AST. This function is used to obtain the return values.
%% @see mapfold/4
map(F, TopNode, Opts) ->
    WithReturn = fun(_Node, Node1) -> #{return => Node1} end,
    F1 = fun(Node, _State, Attr) -> apply_fun(F, [Node, Attr]) end,
    Return = mapfold_1(F1, undefined, TopNode, Opts, WithReturn),
    astranaut_return:lift_m(fun({TopNode1, _State}) -> TopNode1 end, Return).

-spec reduce(reduce_walk(S), S, trees(), traverse_opts()) -> astranaut_return:struct(S).
%% @doc Calls F(AstNode, AccIn, Attr) on successive subtree AstNode of TopNode, starting with AccIn =:= Acc0. F/3 must return a new accumulator, which is passed to the next call. The function returns the final value of the accumulator. Acc0 is returned if the TopNode is empty.
%% @see mapfold/4
reduce(F, Init, TopNode, Opts) ->
    WithReturn = fun(Node, State) -> #{return => Node, state => State} end,
    F1 = fun(Node, State, Attr) -> apply_fun(F, [Node, State, Attr]) end,
    Return = mapfold_1(F1, Init, TopNode, Opts#{static => true, normalize => false}, WithReturn),
    astranaut_return:lift_m(fun({_TopNode1, State}) -> State end, Return).

-spec map_with_state(mapfold_walk(S), S, trees(), traverse_opts()) -> astranaut_return:struct(trees()).
%% @doc like mapfold/4, return astranaut_return:struct(trees()) instead of astranaut_return:struct({trees(), S}), just ignore state.
%% @see mapfold/4
map_with_state(F, Init, TopNode, Opts) ->
    Return = mapfold(F, Init, TopNode, Opts),
    astranaut_return:lift_m(fun({TopNode1, _State}) -> TopNode1 end, Return).

-spec mapfold(mapfold_walk(S), S, trees(), traverse_opts()) -> astranaut_return:struct({trees(), S}).
%% @doc Combines the operations of map/3 and reduce/4 into one pass.
mapfold(F, Init, TopNode, Opts) ->
    WithReturn =
        fun(_Node, {Node1, State1}) ->
                #{return => Node1, state => State1};
           (Node, Return) ->
                %% when return other value, we dont know which part is node and which part is state
                %% just throw exception.
                erlang:error({invalid_mapfold_return, Return, Node})
        end,
    mapfold_1(F, Init, TopNode, Opts, WithReturn).

mapfold_1(F, Init, TopNode, #{} = Opts, WithReturn) ->
    Formatter = maps:get(formatter, Opts, ?MODULE),
    InitAttr = root_traverse_attr(TopNode, Opts, maps:get(attr, Opts, #{})),
    Opts1 = maps:without([formatter], Opts#{attr => InitAttr}),
    F1 = fun(Node) ->
                 astranaut_traverse:bind(
                   astranaut_traverse:get(),
                   fun(State) ->
                           astranaut_traverse:bind(
                             astranaut_traverse:ask(),
                             fun(Attr) ->
                                     with_return(Node, apply_fun(F, [Node, State, Attr]), WithReturn)
                             end)
                   end)
         end,
    TopNodeM = map_m(F1, TopNode, Opts1),
    astranaut_traverse:run(TopNodeM, Formatter, InitAttr, Init).

apply_fun(F, [N|_T]) when is_function(F, 1) ->
    F(N);
apply_fun(F, [A1, A2|_T]) when is_function(F, 2) ->
    F(A1, A2);
apply_fun(F, [A1, A2, A3]) when is_function(F, 3) ->
    F(A1, A2, A3).

%% convert other types to type astranaut_traverse
%% apply With if it's necessary.
%% supported types is: astranaut_return, walk_return, and single value.
with_return(_Node, #{?STRUCT_KEY := ?TRAVERSE_M}, _With) ->
    exit(unsupported_traverse_struct);
with_return(Node, #{?STRUCT_KEY := ?RETURN_OK, return := Return} = Struct, With) ->
    astranaut_traverse:then(
      astranaut_traverse:astranaut_traverse(Struct),
      with_return(Node, Return, With));
with_return(_Node, #{?STRUCT_KEY := ?RETURN_FAIL} = Struct, _With) ->
    astranaut_traverse:astranaut_traverse(Struct);
with_return(Node, #{?STRUCT_KEY := ?WALK_RETURN} = WalkReturn, _With) ->
    WalkReturn1 = walk_return_up_map(Node, WalkReturn),
    astranaut_traverse:astranaut_traverse(WalkReturn1);
with_return(Node, Return, With) ->
    WalkReturn1 = walk_return(walk_return_map(Return)),
    WalkReturn2 =
        case maps:find(return, WalkReturn1) of
            {ok, Return} ->
                maps:merge(WalkReturn1, With(Node, Return));
            error ->
                WalkReturn1
        end,
    with_return(Node, WalkReturn2, With).

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
    Map1 = walk_return_up_map(undefined, maps:with(Keys, Map)),
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

map_m(F, Node, Opts) ->
    Uniplate = maps:get(uniplate, Opts, fun uniplate/1),
    Opts1 = maps:remove(uniplate, Opts),
    with_root_traverse_attr(
      Node, Opts,
      astranaut_uniplate:map_m(
        fun(Node1) ->
                traverse_map_form(F, Node1)
        end, Node, Uniplate, traverse, Opts1)).

%% Map a module form list and perform the form-specific insertion, ordering and
%% __original__ merge once after every source form has been visited.
map_m_forms(F, Forms, Opts) ->
    astranaut_traverse:lift_m(
      fun astranaut_forms:reorder_updated_forms/1,
      astranaut_traverse:map_m(
        fun(Form) ->
                astranaut_traverse:lift_m(
                  fun({Form1, true}) ->
                          {updated, Form, to_list(Form1)};
                     ({_Form1, false}) ->
                          Form
                  end, astranaut_traverse:listen_updated(
                         maybe_catch_map_form_error(
                           map_form(F, Form, Opts), Opts)))
        end, Forms)).

maybe_catch_map_form_error(FormM, Opts) ->
    case maps:get(fail, maps:get(validate_opts, Opts, #{}), raise) of
        collect ->
            FormM;
        raise ->
            astranaut_traverse:catch_on_error(
              FormM,
              fun() -> astranaut_traverse:return({[], true}) end)
    end.

to_list(Form1) when is_list(Form1) ->
    Form1;
to_list(Form1) ->
    [Form1].

map_form(F, Form, #{traverse := none} = Opts) ->
    with_root_traverse_attr(
      Form, Opts,
      astranaut_traverse:bind(
        traverse_map_form(F, Form),
        fun(Form1) ->
                astranaut_traverse:writer_updated({Form1, Form =/= Form1})
        end));
map_form(F, Form, Opts) ->
    map_m(F, Form, Opts).

with_root_traverse_attr(Node, Opts, Monad) ->
    astranaut_traverse:local(
      fun(OuterAttr) -> root_traverse_attr(Node, Opts, OuterAttr) end,
      Monad).

validation_attr_needed(_Node, #{validate := Validate})
  when Validate =:= true; Validate =:= input; Validate =:= output; Validate =:= both ->
    true;
validation_attr_needed(_Node, #{normalize := false}) ->
    false;
validation_attr_needed(_Node, #{role := _Role}) ->
    true;
validation_attr_needed(_Node, #{validator := _Validator}) ->
    true;
validation_attr_needed(_Node, #{attr := _Attr}) ->
    true;
validation_attr_needed(Node, #{normalize := true}) ->
    root_role(Node) =/= unknown;
validation_attr_needed(_Node, _Opts) ->
    false.

root_traverse_attr(Node, Opts, Attr) ->
    Attr1 = maps:merge(Attr, maps:get(attr, Opts, #{})),
    case explicit_root_validator(Opts) of
        {ok, Role, Validator} ->
            explicit_validator_attr(Role, Validator, Attr1);
        error ->
            case maps:is_key(node, Attr1) orelse maps:is_key(validator, Attr1) of
                true ->
                    Attr1;
                false ->
                    case validation_enabled(Opts) of
                        true ->
                            case root_role(Node) of
                                unknown ->
                                    Attr1;
                                ambiguous ->
                                    erlang:error({ambiguous_root_role, Node});
                                Role ->
                                    Attr1#{node => Role, validator => {role, Role}}
                            end;
                        false ->
                            Attr1
                    end
            end
    end.

validation_enabled(Opts) ->
    case maps:find(validate, Opts) of
        {ok, false} -> false;
        {ok, _Validate} -> true;
        error -> maps:get(normalize, Opts, false)
    end.

explicit_root_validator(#{validator := {role, Role} = Validator}) ->
    {ok, Role, Validator};
explicit_root_validator(#{validator := {slot, _ParentType, _Slot, Role} = Validator}) ->
    {ok, Role, Validator};
explicit_root_validator(#{role := Role}) ->
    {ok, Role, {role, Role}};
explicit_root_validator(_Opts) ->
    error.

explicit_validator_attr(Role, Validator, Attr) ->
    Attr1 = Attr#{validator => Validator},
    case lists:member(Role, [expression, pattern, guard, form, type, clause]) of
        true -> Attr1#{node => Role};
        false -> Attr1
    end.

root_role([Node|Nodes]) ->
    case root_role(Node) of
        unknown ->
            unknown;
        ambiguous ->
            ambiguous;
        Role ->
            case lists:all(fun(Node1) -> root_role(Node1) =:= Role end, Nodes) of
                true -> Role;
                false -> ambiguous
            end
    end;
root_role([]) ->
    unknown;
root_role(Node) ->
    try astranaut_syntax:type(Node) of
        Type ->
            root_role_type(Type)
    catch
        _:_ ->
            unknown
    end.

root_role_type(Type) when Type =:= function; Type =:= attribute;
                          Type =:= eof_marker; Type =:= error_marker;
                          Type =:= warning_marker ->
    form;
root_role_type(clause) ->
    clause;
root_role_type(Type) ->
    case astranaut_syntax:node_roles(Type) of
        [type] -> type;
        [Role] -> Role;
        _Roles -> ambiguous
    end.

traverse_map_form(F, Node) ->
    Type = astranaut_syntax:type(Node),
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
                    Pos = astranaut_syntax:get_pos(Node),
                    astranaut_traverse:update_pos(Pos, F(Node))
            end;
        function ->
            Pos = astranaut_syntax:get_pos(Node),
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
            traverse_map_node(F, Node)
    end.

%% Queue ownership belongs to the caller; this helper preserves Astranaut's
%% common file, position, error-marker and warning-marker form boundary.
traverse_form(F, Form) ->
    traverse_map_form(F, Form).

traverse_map_node(F, Node) ->
    Pos = astranaut_syntax:get_pos(Node),
    astranaut_traverse:update_pos(Pos, F(Node)).

uniplate(Node) ->
    case astranaut_syntax:subtrees(Node) of
        [] ->
            {[], fun(_) -> Node end};
        Subtrees ->
            {Subtrees, fun(Subtrees1) ->
                               astranaut_syntax:revert(astranaut_syntax:update_tree(Node, Subtrees1))
                       end}
    end.

-spec format_error(term()) -> term().
format_error(Error) ->
    format_error(Error, #{}).

-spec format_error(term(), map()) -> term().
format_error(Error, Opts) ->
    try format_error_1(Error) of
        Formatted ->
            Formatted
    catch
        error:function_clause:Stacktrace ->
            case format_error_1_no_match(Stacktrace) of
                true ->
                    format_error_fallback(Error, Opts);
                false ->
                    erlang:raise(error, function_clause, Stacktrace)
            end
    end.

-spec format_error_1(term()) -> term().
format_error_1({validate_key_failure, required, Key, _Value}) ->
    io_lib:format("option key ~p is required", [Key]);
format_error_1({validate_key_failure, {invalid_validator, Validator}, Key, _Value}) ->
    io_lib:format("validator ~p for option key ~p is invalid", [Validator, Key]);
format_error_1({validate_key_failure, {invalid_validator_arg, {Validator, Arg}}, Key, _Value}) ->
    io_lib:format("argument ~p of validator ~p for option key ~p is invalid", [Arg, Validator, Key]);
format_error_1({validate_key_failure, {invalid_validator_arg, Validator}, Key, _Value}) when is_atom(Validator) ->
    io_lib:format("argument of validator ~p for option key ~p is empty", [Validator, Key]);
format_error_1({validate_key_failure, {invalid_value, Validator}, Key, Value}) ->
    io_lib:format("validator ~p for option key ~p's value ~p failed", [Validator, Key, Value]);
format_error_1({validate_key_failure, {invalid_validator_return, Validator, Return}, Key, _Value}) ->
    io_lib:format("validator ~p for option key ~p returns a invalid_value ~p", [Validator, Key, Return]);
format_error_1({invalid_option_value, Value}) ->
    io_lib:format("~p is not a valid option value", [Value]).
-spec format_error_fallback(term(), map()) -> term().
format_error_fallback(Message, #{default := throw}) ->
    throw(Message);
format_error_fallback(Message, _Opts) ->
    case io_lib:deep_char_list(Message) of
        true -> Message;
        _    -> io_lib:write(Message)
    end.

-spec format_error_1_no_match(list()) -> boolean().
format_error_1_no_match([{?MODULE, format_error_1, _Arity, _Info}|_]) ->
    true;
format_error_1_no_match(_) ->
    false.
