%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 18 Sep 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_traverse).

%% API
-export([new_state/1, merge_state/2]).
-export([reply_to_traverse_fun_return/2, traverse_fun_return/1, traverse_error/1]).
-export([map/2, map/3]).
-export([map_with_state/3, map_with_state/4]).
-export([reduce/3, reduce/4]).
-export([mapfold/3, mapfold/4]).
-export([bind/2, then/2, bind_return/2, then_return/2, fold_bind_returns/3]).
-export([without_errors/1, with_attributes/5, with_form/3]).
-export([map_m/3, map_m_children/3, m_subtrees/3, update_opts/1]).
-export([map_traverse_return/2, map_traverse_return_e/2, map_traverse_fun_return/2,
         traverse_fun_return_struct/1]).
-export([transform_mapfold_f/2]).
-export([format_error/1, parse_transform_return/1, parse_transform_return/2]).
-export([fun_return_to_monad/2, fun_return_to_monad/3]).
-export([monad_to_traverse_fun_return/1, monad_to_traverse_fun_return/2]).
-export([deep_sequence_m/1, deep_r_sequence_m/1]).

-define(TRAVERSE_FUN_RETURN, astranaut_traverse_fun_return).
-define(TRAVERSE_ERROR, astranaut_traverse_error).

-type traverse_node() :: tuple().
-type traverse_error() :: term().
-type traverse_state() :: term().
-type traverse_opts() :: #{traverse => traverse_style(), parse_transform => boolean(),
                           node => node_type(), formatter => module() }.

-type traverse_map_m_opts() :: #{traverse => traverse_style(), parse_transform => boolean(),
                                 node => node_type(), attribute => atom(), 
                                 formatter => module()
                                }.

-type traverse_error_state() :: #{file => string(), errors => traverse_return_error(), warnings => traverse_return_error(),
                                  file_errors => parse_transform_return_error(), 
                                  file_warnings => parse_transform_return_error()}.

-type node_type() :: attribute | pattern | expression | guard | form.
-type traverse_style() :: traverse_step() | all.
-type traverse_step() :: pre | post | leaf.
-type traverse_attr() :: #{step := traverse_step(), node := node_type()}.
-type traverse_fun_return() :: #{'__struct__' := astranaut_traverse_fun_return, 
                                 node => traverse_node(), state => traverse_state(),
                                 continue => boolean(),
                                 error => traverse_error(), warning => traverse_error(),
                                 errors => [traverse_error()], warings => [traverse_error()]} |
                               {error, traverse_error()} | {warning, traverse_node(), traverse_error()} |
                               continue | {traverse_node(), traverse_state()} | traverse_node().

-type traverse_return(ReturnType) :: ReturnType | 
                                     {ok, ReturnType, traverse_error_state()} |
                                     {error, traverse_error_state()}.

-type traverse_final_return(ReturnType) :: traverse_return(ReturnType) | parse_transform_return(ReturnType).

-type parse_transform_return(ReturnType) :: ReturnType |  {warning, ReturnType, parse_transform_return_error()} |
                                            {error, parse_transform_return_error(), parse_transform_return_error()}.

-type traverse_fun() :: fun((traverse_node(), traverse_attr()) -> traverse_fun_return()).
-type traverse_state_fun() :: fun((traverse_node(), traverse_state(), traverse_attr()) -> traverse_fun_return()).
-type parse_transform_module() :: module().
-type compile_file() :: string().
-type line() :: integer().
-type traverse_return_error() :: [{line(), parse_transform_module(), traverse_error()}].
-type parse_transform_return_error() :: [{compile_file(), traverse_return_error()}].

%%%===================================================================
%%% API
%%%===================================================================
traverse_fun_return(#{} = Map) ->
    Map#{'__struct__' => ?TRAVERSE_FUN_RETURN}.

traverse_error(#{} = Map) ->
    Map#{'__struct__' => ?TRAVERSE_ERROR}.

-spec map(traverse_fun(), Node) -> traverse_final_return(Node).
map(F, TopNode) ->
    map(F, TopNode, #{}).

-spec reduce(traverse_state_fun(), State, _Node) -> traverse_return(State).
reduce(F, Init, TopNode) ->
    reduce(F, Init, TopNode, #{}).

-spec map_with_state(traverse_state_fun(), State, Node) -> traverse_final_return({Node, State}).
map_with_state(F, Init, Form) ->
    map_with_state(F, Init, Form, #{}).

-spec mapfold(traverse_state_fun(), State, Node) -> traverse_return({Node, State}).
mapfold(F, Init, Node) ->
    mapfold(F, Init, Node, #{}).

-spec map(traverse_fun(), Node, traverse_opts()) -> traverse_final_return(Node).
map(F, TopNode, Opts) ->
    NF = fun(Node, State, Attr) ->
                 WalkReturn = F(Node, Attr),
                 map_walk_return(fun(NNode) -> {NNode, State} end, WalkReturn)
         end,
    map_with_state(NF, ok, TopNode, Opts).

-spec reduce(traverse_state_fun(), State, _Node, traverse_opts()) -> traverse_return(State).
reduce(F, Init, TopNode, Opts) ->
    NF = fun(Node, State, Attr) ->
                 WalkReturn = F(Node, State, Attr),
                 map_walk_return(fun(NState) -> {Node, NState} end, WalkReturn)
         end,
    map_traverse_return(
      fun({_NNode, State}) ->
              State
      end, mapfold(NF, Init, TopNode, Opts)).

-spec map_with_state(traverse_state_fun(), State, Node, traverse_opts()) -> traverse_final_return({Node, State}).
map_with_state(F, Init, Node, Opts) ->
    Reply = mapfold(F, Init, Node, Opts),
    Reply1 = 
        map_traverse_return(
          fun({NNode, _State}) ->
                  NNode
          end, Reply),
    case maps:get(parse_transform, Opts, false) of
        true ->
            parse_transform_return(Reply1);
        false ->
            Reply1
    end.

-spec mapfold(traverse_state_fun(), State, Node, traverse_opts()) -> traverse_return({Node, State}).
mapfold(F, Init, Node, Opts) ->
    Return = mapfold_1(F, Init, Node, Opts),
    case maps:get(simplify_return, Opts, true) of
        true ->
            simplify_return(Return);
        false ->
            Return
    end.

-spec bind(traverse_return(A), fun((A) -> B)) -> traverse_return(B).
bind({ok, Return, ErrorState0}, F) ->
    case F(Return) of
        {error, ErrorState1} ->
            ErrorState2 = astranaut_traverse_error_state:merge(ErrorState0, ErrorState1),
            {error, ErrorState2};
        {ok, Return1, ErrorState1} ->
            ErrorState2 = astranaut_traverse_error_state:merge(ErrorState0, ErrorState1),
            {error, Return1, ErrorState2};
        {ok, Return1} ->
            {ok, Return1, ErrorState0};
        Return1 ->
            {ok, Return1, ErrorState0}
    end;
bind({error, ErrorState}, _F) ->
    {error, ErrorState};
bind(Return, F) ->
    F(Return).

then(A, B) ->
    bind(A, fun(_) -> B end).

fold_bind_returns(F, Init, [H|T]) ->
    bind_return(
      F(H, Init),
      fun(Acc) ->
              fold_bind_returns(F, Acc, T)
      end);
fold_bind_returns(_F, Acc, []) ->
    {ok, Acc}.

bind_return(Return, F) ->
    case update_return(Return) of
        #{state := State} = Return1 ->
            Return2 = update_return(F(State)),
            Errors1 = maps:get(errors, Return1, []),
            Warnings1 = maps:get(warnings, Return1, []),
            Errors2 = maps:get(errors, Return2, []),
            Warnings2 = maps:get(warnings, Return2, []),
            maps:merge(Return1, Return2#{errors => Errors1 ++ Errors2, warnings => Warnings1 ++ Warnings2});
        #{} = Return1 ->
            Return1
    end.

then_return(Return1, Return2) ->
    bind_return(Return1, fun(_) -> Return2 end).

without_errors(Forms) ->
    FormsWithErrors = 
        with_form(
          fun({error, Errors}, FormsAcc, ErrorState) when is_list(Errors) ->
                  ErrorState1 = astranaut_traverse_error_state:errors(Errors, ErrorState),
                  {FormsAcc, ErrorState1};
             ({error, Error}, FormsAcc, ErrorState) ->
                  ErrorState1 = astranaut_traverse_error_state:error(Error, ErrorState),
                  {FormsAcc, ErrorState1};
             (Form, FormsAcc, ErrorState) ->
                  {[Form|FormsAcc], ErrorState}
          end, [], Forms),
    bind(
      FormsWithErrors,
      fun(Forms1) ->
              lists:reverse(Forms1)
      end).

with_attributes(F, Init, Name, Forms, Opts) ->
    with_form(
      fun({attribute, Line, Attr, AttrValues}, Acc, ErrorState) when Attr == Name ->
              add_attribute(F, AttrValues, Line, Acc, ErrorState, Opts);
         (_Form, Acc, ErrorState) ->
              {Acc, ErrorState}
      end, Init, Forms).

add_attribute(F, AttrValues, Line, Acc, ErrorState, Opts) when is_list(AttrValues) ->
    lists:foldl(
      fun(AttrValue, {Acc1, ErrorState1}) ->
              add_attribute(F, AttrValue, Line, Acc1, ErrorState1, Opts)
      end, {Acc, ErrorState}, AttrValues);
add_attribute(F, AttrValue, Line, Acc, ErrorState, #{formatter := Formatter}) ->
    Acc1 = apply_attribute_f(F, AttrValue, Line, Acc),
    apply_return(Acc1, Line, Formatter, Acc, ErrorState).

apply_attribute_f(F, AttributeValue, Line, Acc) when is_function(F, 3) ->
    F(AttributeValue, Line, Acc);
apply_attribute_f(F, AttributeValue, _Line, Acc) when is_function(F, 2) ->
    F(AttributeValue, Acc).
        
apply_return(#{state := State} = Return, Line, Formatter, _State0, ErrorState) ->
    Return1 = maps:remove(state, Return),
    apply_return(Return1,  Line, Formatter, State, ErrorState);
apply_return(#{errors := Errors} = Return, Line, Formatter, State, ErrorState) ->
    Return1 = maps:remove(errors, Return),
    ErrorState1 = add_errors(Errors, Line, Formatter, ErrorState),
    apply_return(Return1,  Line, Formatter, State, ErrorState1);
apply_return(#{warnings := Warnings} = Return, Line, Formatter, State, ErrorState) ->
    Return1 = maps:remove(warnings, Return),
    ErrorState1 = add_warnings(Warnings, Line, Formatter, ErrorState),
    apply_return(Return1,  Line, Formatter, State, ErrorState1);
apply_return(#{}, _Line, _Formatter, State, ErrorState) ->
    {State, ErrorState};
apply_return(Return, Line, Formatter, State0, ErrorState) ->
    Return1 = update_return(Return),
    apply_return(Return1, Line, Formatter, State0, ErrorState).

update_return({ok, State}) ->
    #{state => State};
update_return({error, State1, Error}) ->
    #{state => State1, errors => [Error]};
update_return({errors, Errors}) ->
    #{errors => Errors};
update_return({errors, State1, Errors}) ->
    #{state => State1, errors => Errors};
update_return({warning, State1, Warning}) ->
    #{state => State1, warnings => [Warning]};
update_return({warnings, State1, Warnings}) ->
    #{state => State1, warnings => Warnings};
update_return(#{} = Return) ->
    Return.


add_errors(Errors, Line, Formatter, ErrorState) ->
    Errors1 = format(Line, Formatter, Errors),
    astranaut_traverse_error_state:errors(Errors1, ErrorState).

add_warnings(Warnings, Line, Formatter, ErrorState) ->
    Warnings1 = format(Line, Formatter, Warnings),
    astranaut_traverse_error_state:warnings(Warnings1, ErrorState).

format(Line, Formatter, Errors) ->
    lists:map(fun(Error) -> {Line, Formatter, Error} end, Errors).

with_form(F, Init, Forms) ->
    {Forms1, ErrorState} = 
        lists:foldl(
          fun({attribute, _Line1, file, {File, _Line2}} = Form, {Acc, ErrorState}) ->
                  ErrorState1 = astranaut_traverse_error_state:update_file(File, ErrorState),
                  apply_with_form_f(F, Form, Acc, ErrorState1);
             ({eof, _Line} = Form, {Acc, ErrorState}) ->
                  ErrorState1 = astranaut_traverse_error_state:update_file(undefined, ErrorState),
                  apply_with_form_f(F, Form, Acc, ErrorState1);
             (Form, {Acc, ErrorState}) ->
                  apply_with_form_f(F, Form, Acc, ErrorState)
          end, {Init, astranaut_traverse_error_state:new()}, Forms),
    simplify_return({ok, Forms1, ErrorState}).

apply_with_form_f(F, Form, Acc, ErrorState) when is_function(F, 3) ->
    F(Form, Acc, ErrorState);
apply_with_form_f(F, Form, Acc, ErrorState) when is_function(F, 2) ->
    Acc1 = F(Form, Acc),
    {Acc1, ErrorState}.

new_state(State) ->
    traverse_fun_return(#{state => State}).

merge_state(Return, #{'__struct__' := ?TRAVERSE_FUN_RETURN} = Struct)  ->
    merge_traverse_state_1(traverse_fun_return_struct(Return), Struct).

merge_traverse_state_1(#{error := Error} = Return, Struct) ->
    Errors0 = maps:get(errors, Struct, []),
    Errors1 = Errors0 ++ [Error],
    Struct1 = Struct#{errors => Errors1},
    Return1 = maps:remove(error, Return),
    merge_traverse_state_1(Return1, Struct1);
merge_traverse_state_1(#{errors := Errors} = Return, Struct) ->
    Errors0 = maps:get(errors, Struct, []),
    Errors1 = Errors0 ++ Errors,
    Struct1 = Struct#{errors => Errors1},
    Return1 = maps:remove(errors, Return),
    merge_traverse_state_1(Return1, Struct1);
merge_traverse_state_1(#{warning := Warning} = Return, Struct) ->
    Warnings0 = maps:get(warnings, Struct, []),
    Warnings1 = Warnings0 ++ [Warning],
    Struct1 = Struct#{warnings => Warnings1},
    Return1 = maps:remove(warning, Return),
    merge_traverse_state_1(Return1, Struct1);
merge_traverse_state_1(#{warnings := Warnings} = Return, Struct) ->
    Warnings0 = maps:get(warnings, Struct, []),
    Warnings1 = Warnings0 ++ Warnings,
    Struct1 = Struct#{warnings => Warnings1},
    Return1 = maps:remove(warnings, Return),
    merge_traverse_state_1(Return1, Struct1);
merge_traverse_state_1(#{state := State} = Return, Struct) ->
    Return1 = maps:remove(state, Return),
    merge_traverse_state_1(Return1, Struct#{state => State});
merge_traverse_state_1(#{}, Struct) ->
    Struct.

-spec map_traverse_return(fun((A) -> B), traverse_return(A)) -> parse_transform_return(B).
map_traverse_return(F, {ok, Reply, ErrorState}) ->
    case F(Reply) of
        {error, Error} ->
            {error, astranaut_traverse_error_state:error(Error, ErrorState)};
        {error, Reply1, Error} ->
            {error, Reply1, astranaut_traverse_error_state:error(Error, ErrorState)};
        {ok, Reply1} ->
            {ok, Reply1, ErrorState};
        Reply1 ->
            {ok, Reply1, ErrorState}
    end;
map_traverse_return(_F, {error, Errors, Warnings}) ->
    {error, Errors, Warnings};
map_traverse_return(F, {warning, Reply, Warnings}) ->
    {warning, F(Reply), Warnings};
map_traverse_return(F, Reply) ->
    F(Reply).

map_traverse_return_e(F, {ok, Reply, Errors, Warnings}) ->
    F(Reply, Errors, Warnings);
map_traverse_return_e(F, {error, Errors, Warnings}) ->
    F(error, Errors, Warnings);
map_traverse_return_e(F, {warning, Reply, Warnings}) ->
    F(Reply, [], Warnings);
map_traverse_return_e(F, Reply) ->
    F(Reply, [], []).

map_traverse_fun_return(F, {warning, Node, Reason}) ->
    {warning, F(Node), Reason};
map_traverse_fun_return(F, {error, Node, Reason}) ->
    {error, F(Node), Reason};
map_traverse_fun_return(_F, {error, Reason}) ->
    {error, Reason};
map_traverse_fun_return(_F, continue) ->
    continue;
map_traverse_fun_return(F, #{'__struct__' := astranaut_traverse_fun_return, node := Node} = Struct) ->
    Struct#{node => F(Node)};
map_traverse_fun_return(F, Node) ->
    F(Node).

traverse_fun_return_struct({warning, Node, Reason}) ->
    traverse_fun_return(#{warning => Reason, node => Node});
traverse_fun_return_struct({error, Node, Reason}) ->
    traverse_fun_return(#{error => Reason, node => Node});
traverse_fun_return_struct({error, Reason}) ->
    traverse_fun_return(#{error => Reason});
traverse_fun_return_struct(continue) ->
    traverse_fun_return(#{continue => true});
traverse_fun_return_struct(#{'__struct__' := ?TRAVERSE_FUN_RETURN} = Struct) ->
    Struct;
traverse_fun_return_struct(Node) ->
    traverse_fun_return(#{node => Node}).

update_opts(Opts) ->
    maps:merge(#{formatter => ?MODULE, traverse => all}, Opts).

-spec map_m(traverse_fun(), Node, traverse_map_m_opts()) -> astranaut_monad:monadic(M, Node) when M :: astranaut_monad:monad().
map_m(F, Nodes, Opts) ->
    NOpts = update_opts(Opts),
    NF = transform_f(F, NOpts),
    map_m_1(NF, Nodes, NOpts).

format_error({invalid_option_value, OptionName, Key, Value}) ->
    io_lib:format("invalid option of ~p: ~p, ~p", [OptionName, Key, Value]);
format_error({invalid_option, OptionName, Value}) ->
    io_lib:format("invalid option of ~p: ~p", [OptionName, Value]);
format_error(Message) ->
    case io_lib:deep_char_list(Message) of
        true -> Message;
        _    -> io_lib:write(Message)
    end.

parse_transform_return({ok, Forms, ErrorState}) when is_list(Forms) ->
    case astranaut_traverse_error_state:realize(ErrorState) of
        {[], []} ->
            Forms;
        {[], Warnings} ->
            {warning, Forms, Warnings};
        {Errors, Warnings} ->
            {error, Errors, Warnings}
    end;
parse_transform_return({error, Forms, ErrorState}) when is_list(Forms) ->
    case astranaut_traverse_error_state:realize(ErrorState) of
        {[], []} ->
            Forms;
        {[], Warnings} ->
            {warning, Forms, Warnings};
        {Errors, Warnings} ->
            {error, Errors, Warnings}
    end;
parse_transform_return({error, ErrorState}) ->
    {Errors, Warnings} = astranaut_traverse_error_state:realize(ErrorState),
    {error, Errors, Warnings};
parse_transform_return(Forms) when is_list(Forms)->
    Forms.

parse_transform_return(Return, _File) ->
    parse_transform_return(Return).

%%====================================================================
%% Internal functions
%%====================================================================
mapfold_1(F, Init, Node, Opts) ->
    NF = transform_mapfold_f(F, Opts),
    NodeM = map_m(NF, Node, Opts),
    astranaut_traverse_monad:run(NodeM, Init).

map_m_1(F, Nodes, Opts) when is_list(Nodes) ->
    astranaut_traverse_monad:map_m(
      fun(Subtree) ->
              map_m_1(F, Subtree, Opts)
      end, Nodes);
map_m_1(F, NodeA, #{children := true} = Opts) ->
    Opts1 = maps:remove(children, Opts),
    map_m_children(F, NodeA, Opts1);
map_m_1(F, NodeA, #{traverse := list} = Opts) ->
    SyntaxLib = syntax_lib(Opts),
    case SyntaxLib:is_file(NodeA) of
        false ->
            F(NodeA, #{});
        {file, File} ->
            astranaut_traverse_monad:then(
              astranaut_traverse_monad:update_file(File),
              F(NodeA, #{})
             )
    end;
map_m_1(F, NodeA, #{} = Opts) ->
    SyntaxLib = syntax_lib(Opts),
    map_m_tree(F, NodeA, Opts, SyntaxLib).

map_m_tree(_F, {error, Reason}, _Opts, _SyntaxLib) ->
    astranaut_traverse_monad:error(Reason);
map_m_tree(F, NodeA, Opts, SyntaxLib) ->
    Attr = maps:without([traverse, parse_transform, monad, monad_class, formatter], Opts),
    case SyntaxLib:is_file(NodeA) of
        false ->
            NodeType = SyntaxLib:node_type(NodeA, Opts),
            PreType = 
                case SyntaxLib:subtrees(NodeA, Opts) of
                    [] ->
                        leaf;
                    _Subtrees ->
                        pre
                end,
            bind_with_continue(
              NodeA, 
              F(NodeA, Attr#{step => PreType, node => NodeType}),
              fun(NodeB) ->
                      case SyntaxLib:subtrees(NodeB, Opts) of
                          [] ->
                              astranaut_traverse_monad:return(NodeB);
                          Subtrees ->
                              Parent = SyntaxLib:type(NodeB),
                              astranaut_traverse_monad:bind(
                                map_m_children(F, NodeB, Subtrees, Opts#{parent => Parent}, SyntaxLib),
                                fun(NodeC) ->
                                        bind_with_continue(
                                          NodeC, 
                                          F(NodeC, Attr#{step => post, node => NodeType}),
                                          fun(NodeD) ->
                                                  astranaut_traverse_monad:return(NodeD)
                                          end)
                                end)
                      end
              end);
        {file, File} ->
            astranaut_traverse_monad:then(
              astranaut_traverse_monad:update_file(File),
              F(NodeA, Attr#{step => leaf, node => file})
             )
    end.
    
bind_with_continue(NodeA, MNodeB, BMC) ->
    astranaut_traverse_monad:bind(
      MNodeB,
      fun(continue) ->
              astranaut_traverse_monad:return(NodeA);
         ({continue, NodeB}) ->
              astranaut_traverse_monad:return(NodeB);
         (NodeB) ->
              BMC(NodeB)
      end).

map_m_children(F, Node, Opts) ->
    SyntaxLib = syntax_lib(Opts),
    Subtrees = SyntaxLib:subtrees(Node, Opts),
    map_m_children(F, Node, Subtrees, Opts, SyntaxLib).

map_m_children(_F, Node, [], _Opts, _SyntaxLib) ->
    astranaut_traverse_monad:return(Node);

map_m_children(F, Node, Subtrees, Opts, SyntaxLib) ->
    SyntaxType = SyntaxLib:type(Node),
    astranaut_traverse_monad:bind(
      map_m_subtrees(F, Subtrees, Opts#{parent => SyntaxType}),
      fun(Subtrees1) ->
              Node1 = SyntaxLib:update_subtrees(Node, Subtrees1),
              astranaut_traverse_monad:return(Node1)
      end).

syntax_lib(#{syntax_lib := SyntaxLib}) ->
    SyntaxLib;
syntax_lib(#{}) ->
    astranaut_erl_syntax.

map_m_subtrees(F, Nodes, #{sequence_children := Sequence} = Opts) ->
    Opts1 = maps:remove(sequence_children, Opts),
    SubtreesM = m_subtrees(F, Nodes, Opts1),
    Sequence(SubtreesM);
map_m_subtrees(F, Nodes, Opts) ->
    SubtreesM = m_subtrees(F, Nodes, Opts),
    deep_sequence_m(SubtreesM).

m_subtrees(F, Subtrees, Opts) when is_list(Subtrees) ->
    lists:map(
      fun(Subtree) ->
              m_subtrees(F, Subtree, Opts)
      end, Subtrees);
m_subtrees(_F, {skip, Subtree}, _Opts) ->
    deep_return(Subtree);
m_subtrees(F, {transformer, Subtree, Transformer}, Opts) ->
    astranaut_traverse_monad:bind(
      map_m_subtrees(F, Subtree, Opts),
      fun(Subtree1) ->
              Transformed = Transformer(Subtree1),
              astranaut_traverse_monad:return(Transformed)
      end);
m_subtrees(F, {up_attr, Attr, Subtree}, Opts) when is_map(Attr) ->
    Opts1 = maps:merge(Opts, Attr),
    m_subtrees(F, Subtree, Opts1);
m_subtrees(F, {up_node, Node, Subtree}, Opts) when is_atom(Node) ->
    Opts1 = Opts#{node => Node},
    m_subtrees(F, Subtree, Opts1);
m_subtrees(F, Subtree, Opts) ->
    map_m_1(F, Subtree, Opts).

deep_return(Nodes) when is_list(Nodes) ->
    lists:map(
      fun(Node) ->
              astranaut_traverse_monad:return(Node)
      end, Nodes);
deep_return(Node) ->
    astranaut_traverse_monad:return(Node).

deep_sequence_m([MA|MAs]) -> 
    astranaut_traverse_monad:bind(
      deep_sequence_m(MA),
      fun(A) ->
              astranaut_traverse_monad:bind(
                deep_sequence_m(MAs),
                fun(As) ->
                        astranaut_traverse_monad:return([A|As])
                end)
      end);
deep_sequence_m([]) -> 
    astranaut_traverse_monad:return([]);
deep_sequence_m(MA) -> 
    MA.

deep_r_sequence_m(MAs) ->
    astranaut_traverse_monad:lift_m(fun lists:reverse/1, deep_sequence_m(lists:reverse(MAs))).

transform_mapfold_f(F, Opts) ->
    fun(Node, Attr) ->
            astranaut_traverse_monad:bind(
              astranaut_traverse_monad:get(),
              fun(State) ->
                      Reply = F(Node, State, Attr),
                      fun_return_to_monad(Reply, Node, Opts#{with_state => true})
              end)
    end.

fun_return_to_monad(Return, Node) ->
    fun_return_to_monad(Return, Node, #{}).

fun_return_to_monad(Return, Node, Opts) ->
    SyntaxLib = syntax_lib(Opts),
    MA = astranaut_traverse_monad:return(Node),
    Line = SyntaxLib:get_pos(Node),
    fun_return_to_monad_1(Return, MA, Opts#{line => Line}).

%% transform user sytle traverse return to astranaut_traverse_monad
fun_return_to_monad_1(#{'__struct__' := ?TRAVERSE_FUN_RETURN} = Struct, MA, Opts) ->
    struct_to_monad(Struct, MA, Opts);
fun_return_to_monad_1(continue, MA, _Opts) ->
    astranaut_traverse_monad:then(MA, astranaut_traverse_monad:return(continue));
fun_return_to_monad_1({continue, Node}, MA, _Opts) ->
    astranaut_traverse_monad:then(MA, astranaut_traverse_monad:return({continue, Node}));
fun_return_to_monad_1({error, Reason}, MA, Opts) ->
    NReason = format_error(Reason, Opts),
    astranaut_traverse_monad:then(MA, astranaut_traverse_monad:fail(NReason));
fun_return_to_monad_1({error, Node, Reason}, MA, Opts) ->
    NReason = format_error(Reason, Opts),
    astranaut_traverse_monad:then(
      astranaut_traverse_monad:then(MA, astranaut_traverse_monad:error(NReason)),
      astranaut_traverse_monad:return(Node));
fun_return_to_monad_1({warning, Node, Reason}, MA, Opts) ->
    NReason = format_error(Reason, Opts),
    astranaut_traverse_monad:then(
      astranaut_traverse_monad:then(MA, astranaut_traverse_monad:warning(NReason)),
      astranaut_traverse_monad:return(Node));
fun_return_to_monad_1({ok, Return, ErrorState}, MA, #{}) ->
    astranaut_traverse_monad:then(
      astranaut_traverse_monad:then(
        MA,
        astranaut_traverse_monad:merge_error_state(ErrorState)),
      astranaut_traverse_monad:return(Return));
fun_return_to_monad_1({astranaut_monad_state_t, _} = MonadB, _MA, #{}) ->
    MonadB;
fun_return_to_monad_1({Node, State}, MA, #{with_state := true}) ->
    astranaut_traverse_monad:then(MA, astranaut_traverse_monad:state(fun(_) -> {Node, State} end));
fun_return_to_monad_1(Node, MA, _Opts) ->
    astranaut_traverse_monad:then(MA, astranaut_traverse_monad:return(Node)).

struct_to_monad(#{error := Error} = Reply, MA, Opts) ->
    NError = format_error(Error, Opts),
    MB = astranaut_traverse_monad:left_then(MA, astranaut_traverse_monad:error(NError)),
    NReply = maps:remove(error, Reply),
    struct_to_monad(NReply, MB, Opts);
struct_to_monad(#{errors := Errors} = Reply, MA, Opts) ->
    NErrors = format_errors(Errors, Opts),
    MB = astranaut_traverse_monad:left_then(MA, astranaut_traverse_monad:errors(NErrors)),
    NReply = maps:remove(errors, Reply),
    struct_to_monad(NReply, MB, Opts);
struct_to_monad(#{warning := Warning} = Reply, MA, Opts) ->
    NWarning = format_error(Warning, Opts),
    MB = astranaut_traverse_monad:left_then(MA, astranaut_traverse_monad:warning(NWarning)),
    NReply = maps:remove(warning, Reply),
    struct_to_monad(NReply, MB, Opts);
struct_to_monad(#{warnings := Warnings} = Reply, MA, Opts) ->
    NWarnings = format_errors(Warnings, Opts),
    MB = astranaut_traverse_monad:left_then(MA, astranaut_traverse_monad:warnings(NWarnings)),
    NReply = maps:remove(warnings, Reply),
    struct_to_monad(NReply, MB, Opts);
struct_to_monad(#{state := State} = Reply, MA, #{with_state := true} = Opts) ->
    MB = astranaut_traverse_monad:left_then(MA, astranaut_traverse_monad:put(State)),
    NReply = maps:remove(state, Reply),
    struct_to_monad(NReply, MB, Opts);
struct_to_monad(#{continue := true, node := Node} = Reply, MA, Opts) ->
    MB = astranaut_traverse_monad:then(MA, astranaut_traverse_monad:return({continue, Node})),
    NReply = maps:remove(continue, maps:remove(node, Reply)),
    struct_to_monad(NReply, MB, Opts);
struct_to_monad(#{continue := true} = Reply, MA, Opts) ->
    MB = astranaut_traverse_monad:then(MA, astranaut_traverse_monad:return(continue)),
    NReply = maps:remove(continue, Reply),
    struct_to_monad(NReply, MB, Opts);
struct_to_monad(#{node := Node} = Reply, MA, Opts) ->
    MB = astranaut_traverse_monad:then(MA, astranaut_traverse_monad:return(Node)),
    NReply = maps:remove(node, Reply),
    struct_to_monad(NReply, MB, Opts);
struct_to_monad(#{}, MA, _Opts) ->
    MA.

format_errors(Errors, Opts) ->
    lists:map(fun(Error) -> format_error(Error, Opts) end, Errors).

format_error(#{'__struct__' := ?TRAVERSE_ERROR, reason := Reason} = Error, #{formatter := Module, line := Line}) ->
    ErrorLine = maps:get(line, Error, Line),
    ErrorModule = maps:get(module, Error, Module),
    {ErrorLine, ErrorModule, Reason};
format_error({Line1, Error}, #{formatter := Module}) when is_integer(Line1) ->
    {Line1, Module, Error};
format_error({Line1, Module1, Error}, #{}) when is_integer(Line1), is_atom(Module1) ->
    {Line1, Module1, Error}; 
format_error(Error, #{line := Line, formatter := Module}) ->
    {Line, Module, Error}.

transform_f(F, #{traverse := pre}) ->
    fun(Node, #{step := pre  } = Attr) -> F(Node, Attr);
       (Node, #{step := leaf } = Attr) -> F(Node, Attr);
       (Node, #{step := post }) -> astranaut_traverse_monad:return(Node)
    end;
transform_f(F, #{traverse := post}) ->
    fun(Node, #{step := pre  }) -> astranaut_traverse_monad:return(Node);
       (Node, #{step := leaf } = Attr) -> F(Node, Attr);
       (Node, #{step := post } = Attr) -> F(Node, Attr)
    end;
transform_f(F, #{traverse := leaf}) ->
    fun(Node, #{step := pre  }) -> astranaut_traverse_monad:return(Node);
       (Node, #{step := leaf } = Attr) -> F(Node, Attr);
       (Node, #{step := post }) -> astranaut_traverse_monad:return(Node)
    end;
transform_f(F, _) ->
    F.

map_walk_return(F, WalkReturn) ->
    case WalkReturn of
        #{'__struct__' := ?TRAVERSE_FUN_RETURN} = Map ->
            Map;
        {error, Reason} ->
            {error, Reason};
        {ok, WalkReturn} ->
            {ok, F(WalkReturn)};
        WalkReturn ->
            F(WalkReturn)
    end.

simplify_return({ok, Return, ErrorState}) ->
    case astranaut_traverse_error_state:is_empty(ErrorState) of
        true ->
            Return;
        _ ->
            {ok, Return, ErrorState}
    end;
simplify_return(Other) ->
    Other.

reply_to_traverse_fun_return(Reply, Node) ->
    case reply_to_traverse_fun_return(Reply) of
        #{node := _Node1} = Return ->
            Return;
        #{} = Return ->
            Return#{node => Node};
        Return ->
            Return
    end.

reply_to_traverse_fun_return({ok, Reply, [], []}) ->
    Reply;
reply_to_traverse_fun_return({ok, Reply, [], Warnings}) ->
    traverse_fun_return(#{node => Reply, warnings => Warnings});
reply_to_traverse_fun_return({ok, Reply, Errors, []}) ->
    traverse_fun_return(#{node => Reply, errors => Errors});
reply_to_traverse_fun_return({ok, Reply, Errors, Warnings}) ->
    traverse_fun_return(#{node => Reply, warnings => Warnings, errors => Errors});
reply_to_traverse_fun_return({warning, Reply, Warnings}) ->
    traverse_fun_return(#{node => Reply, warnings => Warnings});
reply_to_traverse_fun_return({error, Reply, Errors}) ->
    traverse_fun_return(#{node => Reply, error => Errors});
reply_to_traverse_fun_return({error, Errors}) ->
    traverse_fun_return(#{errors => Errors});
reply_to_traverse_fun_return(Reply) ->
    Reply.

monad_to_traverse_fun_return(Monad) ->
    monad_to_traverse_fun_return(Monad, #{}).

monad_to_traverse_fun_return(Monad, #{init := Init, with_state := true}) ->
    case astranaut_traverse_monad:run(Monad, Init) of
        {ok, Value, #{errors := [], warnings := []}} ->
            Value;
        {ok, {Node, State}, #{errors := Errors, warnings := Warnings}} ->
            traverse_fun_return(#{node => Node, state => State, errors => Errors, warnings => Warnings});
        {error, #{errors := Errors, warnings := Warnings}} ->
            traverse_fun_return(#{errors => Errors, warnings => Warnings})
    end;
monad_to_traverse_fun_return(Monad, #{} = Opts) ->
    Init = maps:get(init, Opts, ok),
    case astranaut_traverse_monad:run(Monad, Init) of
        {ok, {Node, _}, #{errors := [], warnings := []}} ->
            Node;
        {ok, {Node, _}, #{errors := Errors, warnings := Warnings}} ->
            traverse_fun_return(#{node => Node, errors => Errors, warnings => Warnings});
        {error, #{errors := Errors, warnings := Warnings}} ->
            traverse_fun_return(#{errors => Errors, warnings => Warnings})
    end.
