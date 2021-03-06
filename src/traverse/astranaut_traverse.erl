%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 18 Sep 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_traverse).

-include_lib("astranaut/include/astranaut_struct_name.hrl").

%% API
-export([map/2, map/3]).
-export([map_with_state/3, map_with_state/4]).
-export([reduce/3, reduce/4]).
-export([mapfold/3, mapfold/4]).
-export([without_errors/1, with_attributes/5]).
-export([map_m/3]).
-export([format_error/1]).

-define(TRAVERSE_FUN_RETURN, traverse_fun_return).
-define(TRAVERSE_ERROR, astranaut_traverse_error).

-type traverse_node() :: tuple().
-type traverse_error() :: term().
-type traverse_state() :: term().
-type traverse_opts() :: #{traverse => traverse_style(), parse_transform => boolean(),
                           simplify_return => boolean(), parent => atom(),
                           match_right_first => boolean(),
                           children => boolean(),
                           sequence_children => fun((any()) -> any()),
                           transform => fun((any()) -> any()),
                           syntax_lib => module(),
                           node => node_type(), formatter => module() }.

-type traverse_error_state() :: #{file => string(), errors => traverse_return_error(), warnings => traverse_return_error(),
                                  file_errors => parse_transform_return_error(), 
                                  file_warnings => parse_transform_return_error()}.

-type node_type() :: attribute | pattern | expression | guard | form.
-type traverse_style() :: traverse_step() | all | list.
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
    Transform = fun({_NNode, State}) -> 
                        State end,
    mapfold(NF, Init, TopNode, Opts#{transform => Transform}).

-spec map_with_state(traverse_state_fun(), State, Node, traverse_opts()) -> traverse_final_return({Node, State}).
map_with_state(F, Init, Node, Opts) ->
    Transform = fun({NNode, _State}) -> NNode end,
    mapfold(F, Init, Node, Opts#{transform => Transform}).

-spec mapfold(traverse_state_fun(), State, Node, traverse_opts()) -> traverse_return({Node, State}).
mapfold(F, Init, Node, Opts) ->
    Opts1 = maps:without([simplify_return, transform_return], Opts),
    Return = mapfold_1(F, Init, Node, Opts1),
    transform_return_m(Return, Opts).

without_errors(Forms) ->
    map(fun(Node, #{}) -> Node end, Forms, #{traverse => list, formatter => ?MODULE, simplify_return => false}).

with_attributes(F, Init, Name, Forms, Opts) ->
    reduce(
      fun({attribute, _Line, Attr, AttrValues}, Acc, #{}) when Name == Attr ->
              add_attribute(F, AttrValues, Acc);
         (_Form, Acc, #{}) ->
              Acc
      end, Init, Forms, Opts#{traverse => list, simplify_return => false}).

add_attribute(F, AttrValues, Acc) when is_list(AttrValues) ->
    astranaut_traverse_m:then(
      astranaut_traverse_m:put(Acc),
      astranaut_monad:map_m(
        fun(AttrValue) ->
                astranaut_traverse_m:bind(
                  astranaut_traverse_m:get(),
                  fun(Acc1) ->
                          add_attribute(F, AttrValue, Acc1)
                  end)
        end, AttrValues, astranaut_traverse_m));
add_attribute(F, AttrValue, Acc) ->
    Acc1 = apply_attribute_f(F, AttrValue, Acc),
    astranaut_walk_return:to_traverse_m(Acc1, ok, #{}).

apply_attribute_f(F, AttributeValue, Acc) when is_function(F, 2) ->
    F(AttributeValue, Acc).

update_opts(Opts) ->
    maps:merge(#{formatter => ?MODULE, traverse => all}, Opts).

-spec map_m(traverse_fun(), Node, traverse_opts()) -> astranaut_monad:monadic(M, Node) when M :: astranaut_monad:monad().
map_m(F, Nodes, Opts) ->
    NOpts = update_opts(Opts),
    NF = transform_f(F, NOpts),
    astranaut_traverse_m:bind(
      astranaut_traverse_m:pop_nodes(map_m_1(NF, Nodes, Opts)),
      fun(Nodes1) when is_list(Nodes) ->
              astranaut_traverse_m:return(Nodes1);
         ([Node|_T]) ->
              astranaut_traverse_m:return(Node);
         ([]) ->
              erlang:error({no_nodes_return, Nodes})
      end).

format_error(Message) ->
    case io_lib:deep_char_list(Message) of
        true -> Message;
        _    -> io_lib:write(Message)
    end.
%%====================================================================
%% Internal functions
%%====================================================================
mapfold_1(F, Init, Node, #{} = Opts) ->
    NF = astranaut_traverse_m:transform_mapfold_f(F),
    NodeM = map_m(NF, Node, Opts),
    Formatter = maps:get(formatter, Opts, ?MODULE),
    astranaut_traverse_m:run(NodeM, Formatter, Init).

map_m_1(F, Nodes, #{traverse := list} = Opts) ->
    map_m_list(F, Nodes, Opts);
map_m_1(F, Nodes, Opts) when is_list(Nodes) ->
    astranaut_traverse_m:sequence_either(
      lists:map(
        fun(Subtree) ->
                map_m_1(F, Subtree, Opts)
        end, Nodes));
map_m_1(F, NodeA, #{children := true} = Opts) ->
    Opts1 = maps:remove(children, Opts),
    astranaut_traverse_m:as_node(map_m_children(F, NodeA, Opts1));
map_m_1(F, NodeA, #{} = Opts) ->
    SyntaxLib = syntax_lib(Opts),
    map_m_tree(F, NodeA, Opts, SyntaxLib).

map_m_list(F, Nodes, #{} = Opts) when is_list(Nodes) ->
    astranaut_traverse_m:sequence_all(
      lists:map(
        fun(Subtree) ->
                map_m_list(F, Subtree, Opts)
        end, Nodes));
map_m_list(_F, {error, Reason}, #{}) ->
    astranaut_traverse_m:then(
      astranaut_traverse_m:formatted_errors([Reason]),
      astranaut_traverse_m:nodes([]));
map_m_list(F, NodeA, Opts) ->
    SyntaxLib = syntax_lib(Opts),
    MA = case SyntaxLib:is_file(NodeA) of
             false ->
                 astranaut_traverse_m:return(ok);
             {file, File} ->
                 astranaut_traverse_m:update_file(File)
         end,
    astranaut_traverse_m:then(MA, apply_f(F, NodeA, #{}, SyntaxLib)).

map_m_tree(_F, {error, Reason}, _Opts, _SyntaxLib) ->
    astranaut_traverse_m:then(
      astranaut_traverse_m:formatted_errors([Reason]),
      astranaut_traverse_m:nodes([]));
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
            astranaut_traverse_m:bind_continue_nodes(
              apply_f(F, NodeA, Attr#{step => PreType, node => NodeType}, SyntaxLib),
              fun(NodeB) ->
                      case SyntaxLib:tps(NodeB, Opts) of
                          {_Type, _Pos, []} ->
                              astranaut_traverse_m:nodes([NodeB]);
                              %% astranaut_traverse_m:to_node(astranaut_traverse_m:return(NodeB));
                          {Parent, _Pos, Subtrees} ->
                              astranaut_traverse_m:bind(
                                map_m_children(F, NodeB, Subtrees, Opts#{parent => Parent}, SyntaxLib),
                                fun(NodeC) ->
                                        astranaut_traverse_m:set_continue(
                                          apply_f(F, NodeC, Attr#{step => post, node => NodeType}, SyntaxLib), false)
                                end)
                      end
              end);
        {file, File} ->
            astranaut_traverse_m:then(
              astranaut_traverse_m:update_file(File),
              apply_f(F, NodeA, Attr#{step => leaf, node => file}, SyntaxLib)
             )
    end.

apply_f(F, Node, Attr, SyntaxLib) ->
    Line = SyntaxLib:get_pos(Node),
    astranaut_traverse_m:updated_node(
      Node, astranaut_traverse_m:update_line(Line, F(Node, Attr))).

map_m_children(F, Node, Opts) ->
    SyntaxLib = syntax_lib(Opts),
    Subtrees = SyntaxLib:subtrees(Node, Opts),
    map_m_children(F, Node, Subtrees, Opts, SyntaxLib).

map_m_children(_F, Node, [], _Opts, _SyntaxLib) ->
    astranaut_traverse_m:return(Node);

map_m_children(F, Node, Subtrees, Opts, SyntaxLib) ->
    SyntaxType = SyntaxLib:type(Node),
    astranaut_traverse_m:bind(
      astranaut_traverse_m:listen_updated(map_m_subtrees(F, Subtrees, Opts#{parent => SyntaxType})),
      fun({_Subtrees1, false}) ->
              astranaut_traverse_m:return(Node);
         ({Subtrees1, true}) ->
              Node1 = SyntaxLib:update_subtrees(Node, Subtrees1),
              astranaut_traverse_m:return(Node1)
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
    astranaut_traverse_m:deep_sequence_m(SubtreesM).


m_subtrees(F, Subtrees, Opts) when is_list(Subtrees) ->
    lists:map(
      fun(Subtree) ->
              m_subtrees(F, Subtree, Opts)
      end, Subtrees);
m_subtrees(_F, {skip, Subtree}, _Opts) ->
    deep_node(Subtree);
m_subtrees(F, {transformer, Subtree, Transformer}, Opts) ->
    astranaut_traverse_m:bind(
      map_m_subtrees(F, Subtree, Opts#{sequence_children => fun astranaut_traverse_m:deep_sequence_m_1/1}),
      fun(Subtree1) ->
              Transformed = Transformer(Subtree1),
              astranaut_traverse_m:nodes(Transformed) 
      end);
m_subtrees(F, {up_attr, Attr, Subtree}, Opts) when is_map(Attr) ->
    Opts1 = maps:merge(Opts, Attr),
    m_subtrees(F, Subtree, Opts1);
m_subtrees(F, {up_node, Node, Subtree}, Opts) when is_atom(Node) ->
    Opts1 = Opts#{node => Node},
    m_subtrees(F, Subtree, Opts1);
m_subtrees(F, Subtree, Opts) ->
    map_m_1(F, Subtree, Opts).

deep_node(Nodes) when is_list(Nodes) ->
    lists:map(
      fun(Node) ->
              astranaut_traverse_m:node(Node)
      end, Nodes);
deep_node(Node) ->
    astranaut_traverse_m:node(Node).

transform_f(F, #{traverse := pre}) ->
    fun(Node, #{step := pre  } = Attr) -> F(Node, Attr);
       (Node, #{step := leaf } = Attr) -> F(Node, Attr);
       (Node, #{step := post }) -> astranaut_traverse_m:return(Node)
    end;
transform_f(F, #{traverse := post}) ->
    fun(Node, #{step := pre  }) -> astranaut_traverse_m:return(Node);
       (Node, #{step := leaf } = Attr) -> F(Node, Attr);
       (Node, #{step := post } = Attr) -> F(Node, Attr)
    end;
transform_f(F, #{traverse := leaf}) ->
    fun(Node, #{step := pre  }) -> astranaut_traverse_m:return(Node);
       (Node, #{step := leaf } = Attr) -> F(Node, Attr);
       (Node, #{step := post }) -> astranaut_traverse_m:return(Node)
    end;
transform_f(F, _) ->
    F.

map_walk_return(F, WalkReturn) ->
    case WalkReturn of
        #{'__struct__' := ?WALK_RETURN} = Map ->
            Map;
        #{'__struct__' := ?TRAVERSE_M} = Map ->
            Map;
        #{'__struct__' := ?RETURN_OK} = Map ->
            Map;
        #{'__struct__' := ?RETURN_FAIL} = Map ->
            Map;
        {error, Reason} ->
            {error, Reason};
        {warning, WalkReturn1, Warning} ->
            {Return, State} = F(WalkReturn1),
            astranaut_walk_return:new(#{return => Return, state => State, warning => Warning});
        {ok, WalkReturn1} ->
            {Return, State} = F(WalkReturn1),
            astranaut_walk_return:new(#{return => Return, state => State});
        {continue, WalkReturn1} ->
            {Return, State} = F(WalkReturn1),
            astranaut_walk_return:new(#{continue => true, return => Return, state => State});
        WalkReturn ->
            F(WalkReturn)
    end.

transform_return_m(Return, Opts) ->
    Return1 = 
        case maps:find(transform, Opts) of
            {ok, Transform} ->
                astranaut_monad:lift_m(Transform, Return, astranaut_return_m);
            error ->
                Return
        end,
    case maps:get(parse_transform, Opts, false) of
        true ->
            astranaut_return_m:to_compiler(Return1);
        false ->
            case maps:get(simplify_return, Opts, true) of
                true ->
                    astranaut_return_m:simplify(Return1);
                false ->
                    Return1
            end
    end.
