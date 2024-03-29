%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2021, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  2 Apr 2021 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_uniplate).

-include("stacktrace.hrl").

-export_type([uniplate/1]).
-export_type([node_context/1]).

-type uniplate(A) :: fun((A) -> {[[A]], fun(([[A]]) -> A)}).

-type monad(M, A) :: astranaut_monad:monad(M, A).
-type monad_opts(M) :: #{bind := astranaut_monad:monad_bind(M),
                         return := astranaut_monad:monad_return(M),
                         ask => astranaut_monad:monad_ask(M),
                         local => astranaut_monad:monad_local(M),
                         state => astranaut_monad:monad_state(M),
                         writer_updated => astranaut_monad:monad_writer(M),
                         listen_updated => astranaut_monad:monad_listen(M)
                        }.

-record(uniplate_node_context, {node,
                                withs = [],
                                reduces = [],
                                skip = false,
                                up_attrs = [],
                                entries = [],
                                exits = []
                               }).

-type traverse_style() :: pre | post | all | subtree | none.
-type node_context(A) :: #uniplate_node_context{node :: A} | A.
-type with_nodes(Node) :: fun(([[Node]]) -> [[node_context(Node)]]).
-type reduce_nodes(Node) :: fun(([[node_context(Node)]]) -> [[node_context(Node)]]).
-type maybe_list(A) :: [A] | A.

%% API
-export([map_m/5]).
%% Apply node with context series functions.
-export([with_subtrees/2, with_subtrees/3]).
-export([skip/1, up_attr/2, with/3, with_each/3]).
-export([every_tree/2, clamp_trees/3, left_trees/2, right_trees/2]).

%%%===================================================================
%%% API
%%%===================================================================
-spec map_m(fun((N) -> monad(M, N)), N, uniplate(N), M | monad_opts(M),
            #{traverse => traverse_style(), static => boolean()}) -> monad(M, N).
%% @doc traverse node with user defined monad.
map_m(F, Node, Uniplate, Monad, Opts) when is_atom(Monad); is_tuple(Monad) ->
    MonadOpts = monad_opts(Monad),
    map_m(F, Node, Uniplate, MonadOpts, Opts);
map_m(F, Node, Uniplate, #{} = MonadOpts, Opts) ->
    Static = maps:get(static, Opts, false),
    Opts1 = maps:merge(#{traverse => pre}, maps:with([traverse], Opts)),
    UniplateContext = uniplate_context(Uniplate),
    with_writer_updated(
      fun(MonadOpts1) ->
              map_m_1(F, Node, UniplateContext, MonadOpts1, Opts1)
      end, MonadOpts, Static).

-spec uniplate_context(uniplate(A)) -> uniplate(A).
uniplate_context(Uniplate) ->
    fun(#uniplate_node_context{node = Node, withs = Withs, reduces = Reduces}) ->
            {Subtrees, MakeTree} = Uniplate(Node),
            Subtrees1 = apply_functions(Withs, Subtrees),
            {Subtrees1, fun(Subtrees2) ->
                                MakeTree(apply_functions(lists:reverse(Reduces), Subtrees2))
                        end};
       (Node) ->
            Uniplate(Node)
    end.

with_writer_updated(Fun, #{bind := Bind, return := Return} = Opts, true) ->
    %% a writer monad just ignore the updated value.
    %% while listen to this monad always return false
    IdentityLift = fun(A) -> A end,
    IgnoreUpdatedWriter = fun({A, _Updated}) -> Return(A) end,
    ListenNeverUpdated = fun(MA) -> Bind(MA, fun(A) -> Return({A, false}) end) end,
    Opts1 = Opts#{writer_updated => IgnoreUpdatedWriter, listen_updated => ListenNeverUpdated,
                  writer_updated_lift => IdentityLift},
    Fun(Opts1);
with_writer_updated(Fun, #{writer_updated := _Writer, listen_updated := _Listen} = Opts, false) ->
    %% if monad already a monad writer, it is not have to lift it
    %% lift function do nothing
    Opts1 = Opts#{writer_updated_lift => fun(A) -> A end},
    Fun(Opts1);
with_writer_updated(Fun, #{bind := Bind, return := Return} = MonadOpts, false) ->
    %% if monad is not a monad writer, a default WriterT is lifted.
    LiftedOpts = lifted_writer_updated_opts(MonadOpts),
    %% map_m_monads returns writer monad.
    NodeUpdated = Fun(LiftedOpts),
    %% remove the lifted writer monad transformer.
    astranaut_monad:lift_m(
      fun({Node1, _Updated}) ->
              Node1
      end, NodeUpdated, Bind, Return).

monad_opts(Monad) ->
    Bind = astranaut_monad:monad_bind(Monad),
    Return = astranaut_monad:monad_return(Monad),
    Ask = astranaut_monad:monad_ask(Monad),
    Local = astranaut_monad:monad_local(Monad),
    State = astranaut_monad:monad_state(Monad),
    Writer = astranaut_monad:monad_writer_updated(Monad),
    Listen = astranaut_monad:monad_listen_updated(Monad),
    FailOnError = astranaut_monad:monad_fail_on_error(Monad),
    CatchOnError = astranaut_monad:monad_catch_on_error(Monad),
    MOpts = #{bind => Bind, return => Return,
              ask => Ask, local => Local,
              state => State,
              writer_updated => Writer, listen_updated => Listen,
              fail_on_error => FailOnError, catch_on_error => CatchOnError
             },
    maps:filter(
      fun(_Key, Value) ->
              Value =/= undefined
      end, MOpts).

lifted_writer_updated_opts(#{bind := Bind, return := Return} = MOpts) ->
    Mempty = astranaut_monad:mempty('or'),
    Mappend = astranaut_monad:mappend('or'),
    BindW = astranaut_monad:writer_bind(Bind, Return, Mappend),
    ReturnW = astranaut_monad:writer_return(Return, Mempty),
    Lift = astranaut_monad:writer_lift(Bind, Return, Mempty),
    Writer = astranaut_monad:writer_writer(Return),
    Listen = astranaut_monad:writer_listen(Bind, Return),
    maps:map(
      fun(ask, Ask) ->
              astranaut_monad:writer_ask(Lift, Ask);
         (local, Local) ->
              astranaut_monad:writer_local(Local);
         (state, State) ->
              astranaut_monad:writer_state(Lift, State);
         (_Key, Value) ->
              Value
      end, MOpts#{listen_updated => Listen, writer_updated => Writer,
                  writer_updated_lift => Lift, bind => BindW, return => ReturnW}).

map_m_1(F, Nodes, Uniplate, #{bind := Bind, return := Return} = MOpts, Opts) when is_list(Nodes) ->
    %% list maybe returned when traverse one node, map_m_flatten is required.
    astranaut_monad:map_m_flatten(
      fun(Node) ->
              catch_on_error(map_m_2(F, Node, Uniplate, MOpts, Opts), fun() -> Return([]) end, MOpts)
      end, Nodes, Bind, Return);
map_m_1(F, Node, Uniplate, MOpts, Opts) ->
    map_m_2(F, Node, Uniplate, MOpts, Opts).

map_m_2(F, Node, Uniplate, MOpts, #{traverse := none}) ->
    validated_transform(F, Node, Uniplate, MOpts, invalid_transform);
map_m_2(F, Node, Uniplate, #{bind := Bind} = MOpts, Opts) ->
    %% Node is simple node
    %% NodeContext1 is node with context
    %% SubNode is sub_node without context
    %% Node1 is node without context
    Bind(
      step_apply(F, Node, pre, Uniplate, MOpts, Opts),
      %% F(Node) -> [Node] | Node
      %% returned value is node or list of node, use map_m_if_list to mapover nodes
      fun(NodeOrNodes) ->
              map_m_if_list(
                %% after pre_apply, NodeContext1 is node with context
                fun(NodeContext1) ->
                        Bind(
                          descend_m_1(
                            fun(SubNode) ->
                                    %% add parent to subtree incase of exception raised
                                    sub_apply(F, SubNode, Uniplate, MOpts, Opts#{parent => Node})
                            end, Node, NodeContext1, Uniplate, MOpts, Opts),
                          fun(Node1) ->
                                  step_apply(F, Node1, post, Uniplate, MOpts, Opts)
                          end)
                end, NodeOrNodes, MOpts)
      end).

sub_apply(F, Node, Uniplate, MOpts, #{traverse := subtree}) ->
    validated_transform(F, Node, Uniplate, MOpts, invalid_subtree_transform);
sub_apply(F, Node, Uniplate, MOpts, Opts) ->
    map_m_2(F, Node, Uniplate, MOpts, Opts).

descend_m_1(F, Node, NodeContext, Uniplate, #{} = MOpts, #{} = Opts) ->
    context_apply(
      %% apply entries, exits, up_attrs, skip in NodeContext
      NodeContext,
      %% NodeContext1 is node context without 'entrie's, 'exit's, 'up_attr's, 'skip', but with 'with's, 'reduce's
      %% 'with' is the context of how to generate node context in subtrees
      %% 'reduce' is the context of how to make tree by subtree
      fun(NodeContext1) ->
              descend_m_2(
                %% for uniplate_context/1, subtrees of NodeContext1 is also a node with context
                fun(SubtreeContext) ->
                        context_apply(SubtreeContext, fun(SubtreeContext1) -> F(context_node(SubtreeContext1)) end, MOpts)
                end, Node, NodeContext1, Uniplate, MOpts, Opts)
      end, MOpts).

descend_m_2(F, Node, NodeContext, Uniplate, #{bind := Bind, return := Return, listen_updated := ListenUpdated} = MOpts, Opts) ->
    %% it's not wise to generate subtrees twice,
    %% validate_pre_transform chould be done here.
    case uniplate(Uniplate, Node, NodeContext, Opts, invalid_pre_transform) of
        {[], _MakeTree} ->
            Return(context_node(NodeContext));
        {Subtreess, MakeTree} ->
            Bind(
              ListenUpdated(map_m_subtreess(F, Subtreess, MOpts)),
              fun({Subtrees1, true}) ->
                      Return(make_tree(MakeTree, Node, Subtreess, Subtrees1));
                 ({_Subtrees1, false}) ->
                      %% context should be removed if node is not updated.
                      Return(context_node(NodeContext))
              end)
    end.

map_m_subtreess(F, Subtreess, #{bind := Bind, return := Return} = MOpts) ->
    fail_on_error(
      astranaut_monad:map_m(
        fun(Subtrees) ->
                astranaut_monad:map_m_flatten(
                  fun(Subtree) ->
                          catch_on_error(F(Subtree), fun() -> Return([]) end, MOpts)
                  end, Subtrees, Bind, Return)
        end, Subtreess, Bind, Return), MOpts).

fail_on_error(MA, #{fail_on_error := FailOnError}) ->
    FailOnError(MA);
fail_on_error(MA, #{}) ->
    MA.

catch_on_error(MA, FMA, #{catch_on_error := CatchOnError}) ->
    CatchOnError(MA, FMA);
catch_on_error(MA, _FMA, #{}) ->
    MA.

%% add extra info to exception raised from Uniplate
uniplate(Uniplate, Node, NodeContext1, Opts, ExceptionType) ->
    try Uniplate(NodeContext1) of
        {Subtreess, MakeTree} ->
            {Subtreess, MakeTree}
    catch
        EType:Exception?CAPTURE_STACKTRACE ->
            case context_node(NodeContext1) of
                Node ->
                    case maps:find(parent, Opts) of
                        {ok, Parent} ->
                            erlang:raise(EType, {invalid_uniplate_subnode, Parent, Node, Exception}, ?GET_STACKTRACE);
                        error ->
                            erlang:raise(EType, {invalid_node, Node, Exception}, ?GET_STACKTRACE)
                        end;
                Node1 ->
                    erlang:raise(EType, {ExceptionType, Node, Node1, Exception}, ?GET_STACKTRACE)
            end
    end.

%% add extra info to exception raised from MakeTree
make_tree(MakeTree, Node, Subtrees, Subtrees1) ->
    try MakeTree(Subtrees1) of
        Node1 ->
            Node1
    catch
        EType:Exception?CAPTURE_STACKTRACE ->
            erlang:raise(EType, {invalid_transform_maketree, Node, Subtrees, Subtrees1, Exception}, ?GET_STACKTRACE)
    end.

map_m_if_list(AFB, Nodes, #{bind := Bind, return := Return}) when is_list(Nodes) ->
    astranaut_monad:map_m_flatten(AFB, Nodes, Bind, Return);
map_m_if_list(AFB, Node, #{}) ->
    AFB(Node).

step_apply(F, Node, pre, _Uniplate, MOpts, #{traverse := pre}) ->
    updated_node_apply(F, Node, MOpts);
step_apply(F, Node, post, Uniplate, #{} = MOpts, #{traverse := post}) ->
    validated_transform(F, Node, Uniplate, MOpts, invalid_post_transform);
step_apply(F, Node, Step, Uniplate, MOpts, #{traverse := all} = Opts) ->
    NodeM = step_apply(F, Node, Step, Uniplate, MOpts, Opts#{traverse => Step}),
    %% add #{step => Step} to attr while traverse is all
    context_up_attrs(NodeM, [#{step => Step}], MOpts);
step_apply(_F, Node, _Step, _Uniplate, #{return := Return}, #{}) ->
    Return(Node).

updated_node_apply(F, Node1, #{writer_updated_lift := Lift, writer_updated := WriterUpdated, bind := Bind}) ->
    Bind(
      Lift(F(Node1)),
      fun(Node2) ->
                WriterUpdated(updated_node(Node1, Node2))
      end).

validated_transform(F, Node, Uniplate, #{bind := Bind, return := Return} = MOpts, ExceptionType) ->
    Bind(
      updated_node_apply(F, Node, MOpts),
      fun(NodeOrNodes) ->
              validate_transformed_node(Uniplate, Node, NodeOrNodes, ExceptionType),
              Return(NodeOrNodes)
      end).

validate_transformed_node(Uniplate, Node, Nodes, ExceptionType) when is_list(Nodes) ->
    lists:foreach(fun(Node1) -> validate_transformed_node_1(Uniplate, Node, Node1, ExceptionType) end, Nodes);
validate_transformed_node(Uniplate, Node, Node1, ExceptionType) ->
    validate_transformed_node_1(Uniplate, Node, Node1, ExceptionType).

validate_transformed_node_1(Uniplate, Node, Node1, ExceptionType) ->
    case Node1 of
        #uniplate_node_context{} ->
            ContextExceptionType = list_to_atom(atom_to_list(ExceptionType) ++ "_with_context"),
            erlang:error({ContextExceptionType, Node, Node1});
        Node ->
            ok;
        Node1 ->
            uniplate(Uniplate, Node, Node1, #{}, ExceptionType)
    end.

%%%===================================================================
%%% apply node context in map_m/5 series functions
%%%===================================================================
context_apply(#uniplate_node_context{node = Node, skip = true}, _F, #{return := Return}) ->
    Return(Node);
context_apply(#uniplate_node_context{entries = Entries, exits = Exits, up_attrs = UpAttrs} = Context1, F, MOpts) ->
    %% remove context after applied
    Context2 = Context1#uniplate_node_context{entries = [], exits = [], up_attrs = [], skip = false},
    context_up_attrs(context_state_changes(F(Context2), Entries, Exits, MOpts), UpAttrs, MOpts);
context_apply(Node, F, #{}) ->
    F(Node).

context_up_attrs(MA, [], #{}) ->
    MA;
context_up_attrs(MA, UpAttrs, #{local := Local}) ->
    Local(fun(Attr0) -> apply_up_attrs(UpAttrs, Attr0) end, MA);
context_up_attrs(MA, _UpAttrs, #{}) ->
    MA.

context_state_changes(MA, [], [], #{}) ->
    MA;
context_state_changes(MA, Entries, Exits, #{bind := Bind, state := State}) ->
    Bind(
      State(fun(S0) -> {ok, apply_modifies(Entries, S0)} end),
      fun(ok) ->
              Bind(
                MA,
                fun(A) ->
                        State(fun(S1) -> {A, apply_modifies(lists:reverse(Exits), S1)} end)
                end)
      end);
context_state_changes(MA, _Entries, _Exits, #{}) ->
    MA.

apply_up_attrs([], Attr) ->
    Attr;
apply_up_attrs([Attr1|T], Attr) when is_map(Attr1) ->
    apply_up_attrs(T, maps:merge(Attr, Attr1));
apply_up_attrs([UpAttr|T], Attr) when is_function(UpAttr, 1) ->
    apply_up_attrs(T, UpAttr(Attr)).

apply_modifies([], S) ->
    S;
apply_modifies([Modify|T], S) when is_function(Modify, 1) ->
    apply_modifies(T, Modify(S));
apply_modifies([S1|T], S) when not is_function(S) ->
    apply_modifies(T, S1).

apply_functions([], Value) ->
    Value;
apply_functions([F|T], Value) when is_function(F, 1) ->
    apply_functions(T, F(Value)).

updated_node(Node1, #uniplate_node_context{node = Node1} = NodeContext2) ->
    {NodeContext2#uniplate_node_context{node = Node1}, false};
updated_node(Node1, Node1) ->
    {Node1, false};
updated_node(_Node1, Node2) ->
    {Node2, true}.

context_node(#uniplate_node_context{node = Node}) ->
    Node;
context_node(Node) ->
    Node.

%%%===================================================================
%%% Apply node with context series functions.
%%%===================================================================
-spec with_subtrees(with_nodes(Node), reduce_nodes(Node) | node_context(Node)) -> node_context(Node).
with_subtrees(With, #uniplate_node_context{withs = Withs} = Node) ->
    Node#uniplate_node_context{withs = [With|Withs]};
with_subtrees(With, Node) ->
    with_subtrees(With, #uniplate_node_context{node = Node}).

-spec with_subtrees(with_nodes(Node), reduce_nodes(Node), node_context(Node)) -> node_context(Node).
with_subtrees(With, Reduce, #uniplate_node_context{reduces = Reduces} = Node) ->
    with_subtrees(With, Node#uniplate_node_context{reduces = [Reduce|Reduces]});
with_subtrees(With, Reduce, Node) ->
    with_subtrees(With, Reduce, #uniplate_node_context{node = Node}).

-spec skip(maybe_list(node_context(Node))) -> maybe_list(node_context(Node)).
skip(Trees) ->
    every_tree(
      fun(#uniplate_node_context{} = Context) ->
              Context#uniplate_node_context{skip = true};
         (Node) ->
              #uniplate_node_context{node = Node, skip = true}
      end, Trees).

-spec up_attr(fun((map()) -> map()) | map(), maybe_list(node_context(Node))) -> maybe_list(node_context(Node)).
up_attr(Attr, Trees) ->
    every_tree(
      fun(#uniplate_node_context{skip = true} = Context) ->
              Context;
         (#uniplate_node_context{up_attrs = UpAttrs} = Context) ->
              UpAttrs1 = compose_up_attr(Attr, UpAttrs),
              Context#uniplate_node_context{up_attrs = UpAttrs1};
         (Node) ->
              #uniplate_node_context{node = Node, up_attrs = [Attr]}
      end, Trees).

compose_up_attr(Attr, []) ->
    [Attr];
compose_up_attr(Attr0, [Attr1|T]) when is_map(Attr0), is_map(Attr1) ->
    [maps:merge(Attr0, Attr1)|T];
compose_up_attr(Attr0, [Attr1|T]) ->
    [Attr0, Attr1|T].

-spec with(fun((S) -> S) | S, fun((S) -> S) | S, maybe_list(node_context(Node))) -> maybe_list(node_context(Node)).
with(Entry, Exit, Trees) ->
    with_exit(Exit, with_entry(Entry, Trees)).

-spec with_each(fun((S) -> S) | S, fun((S) -> S) | S, maybe_list(node_context(Node))) -> maybe_list(node_context(Node)).
with_each(Entry, Exit, Trees) ->
    every_tree(
      fun(#uniplate_node_context{entries = Entries, exits = Exits} = Context) ->
              Context#uniplate_node_context{entries = [Entry|Entries], exits = [Exit|Exits]};
         (Node) ->
              #uniplate_node_context{node = Node, entries = [Entry], exits = [Exit]}
      end, Trees).

with_entry(Entry, Trees) ->
    left_trees(
      fun(#uniplate_node_context{entries = Entries} = Context) ->
              Context#uniplate_node_context{entries = [Entry|Entries]};
         (Node) ->
              #uniplate_node_context{node = Node, entries = [Entry]}
      end, Trees).

with_exit(Exit, Trees) ->
    right_trees(
      fun(#uniplate_node_context{exits = Exits} = Context) ->
              Context#uniplate_node_context{exits = [Exit|Exits]};
         (Node) ->
              #uniplate_node_context{node = Node, exits = [Exit]}
      end, Trees).

every_tree(F, Trees) when is_list(Trees) ->
    lists:map(fun(Tree) -> every_tree(F, Tree) end, Trees);
every_tree(F, Tree) ->
    F(Tree).

clamp_trees(Left, Right, Trees) ->
    left_trees(Left, right_trees(Right, Trees)).

left_trees(F, [[]|T]) ->
    [[]|left_trees(F, T)];
left_trees(F, [Head|T]) ->
    [left_trees(F, Head)|T];
left_trees(_F, []) ->
    [];
left_trees(F, Tree) ->
    F(Tree).

right_trees(F, [Head|_T] = Trees) when is_list(Head) ->
    lists:reverse(right_trees_1(F, lists:reverse(Trees)));
right_trees(F, Trees) when is_list(Trees) ->
    lists:reverse(left_trees(F, lists:reverse(Trees)));
right_trees(F, Tree) ->
    left_trees(F, Tree).

right_trees_1(F, [[]|T]) ->
    [[]|right_trees_1(F, T)];
right_trees_1(F, [Head|T]) when is_list(Head) ->
    [right_trees(F, Head)|T].
