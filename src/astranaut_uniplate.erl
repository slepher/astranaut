%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2021, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  2 Apr 2021 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_uniplate).

%% API
-export([map/4, reduce/5, mapfold/5]).
-export([uniplate_static/1, uniplate_context/1]).
-export([map_m/5, map_m_static/5]).
-export([is_node_context/1]).
-export([with_subtrees/1, with_subtrees/2, with_subtrees/3]).
-export([keep/0, skip/0, skip/1, up_attr/2, with/3, with_each/3]).
-export([every_tree/2, clamp_trees/3, left_trees/2, right_trees/2]).

-export_type([uniplate/1]).
-export_type([node_context/1]).

-type uniplate(A) :: fun((A) -> {[[A]], fun(([[A]]) -> A)}).

-type monad(M, A) :: astranaut_monad:monad(M, A).
-type monad_opts(M) :: #{bind := astranaut_monad:monad_bind(M),
                         return := astranaut_monad:monad_return(M),
                         ask => astranaut_monad:monad_ask(M),
                         local => astranaut_monad:monad_local(M),
                         state => astranaut_monad:monad_state(M),
                         updated_writer => astranaut_monad:monad_writer(M)
                        }.

-type traverse_opts() :: #{traverse => traverse_style()}.

-record(node_context, {node,
                       withs = [],
                       reduces = [],
                       updated = true,
                       skip = false,
                       up_attrs = [],
                       entries = [],
                       exits = []
                      }).

-type traverse_style() :: pre | post | all | subtree.

-type node_context(A) :: #node_context{node :: A} | A.
%%%===================================================================
%%% API
%%%===================================================================
-spec map(fun((A) -> A) | fun((A, #{}) -> A), A, uniplate(A), traverse_opts()) -> A.
map(F, Node, Uniplate, Opts) when is_function(F, 1) ->
    map_m(F, Node, Uniplate, identity, Opts);
map(F, Node, Uniplate, Opts) when is_function(F, 2) ->
    RA =
        map_m(
          fun(N) ->
                  fun(A) ->
                          F(N, A)
                  end
          end, Node, Uniplate, reader, Opts),
    Attr = maps:get(attr, Opts, #{}),
    RA(Attr).

-spec reduce(fun((A, S) -> S) | fun((A, S, #{}) -> S), S, A, uniplate(A), traverse_opts()) -> S.
reduce(F, Init, Node, Uniplate, Opts) when is_function(F, 2) ->
    SA =
        map_m_static(
          fun(N) ->
                  fun(S) ->
                          S1 = F(N, S),
                          {N, S1}
                  end
          end, Node, Uniplate, state, Opts),
    {_Node1, State1} = SA(Init),
    State1;
reduce(F, Init, Node, Uniplate, Opts) when is_function(F, 3) ->
    SRA =
        map_m_static(
          fun(N) ->
                  fun(S) ->
                          fun(R) ->
                                  S1 = F(N, S, R),
                                  {N, S1}
                          end
                  end
          end, Node, Uniplate, {state, reader}, Opts),
    Attr = maps:get(attr, Opts, #{}),
    {_Node1, State1} = (SRA(Init))(Attr),
    State1.

-spec mapfold(fun((A, S) -> {A, S}) | fun((A, S, #{}) -> {A, S}), S,  A, uniplate(A), traverse_opts()) -> {A, S}.
mapfold(F, Init, Node, Uniplate, Opts) when is_function(F, 2) ->
    SA =
        map_m(
          fun(N) ->
                  fun(S) ->
                          F(N, S)
                  end
          end, Node, Uniplate, state, Opts),
    (SA(Init));
mapfold(F, Init, Node, Uniplate, Opts) when is_function(F, 3) ->
    SRA =
        map_m(
          fun(N) ->
                  fun(S) ->
                          fun(R) ->
                                  F(N, S, R)
                          end
                  end
          end, Node, Uniplate, {state, reader}, Opts),
    Attr = maps:get(attr, Opts, #{}),
    (SRA(Init))(Attr).

-spec uniplate_static(uniplate(A)) -> uniplate(A).
uniplate_static(Uniplate) ->
    fun(Node) ->
            {Subtrees, _MakeTree} = Uniplate(Node),
            {Subtrees, fun(_) -> context_node(Node) end}
    end.

-spec uniplate_context(uniplate(A)) -> uniplate(A).
uniplate_context(Uniplate) ->
    fun(#node_context{node = Node, withs = Withs, reduces = Reduces}) ->
            {Subtrees, MakeTree} = Uniplate(Node),
            Subtrees1 = apply_functions(Withs, Subtrees),
            {Subtrees1, fun(Subtrees2) ->
                                MakeTree(apply_functions(lists:reverse(Reduces), Subtrees2))
                        end};
       (Node) ->
            Uniplate(Node)
    end.

-spec map_m_static(fun((A) -> monad(M, A)), A, uniplate(A), M | monad_opts(M), traverse_opts()) -> monad(M, A).
%% @doc works like map_m/5, but node will not be changed.
map_m_static(F, Node, Uniplate, Monad, Opts) when is_atom(Monad); is_tuple(Monad) ->
    MonadOpts = monad_opts(Monad),
    map_m_static(F, Node, Uniplate, MonadOpts, Opts);
map_m_static(F, Node, Uniplate, #{} = MonadOpts, Opts) ->
    MonadOpts1 = never_updated_writer(MonadOpts),
    map_m_1(F, Node, uniplate_static(Uniplate), MonadOpts1, Opts).

-spec map_m(fun((A) -> monad(M, A)), A, uniplate(A), M | monad_opts(M), traverse_opts()) -> monad(M, A).
%% @doc traverse node with user defined monad with node changed detect.
map_m(F, Node, Uniplate, Monad, Opts) when is_atom(Monad); is_tuple(Monad) ->
    MonadOpts = monad_opts(Monad),
    map_m(F, Node, Uniplate, MonadOpts, Opts);
map_m(F, Node, Uniplate, #{} = MonadOpts, Opts) ->
    with_updated_writer(
      fun(MonadOpts1) ->
              map_m_1(F, Node, uniplate_context(Uniplate), MonadOpts1, Opts)
      end, MonadOpts).

never_updated_writer(#{bind := Bind, return := Return} = Opts) ->
    %% a writer monad just ignore the updated value.
    %% while listen to this monad always return false
    IdentityLift = fun(A) -> A end,
    IgnoreUpdatedWriter = fun({A, _Updated}) -> Return(A) end,
    ListenNeverUpdated = fun(MA) -> Bind(MA, fun(A) -> Return({A, false}) end) end,
    Opts#{updated_writer => IgnoreUpdatedWriter, updated_listen => ListenNeverUpdated, updated_writer_lift => IdentityLift}.

with_updated_writer(Fun, #{updated_writer := _Writer, updated_listen := _Listen} = MonadOpts) ->
    %% if monad already a monad writer, it is not have to lift it
    %% lift function do nothing
    MonadOpts1 = MonadOpts#{updated_writer_lift => fun(A) -> A end},
    Fun(MonadOpts1);
with_updated_writer(Fun, #{bind := Bind, return := Return} = MonadOpts) ->
    %% if monad is not a monad writer, a default writer monad is lifted.
    LiftedOpts = lifted_updated_writer_opts(MonadOpts),
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
    Writer = astranaut_monad:monad_updated_writer(Monad),
    Listen = astranaut_monad:monad_updated_listen(Monad),
    MOpts = #{bind => Bind, return => Return,
              ask => Ask, local => Local,
              state => State,
              updated_writer => Writer, updated_listen => Listen},
    maps:filter(
      fun(_Key, Value) ->
              Value =/= undefined
      end, MOpts).

lifted_updated_writer_opts(#{bind := Bind, return := Return} = MOpts) ->
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
      end, MOpts#{updated_listen => Listen, updated_writer => Writer,
                  updated_writer_lift => Lift, bind => BindW, return => ReturnW}).

map_m_1(F, Node, Uniplate, #{bind := Bind} = MOpts, Opts) ->
    %% set traverse default value to 'pre'.
    Opts1 = maps:merge(#{traverse => pre}, Opts),
    map_m_2(F, Node, Uniplate, #{bind := Bind} = MOpts, Opts1).

map_m_2(F, Nodes, Uniplate, #{bind := Bind, return := Return} = MOpts, Opts) when is_list(Nodes) ->
    %% list maybe returned when traverse one node, map_m_flatten is required.
    map_m_flatten(
      fun(Node) ->
              sub_apply(F, Node, Uniplate, MOpts, Opts)
      end, Nodes, Bind, Return);
map_m_2(F, Node, Uniplate, #{bind := Bind} = MOpts, Opts) ->
    %% Node is simple node
    %% NodeContext1 is node with context
    %% SubNode is sub_node without context
    %% Node1 is node without context
    Bind(
      step_apply(F, Node, pre, MOpts, Opts),
      %% F(Node) -> [Node] | Node
      %% returned value is node or list of node, use map_m_if_list to mapover nodes
      fun(NodeOrNodes) ->
              map_m_if_list(
                %% after pre_apply, NodeContext1 is node with context
                fun(NodeContext1) ->
                        Bind(
                          context_descend_m(
                            fun(SubNode) ->
                                    sub_apply(F, SubNode, Uniplate, MOpts, Opts)
                            end, NodeContext1, Uniplate, MOpts),
                          fun(Node1) ->
                                  step_apply(F, Node1, post, MOpts, Opts)
                          end)
                end, NodeOrNodes, MOpts)
      end).

context_descend_m(F, NodeContext, Uniplate, #{} = Opts) ->
    context_apply(
      %% apply entries, exits, up_attrs, skip in NodeContext
      NodeContext,
      %% NodeContext1 is node context without 'entrie's, 'exit's, 'up_attr's, 'skip', but with 'with's, 'reduce's
      %% 'with' is the context of how to generate node context in subtrees
      %% 'reduce' is the context of how to make tree by subtree
      fun(NodeContext1) ->
              descend_m(
                %% for uniplate_context/1, subtrees of NodeContext1 is also a node with context
                fun(SubtreeContext) ->
                        context_apply(SubtreeContext, fun(SubtreeContext1) -> F(context_node(SubtreeContext1)) end, Opts)
                end, NodeContext1, Uniplate, Opts)
      end, Opts).

descend_m(F, NodeContext, Uniplate, #{bind := Bind, return := Return, updated_listen := ListenUpdated}) ->
    {Subtrees, MakeTree} = Uniplate(NodeContext),
    Bind(
      ListenUpdated(map_subtreess_m(F, Subtrees, Bind, Return)),
      fun({Subtrees1, true}) ->
              Return(MakeTree(Subtrees1));
         ({_Subtrees1, false}) ->
              %% context should be removed if node is not updated.
              Return(context_node(NodeContext))
      end).

%% list of list of subtree, so there is two 's'
map_subtreess_m(F, Subtreess, Bind, Return) ->
    %% list maybe returned when apply f over subtree, map_m_flatten is required.
    astranaut_monad:map_m(fun(Subtrees) -> map_m_flatten(F, Subtrees, Bind, Return) end, Subtreess, Bind, Return).

map_m_flatten(F, As, Bind, Return) ->
    Fold = fun(AHs1, As1) when is_list(AHs1) ->
                   AHs1 ++ As1;
              (A1, As1) ->
                   [A1|As1]
           end,
    astranaut_monad:map_m_fold(F, As, Fold, Bind, Return).

map_m_if_list(AFB, Nodes, #{bind := Bind, return := Return}) when is_list(Nodes) ->
    astranaut_monad:map_m(AFB, Nodes, Bind, Return);
map_m_if_list(AFB, Node, #{}) ->
    AFB(Node).

step_apply(F, Node, pre, MOpts, #{traverse := pre}) ->
    updated_node_apply(F, Node, MOpts);
step_apply(F, Node, post, #{bind := Bind, return := Return} = MOpts, #{traverse := post}) ->
    astranaut_monad:lift_m(fun context_node/1, updated_node_apply(F, Node, MOpts), Bind, Return);
step_apply(F, Node, Step, MOpts, #{traverse := all} = Opts) ->
    NodeM = step_apply(F, Node, Step, MOpts, Opts#{traverse => Step}),
    %% add #{step => Step} to attr while traverse is all
    context_up_attrs(NodeM, [#{step => Step}], MOpts);
step_apply(_F, Node, _Step, #{return := Return}, #{}) ->
    Return(Node).

sub_apply(F, Node, _Uniplate, MOpts, #{traverse := subtree}) ->
    updated_node_apply(F, Node, MOpts);
sub_apply(F, Node, Uniplate, MOpts, Opts) ->
    map_m_2(F, Node, Uniplate, MOpts, Opts).

updated_node_apply(F, Node1, #{updated_writer_lift := Lift, updated_writer := Writer, bind := Bind}) ->
    Bind(
      Lift(F(Node1)),
      fun(Node2) ->
                Writer(updated_node(Node1, Node2))
      end).

context_apply(#node_context{node = Node, skip = true}, _F, #{return := Return}) ->
    Return(Node);
context_apply(#node_context{entries = Entries, exits = Exits, up_attrs = UpAttrs} = Context1, F, MOpts) ->
    %% remove context after applied
    Context2 = Context1#node_context{entries = [], exits = [], up_attrs = [], skip = false},
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

updated_node(Node1, ok) ->
    {Node1, false};
updated_node(Node1, keep) ->
    {Node1, false};
updated_node(Node1, #node_context{node = keep} = NodeContext2) ->
    {NodeContext2#node_context{node = Node1}, false};
updated_node(_Node1, Node2) ->
    {Node2, true}.

context_node(#node_context{node = Node}) ->
    Node;
context_node(Node) ->
    Node.

%%%===================================================================
%%% Apply node with context series functions.
%%%===================================================================
is_node_context(#node_context{}) ->
    true;
is_node_context(_) ->
    false.

with_subtrees(With) ->
    with_subtrees(With, keep).

with_subtrees(With, Reduce) when is_function(With), is_function(Reduce) ->
    with_subtrees(With, Reduce, keep);
with_subtrees(With, #node_context{withs = Withs} = Node) ->
    Node#node_context{withs = [With|Withs]};
with_subtrees(With, Node) ->
    with_subtrees(With, #node_context{node = Node}).

with_subtrees(With, Reduce, #node_context{reduces = Reduces} = Node) ->
    with_subtrees(With, Node#node_context{reduces = [Reduce|Reduces]});
with_subtrees(With, Reduce, Node) ->
    with_subtrees(With, Reduce, #node_context{node = Node}).

keep() ->
    #node_context{node = ok, updated = false}.

skip() ->
    #node_context{skip = true, updated = false}.

skip(Trees) ->
    every_tree(
      fun(#node_context{} = Context) ->
              Context#node_context{skip = true};
         (Node) ->
              #node_context{node = Node, skip = true}
      end, Trees).

up_attr(Attr, Trees) ->
    every_tree(
      fun(#node_context{skip = true} = Context) ->
              Context;
         (#node_context{up_attrs = UpAttrs} = Context) ->
              UpAttrs1 = compose_up_attr(Attr, UpAttrs),
              Context#node_context{up_attrs = UpAttrs1};
         (Node) ->
              #node_context{node = Node, up_attrs = [Attr]}
      end, Trees).

compose_up_attr(Attr, []) ->
    [Attr];
compose_up_attr(Attr0, [Attr1|T]) when is_map(Attr0), is_map(Attr1) ->
    [maps:merge(Attr0, Attr1)|T];
compose_up_attr(Attr0, [Attr1|T]) ->
    [Attr0, Attr1|T].

with(Entry, Exit, Trees) ->
    with_exit(Exit, with_entry(Entry, Trees)).

with_each(Entry, Exit, Trees) ->
    every_tree(
      fun(#node_context{entries = Entries, exits = Exits} = Context) ->
              Context#node_context{entries = [Entry|Entries], exits = [Exit|Exits]};
         (Node) ->
              #node_context{node = Node, entries = [Entry], exits = [Exit]}
      end, Trees).

with_entry(Entry, Trees) ->
    left_trees(
      fun(#node_context{entries = Entries} = Context) ->
              Context#node_context{entries = [Entry|Entries]};
         (Node) ->
              #node_context{node = Node, entries = [Entry]}
      end, Trees).

with_exit(Exit, Trees) ->
    right_trees(
      fun(#node_context{exits = Exits} = Context) ->
              Context#node_context{exits = [Exit|Exits]};
         (Node) ->
              #node_context{node = Node, exits = [Exit]}
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
