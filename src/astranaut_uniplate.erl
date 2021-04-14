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
-export([uniplate_static/1]).
-export([map_m/5, map_m_static/5]).
-export([is_node_context/1]).
-export([with_subtrees/1, with_subtrees/2, with_subtrees/3]).
-export([skip/1, up_attr/2, with/3, with_each/3]).
-export([every_tree/2, clamp_trees/3, left_trees/2, right_trees/2]).

-export_type([uniplate/1]).
-export_type([node_context/1]).

-type uniplate(A) :: fun((A) -> {[[A]], fun(([[A]]) -> A)}).

-type monad(M, A) :: astranaut_monad:monad(M, A).
-type monad_opts(M) :: #{bind := astranaut_monad:monad_bind(M),
                         return := astranaut_monad:monad_return(M),
                         ask => astranaut_monad:monad_ask(M),
                         local => astranaut_monad:monad_local(M),
                         state => astranaut_monad:monad_state(M)}.

-record(node_context, {node, updated = false, withs = [], reduces = [],
                       skip = false, up_attrs = [], entries = [], exits = []
                      }).

-type traverse_style() :: pre | post | all | subtree.

-type node_context(A) :: #node_context{node :: A} | A.
%%%===================================================================
%%% API
%%%===================================================================
-spec map(fun((A) -> A) | fun((A, #{}) -> A), A, uniplate(A), #{}) -> A.
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

-spec reduce(fun((A, S) -> S) | fun((A, S, #{}) -> S), S, A, uniplate(A), #{}) -> S.
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

-spec uniplate_static(uniplate(A)) -> uniplate(A).
uniplate_static(Uniplate) ->
    fun(Node) ->
            {Subtrees, _Context} = Uniplate(Node),
            {Subtrees, fun(_) -> Node end}
    end.

-spec mapfold(fun((A, S) -> {A, S}) | fun((A, S, #{}) -> {A, S}), S, #{}, A, uniplate(A)) -> {A, S}.
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

-spec map_m(fun((A) -> monad(M, A)), A, uniplate(A), M | monad_opts(M), #{traverse => traverse_style()}) -> monad(M, A).
%% @doc traverse node with user defined monad with node changed detect.
map_m(F, Node, Uniplate, #{writer := _Writer} = MonadOpts, Opts) ->
    WriterOpts = MonadOpts#{lift_writer => fun(A) -> A end},
    map_m_monads(F, Node, uniplate_context(Uniplate), WriterOpts, Opts);
map_m(F, Node, Uniplate, #{bind := Bind, return := Return} = MonadOpts, Opts) ->
    WriterOpts = writer_opts(MonadOpts),
    %% remove writer monad transformer.
    astranaut_monad:lift_m(
      fun({Node1, _Updated}) ->
              Node1
      end,
      map_m_monads(F, Node, uniplate_context(Uniplate), WriterOpts, Opts), Bind, Return);
map_m(F, Node, Uniplate, Monad, Opts) when is_atom(Monad); is_tuple(Monad) ->
    MonadOpts = monad_opts(Monad),
    map_m(F, Node, Uniplate, MonadOpts, Opts).

-spec map_m_static(fun((A) -> monad(M, A)), A, uniplate(A), M | monad_opts(M), #{traverse => traverse_style()}) -> monad(M, A).
%% @doc works line map_m/5, but node will not be changed.
map_m_static(F, Node, Uniplate, #{} = MonadOpts, Opts) ->
    map_m_monads(F, Node, uniplate_static(Uniplate), MonadOpts, Opts);
map_m_static(F, Node, Uniplate, Monad, Opts) when is_atom(Monad); is_tuple(Monad) ->
    MonadOpts = monad_opts(Monad),
    map_m_monads(F, Node, Uniplate, MonadOpts, Opts).

writer_opts(#{bind := Bind, return := Return} = MOpts) ->
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
      end, MOpts#{listen => Listen, writer => Writer, lift_writer => Lift, bind => BindW, return => ReturnW}).

monad_opts(Monad) ->
    Bind = astranaut_monad:monad_bind(Monad),
    Return = astranaut_monad:monad_return(Monad),
    Ask = astranaut_monad:monad_ask(Monad),
    Local = astranaut_monad:monad_local(Monad),
    State = astranaut_monad:monad_state(Monad),
    Writer = astranaut_monad:monad_writer(Monad),
    Listen = astranaut_monad:monad_listen(Monad),
    MOpts = #{bind => Bind, return => Return, ask => Ask, local => Local, state => State, writer => Writer, listen => Listen},
    maps:filter(
      fun(_Key, undefined) ->
              false;
         (_Key, _Value) ->
              true
      end, MOpts).

-spec map_m_monads(fun((A) -> monad(M, A)), A, uniplate(A),
                   #{
                     writer => astranaut_monad:monad_writer(M),
                     listen => astranaut_monad:monad_listen(M),
                     lift => astranaut_monad:monad_lift(_W)}, #{traverse => pre | post | all | subtree}) -> monad(M, A).
map_m_monads(F, Nodes, Uniplate, #{bind := Bind, return := Return} = MOpts, Opts) when is_list(Nodes) ->
    astranaut_monad:sequence_m(
      fun(Node) ->
              sub_apply(F, Node, Uniplate, MOpts, Opts)
      end, Nodes, Bind, Return);
map_m_monads(F, Node1, Uniplate, #{bind := Bind} = MOpts, Opts) ->
    %% this function is too compliacated
    %% how to make it simpler.
    %% Node1 is simple node
    %% Node2 is node with context
    %% Node3 is node without entries, exits, up_attrs, skip context, but with withs, reduces
    %% SubNode1 is sub_node with context
    %% SubNode2 is sub_node without context
    %% Node4 is node without context
    pre_apply_bind(
      F, Node1, MOpts, Opts,
      fun(Node2) ->
              Bind(
                context_apply(
                  fun(Node3) ->
                          descend_m(
                            fun(SubNode1) ->
                                    context_apply(
                                      fun(SubNode2) ->
                                              sub_apply(F, context_node(SubNode2), Uniplate, MOpts, Opts)
                                    end, SubNode1, MOpts)
                            end, Node3, Uniplate, MOpts)
                  end, Node2, MOpts),
                fun(Node4) ->
                        post_apply(F, context_node(Node4), MOpts, Opts)
                end)
      end).

descend_m(F, Node, Uniplate, #{bind := Bind, return := Return, listen := Listen}) ->
    {Subtrees, Context} = Uniplate(Node),
    Bind(
      Listen(map_subtreess_m(F, Subtrees, Bind, Return)),
      fun({Subtrees1, true}) ->
              Return(Context(Subtrees1));
         ({_Subtrees1, false}) ->
              Return(Node)
      end);
descend_m(F, Node, Uniplate, #{bind := Bind, return := Return}) ->
    {Subtrees, Context} = Uniplate(Node),
    Bind(
      map_subtreess_m(F, Subtrees, Bind, Return),
      fun(Subtrees1) ->
              Return(Context(Subtrees1))
      end).

context_apply(_F, #node_context{node = Node, skip = true}, #{return := Return}) ->
    Return(Node);
context_apply(F, #node_context{entries = Entries, exits = Exits, up_attrs = UpAttrs} = Context1, MOpts) ->
    %% remove context after applied
    Context2 = Context1#node_context{entries = [], exits = [], up_attrs = [], skip = false},
    context_up_attrs(context_state_changes(F(Context2), Entries, Exits, MOpts), UpAttrs, MOpts);
context_apply(F, Node, #{}) ->
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

pre_apply_bind(F, Node1, #{bind := Bind, return := Return} = MOpts, #{traverse := Traverse} = Opts, AFB)
  when Traverse == pre; Traverse == all ->
    Bind(
      pre_apply(F, Node1, MOpts, Opts),
      fun(Nodes) when is_list(Nodes) ->
              astranaut_monad:sequence_m(AFB, Nodes, Bind, Return);
         (Node2) ->
              AFB(Node2)
      end);
pre_apply_bind(_F, Node1, #{}, #{}, AFB) ->
    AFB(Node1).

pre_apply(F, Node1, MOpts, #{traverse := pre}) ->
    node_apply(F, Node1, MOpts);
pre_apply(F, Node1, MOpts, #{traverse := all}) ->
    context_apply(
      fun(Node2) ->
              node_apply(F, context_node(Node2), MOpts)
      end, up_attr(#{step => pre}, Node1), MOpts).

post_apply(F, Node, #{bind := Bind, return := Return} = MOpts, #{traverse := post}) ->
    %% #node_context should not be returned after post.
    astranaut_monad:lift_m(fun context_node/1, node_apply(F, Node, MOpts), Bind, Return);
post_apply(F, Node1, MOpts, #{traverse := all} = Opts) ->
    context_apply(
      fun(Node2) ->
              post_apply(F, context_node(Node2), MOpts, Opts#{traverse => post})
      end, up_attr(#{step => post}, Node1), MOpts);
post_apply(_F, Node, #{return := Return}, #{}) ->
    Return(Node).

sub_apply(F, Node, _Uniplate, MOpts, #{traverse := subtree}) ->
    node_apply(F, Node, MOpts);
sub_apply(F, Node, Uniplate, MOpts, Opts) ->
    map_m_monads(F, Node, Uniplate, MOpts, Opts).

node_apply(F, Node1, #{lift_writer := Lift, writer := Writer, bind := Bind}) ->
    Bind(
      Lift(F(Node1)),
      fun(Node2) ->
              Writer(updated_node(Node1, Node2))
      end);
node_apply(F, Node1, #{}) ->
    F(Node1).

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

uniplate_context(Uniplate) ->
    fun(#node_context{node = Node, withs = Withs, reduces = Reduces}) ->
            {Subtrees, Context} = Uniplate(Node),
            Subtrees1 = apply_functions(Withs, Subtrees),
            {Subtrees1, fun(Subtrees2) ->
                                Context(apply_functions(lists:reverse(Reduces), Subtrees2))
                        end};
       (Node) ->
            Uniplate(Node)
    end.

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
              Context#node_context{entries = [Exit|Exits]};
         (Node) ->
              #node_context{node = Node, entries = [Exit]}
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

map_subtreess_m(F, Subtreess, Bind, Return) ->
    astranaut_monad:sequence_m(fun(Subtrees) -> sequence_nodes_m(F, Subtrees, Bind, Return) end, Subtreess, Bind, Return).

sequence_nodes_m(F, [H|T], Bind, Return) ->
    Bind(
      F(H),
      fun(H1) ->
              Bind(
                sequence_nodes_m(F, T, Bind, Return),
                fun(T1) ->
                        Return([H1|T1])
                end)
      end);
sequence_nodes_m(_F, [], _Bind, Return) ->
    Return([]).
