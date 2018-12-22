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
-export([map/2, map/3]).
-export([map_with_state/3, map_with_state/4]).
-export([reduce/3, reduce/4, reduce_e/3, reduce_e/4]).
-export([mapfold/3, mapfold/4, mapfolde/3, mapfolde/4]).
-export([map_m/4, map_m/5]).

-type traverse_opts() :: #{traverse := traverse_style()}.
-type traverse_style() :: pre | post | all.
-type traverse_fun(Node, Return) :: fun((_Type, Node) -> Return) | fun((Node) -> Return).
-type traverse_state_fun(Node, State, Return) :: fun((_Type, State, Node) -> Return) | fun((Node, State) -> Return).

%%%===================================================================
%%% API
%%%===================================================================
map(F, TopNode) ->
    map(F, TopNode, #{}).

reduce(F, Init, TopNode) ->
    reduce(F, Init, TopNode, #{}).

map_with_state(F, Init, Form) ->
    map_with_state(F, Init, Form, #{}).

reduce_e(F, Init, TypeNode) ->
    reduce_e(F, Init, TypeNode, #{}).

-spec mapfold(traverse_state_fun(Node, State, {Node, State}), State, Node) -> {Node, State}.
mapfold(F, Init, Node) ->
    mapfold(F, Init, Node, #{}).

mapfolde(F, Init, Node) ->
    mapfolde(F, Init, Node, #{}).

-spec map(fun((_Type, Node) -> Node), Node) -> Node.
map(F, TopNode, Opts) ->
    NF = fun(Node, State, Attr) ->
                 {F(Node, Attr), State}
         end,
    map_with_state(NF, ok, TopNode, Opts).

-spec map_with_state(fun((_Type, Node, State) -> {Node, State}), State, Node) -> Node.
map_with_state(F, Init, Forms, Opts) ->
    {NForms, _State} = mapfold(F, Init, Forms, Opts),
    NForms.

-spec reduce(fun((_Type, Node, State) -> State), State, Node, traverse_opts()) -> State.
reduce(F, Init, TopNode, Style) ->
    NF = 
        fun(Node, State, Attr) ->
                {Node, F(Node, State, Attr)}
        end,
    {_NForms, NState} = 
        mapfold(NF, Init, TopNode, Style),
    NState.

reduce_e(F, Init, TopNode, Opts) ->
    NF = 
        fun(Node, State, Attr) ->
                {Node, F(Node, State, Attr)}
        end,
    case mapfolde(NF, Init, TopNode, Opts) of
        {ok, {_NForms, NState}} ->
            {ok, NState};
        {error, Reason} ->
            {error, Reason}
    end.

-spec mapfold(traverse_state_fun(Node, State, {Node, State}), State, Node, traverse_opts()) -> {Node, State}.
mapfold(F, Init, Node, Opts) ->
    Monad = astranaut_monad:new_state(),
    NF = transform_mapfoldm_f(F, Monad),
    NodeM = map_m(NF, Node, Monad, Opts),
    astranaut_monad:run_state(NodeM, Init).

-spec mapfolde(traverse_state_fun(Node, State, {Node, State}), State, Node, traverse_opts()) -> {Node, State}.
mapfolde(F, Init, Node, Opts) ->
    Monad = astranaut_monad:new_state_error(),
    NF = transform_mapfoldm_f(F, Monad),
    NodeM = map_m(NF, Node, Monad, Opts),
    astranaut_monad:run_state_error(NodeM, Init).

map_m(F, Nodes, Monad, Opts) ->
    map_m(F, Nodes, Monad, Opts, #{node => form}).

-spec map_m(traverse_fun(Node, astranaut_monad:monadic(M, Node)), Node, M, traverse_opts(), #{}) -> 
                   astranaut_monad:monadic(M, Node) when M :: astranaut_monad:monad().
map_m(F, Nodes, Monad, Opts, Attrs) ->
    TraverseStyle = maps:get(traverse, Opts, all),
    NF = transform_f(F, Monad, TraverseStyle),
    map_m_1(NF, Nodes, Monad, Attrs).
    
-spec map_m_1(traverse_fun(Node, astranaut_monad:monadic(M, Node)), Node, M, #{}) -> 
                   astranaut_monad:monadic(M, Node) when M :: astranaut_monad:monad().
map_m_1(F, Nodes, Monad, Attrs) when is_list(Nodes) ->
    astranaut_monad:map_m(
      fun(Subtree) ->
              map_m_1(F, Subtree, Monad, Attrs)
      end, Nodes, Monad);
map_m_1(F, XNode, Monad, Attrs) ->
    PreType = 
        case erl_syntax:subtrees(XNode) of
            [] ->
                leaf;
            _Subtrees ->
                pre
        end,
    %% do form
    %% do([Monad ||
    %%           YNode <- F(pre, XNode),
    %%           NSubtrees <- map_m(F, Subtrees, Monad),
    %%           ZNode = erl_syntax:revert(erl_syntax:update_tree(YNode, NSubTrees)),
    %%           F(post, ZNode)
    %%    ]).
    astranaut_monad:bind(
      F(XNode, Attrs#{step => PreType}),
      fun(YNode) ->
              case erl_syntax:subtrees(YNode) of
                  [] ->
                      astranaut_monad:return(YNode, Monad);
                  Subtrees ->
                      NodeType = erl_syntax:type(YNode),
                      astranaut_monad:bind(
                        map_m_subtrees(F, Subtrees, Monad, NodeType, Attrs),
                        fun(NSubTrees) ->
                                ZNode = erl_syntax:revert(erl_syntax:update_tree(YNode, NSubTrees)),
                                F(ZNode, Attrs#{step => PreType})
                        end, Monad)
              end
      end, Monad).

map_m_subtrees(F, [Pattern|Rest], Monad, NodeType, #{node := pattern} = Attrs) when NodeType == match_expr; NodeType == clause ->
    astranaut_monad:bind(
      map_m_1(F, Pattern, Monad, Attrs#{node => pattern}),
      fun(NHead) ->
              astranaut_monad:bind(
                map_m_1(F, Rest, Monad, Attrs#{node => expression}),
                fun(NRest) ->
                        astranaut_monad:return([NHead|NRest])
                end)
      end);
map_m_subtrees(F, Nodes, Monad, _NodeType, Attrs) ->
    map_m_1(F, Nodes, Monad, Attrs).


%%====================================================================
%% Internal functions
%%====================================================================
transform_mapfoldm_f(F, Monad) ->
    fun(Node, Attr) ->
            astranaut_monad:state(
              fun(State) ->
                      F(Node, State, Attr)
              end, Monad)
    end.

transform_f(F, Monad, pre) ->
    fun(Node, #{step := pre  } = Attr) -> F(Node, Attr);
       (Node, #{step := leaf } = Attr) -> F(Node, Attr);
       (Node, #{step := post }) -> astranaut_monad:return(Node, Monad)
    end;
transform_f(F, Monad, post) ->
    fun(Node, #{step := pre  }) -> astranaut_monad:return(Node, Monad);
       (Node, #{step := leaf } = Attr) -> F(Node, Attr);
       (Node, #{step := post } = Attr) -> F(Node, Attr)
    end;
transform_f(F, Monad, leaf) ->
    fun(Node, #{step := pre  }) -> astranaut_monad:return(Node, Monad);
       (Node, #{step := leaf } = Attr) -> F(Node, Attr);
       (Node, #{step := post }) -> astranaut_monad:return(Node, Monad)
    end;
transform_f(F, _Monad, _) ->
    F.
