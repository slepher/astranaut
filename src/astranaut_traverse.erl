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
-export([map_m/3, map_m/4]).

-type traverse_style() :: pre | post | all.
-type traverse_fun(Node, Return) :: fun((_Type, Node) -> Return) | fun((Node) -> Return).
-type traverse_state_fun(Node, State, Return) :: fun((_Type, State, Node) -> Return) | fun((Node, State) -> Return).

%%%===================================================================
%%% API
%%%===================================================================
map(F, TopNode) ->
    map(F, TopNode, all).

-spec map(fun((_Type, Node) -> Node), Node) -> Node.
map(F, TopNode, Style) ->
    NNF = with_style(
           fun(NF) ->
                   fun(Node, State) ->
                           {NF(Node), State}
                   end
           end, F, Style),
    map_with_state(NNF, ok, TopNode, Style).

map_with_state(F, Init, Form) ->
    map_with_state(F, Init, Form, all).

-spec map_with_state(fun((_Type, Node, State) -> {Node, State}), State, Node) -> Node.
map_with_state(F, Init, Forms, Style) ->
    {NForms, _State} = mapfold(F, Init, Forms, Style),
    NForms.

reduce(F, Init, TopNode) ->
    reduce(F, Init, TopNode, all).

-spec reduce(fun((_Type, Node, State) -> State), State, Node, traverse_style()) -> State.
reduce(F, Init, TopNode, Style) ->
    NNF = with_style(
           fun(NF) ->
                   fun(Node, State) ->
                           {Node, NF(Node, State)}
                   end
           end, F, Style),
    {_NForms, NState} = 
        mapfold(NNF, Init, TopNode, Style),
    NState.

reduce_e(F, Init, TypeNode) ->
    reduce_e(F, Init, TypeNode, all).

reduce_e(F, Init, TopNode, Style) ->
    NNF = with_style(
           fun(NF) ->
                   fun(Node, State) ->
                           {Node, NF(Node, State)}
                   end
           end, F, Style),
    case mapfolde(NNF, Init, TopNode, Style) of
        {ok, {_NForms, NState}} ->
            {ok, NState};
        {error, Reason} ->
            {error, Reason}
    end.

-spec mapfold(traverse_state_fun(Node, State, {Node, State}), State, Node) -> {Node, State}.
mapfold(F, Init, Node) ->
    mapfold(F, Init, Node, all).

-spec mapfold(traverse_state_fun(Node, State, {Node, State}), State, Node, traverse_style()) -> {Node, State}.
mapfold(F, Init, Node, Style) ->
    Monad = astranaut_monad:new_state(),
    NF = transform_mapfoldm_f(F, Monad, Style),
    NodeM = map_m(NF, Node, Monad, Style),
    astranaut_monad:run_state(NodeM, Init).

mapfolde(F, Init, Node) ->
    mapfolde(F, Init, Node, all).

-spec mapfolde(traverse_state_fun(Node, State, {Node, State}), State, Node, traverse_style()) -> {Node, State}.
mapfolde(F, Init, Node, Style) ->
    Monad = astranaut_monad:new_state_error(),
    NF = transform_mapfoldm_f(F, Style, Monad),
    NodeM = map_m(NF, Node, Monad, Style),
    astranaut_monad:run_state_error(NodeM, Init).

-spec map_m(traverse_fun(Node, astranaut_monad:monadic(M, Node)), Node, M, traverse_style()) -> 
                   astranaut_monad:monadic(M, Node) when M :: astranaut_monad:monad().
map_m(F, Nodes, Monad, TraverseStyle) ->
    NF = transform_f(F, Monad, TraverseStyle),
    map_m(NF, Nodes, Monad).
    
-spec map_m(traverse_fun(Node, astranaut_monad:monadic(M, Node)), Node, M) -> 
                   astranaut_monad:monadic(M, Node) when M :: astranaut_monad:monad().
map_m(F, Nodes, Monad) when is_list(Nodes) ->
    astranaut_monad:map_m(
      fun(Subtree) ->
              map_m(F, Subtree, Monad)
      end, Nodes, Monad);
map_m(F, XNode, Monad) ->
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
      F(PreType, XNode),
      fun(YNode) ->
              case erl_syntax:subtrees(YNode) of
                  [] ->
                      astranaut_monad:return(YNode, Monad);
                  Subtrees ->
                      astranaut_monad:bind(
                        map_m(F, Subtrees, Monad),
                        fun(NSubTrees) ->
                                ZNode = erl_syntax:revert(erl_syntax:update_tree(YNode, NSubTrees)),
                                F(post, ZNode)
                        end, Monad)
              end
      end, Monad).

%%====================================================================
%% Internal functions
%%====================================================================
zip_style(F, all) when is_function(F, 3) ->
    fun({Type, Node}, State) ->
            F(Type, Node, State)
    end;
zip_style(F, all) when is_function(F, 2) ->
    fun({Type, Node}) ->
            F(Type, Node)
    end;
zip_style(F, _Style) ->
    F.

unzip_style(F, all) when is_function(F, 2) ->
    fun(Type, Node, State) ->
            F({Type, Node}, State)
    end;
unzip_style(F, all) when is_function(F, 1) ->
    fun(Type, Node) ->
            F({Type, Node})
    end;
unzip_style(F, _Style) ->
    F.

with_style(Fun, F, Style) ->
    NF = zip_style(F, Style),
    NNF = Fun(NF),
    unzip_style(NNF, Style).

transform_mapfoldm_f(F, Monad, Style) ->
    with_style(
      fun(NF) ->
              fun(Node) ->
                      astranaut_monad:state(
                        fun(State) ->
                                NF(Node, State)
                        end, Monad)
              end
      end, F, Style).

transform_f(F, Monad, pre) ->
    fun(pre, Node) -> F(Node);
       (leaf, Node) -> F(Node);
       (post, Node) -> astranaut_monad:return(Node, Monad)
    end;
transform_f(F, Monad, post) ->
    fun(pre, Node) -> astranaut_monad:return(Node, Monad);
       (leaf, Node) -> F(Node);
       (post, Node) -> F(Node)
    end;
transform_f(F, Monad, leaf) ->
    fun(pre, Node) -> astranaut_monad:return(Node, Monad);
       (leaf, Node) -> F(Node);
       (post, Node) -> astranaut_monad:return(Node, Monad)
    end;
transform_f(F, _Monad, all) ->
    F.
