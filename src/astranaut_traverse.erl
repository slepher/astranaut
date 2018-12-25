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
-export([reduce/3, reduce/4]).
-export([mapfold/3, mapfold/4]).
-export([map_m/4, map_m/5]).

-type traverse_opts() :: #{traverse := traverse_style()}.
-type traverse_style() :: pre | post | all.
-type traverse_fun(Node, Return) :: fun((_Type, Node) -> Return) | fun((Node) -> Return).
-type traverse_state_fun(Node, State, Return) :: fun((_Type, State, Node) -> Return) | fun((Node, State) -> Return).

%%%===================================================================
%%% API
%%%===================================================================
map(F, TopNode) ->
    map(F, TopNode, #{final => true}).

reduce(F, Init, TopNode) ->
    reduce(F, Init, TopNode, #{final => true}).

map_with_state(F, Init, Form) ->
    map_with_state(F, Init, Form, #{final => true}).

-spec mapfold(traverse_state_fun(Node, State, {Node, State}), State, Node) -> {Node, State}.
mapfold(F, Init, Node) ->
    mapfold(F, Init, Node, #{final => true}).

-spec map(fun((_Type, Node) -> Node), Node) -> Node.
map(F, TopNode, Opts) ->
    NF = fun(Node, State, Attr) ->
                 case F(Node, Attr) of
                     Map when is_map(Map) ->
                         Map;
                     {error, Reason} ->
                         {error, Reason};
                     NNode ->
                         {NNode, State}
                 end
         end,
    map_with_state(NF, ok, TopNode, Opts).

-spec map_with_state(fun((_Type, Node, State) -> {Node, State}), State, Node) -> Node.
map_with_state(F, Init, Node, Opts) ->
    Reply = 
        case mapfold(F, Init, Node, Opts#{final => false}) of
            {error, Errors, Warnings} ->
                {error, Errors, Warnings};
            {ok, {NNode, _State}, Errors, Warnings} ->
                {ok, NNode, Errors, Warnings}
        end,
    transform_final_reply(Reply, Opts).

-spec reduce(fun((_Type, Node, State) -> State), State, Node, traverse_opts()) -> State.
reduce(F, Init, TopNode, Opts) ->
    NF = 
        fun(Node, State, Attr) ->
                case F(Node, State, Attr) of
                    Map when is_map(Map) ->
                        Map;
                    {error, Reason} ->
                        {error, Reason};
                    NState ->
                        {Node, NState}
                end
        end,
    NReply = 
        case mapfold(NF, Init, TopNode, Opts#{final => false}) of
            {error, Errors, Warnings} ->
                {error, Errors, Warnings};
            {ok, {_NNode, State}, Errors, Warnings} ->
                {ok, State, Errors, Warnings}
        end,
    transform_final_reply(NReply, Opts).

-spec mapfold(traverse_state_fun(Node, State, {Node, State}), State, Node, traverse_opts()) -> {Node, State}.
mapfold(F, Init, Node, Opts) ->
    Monad = astranaut_traverse_monad:new(),
    NF = transform_mapfold_f(F),
    NodeM = map_m(NF, Node, Monad, Opts),
    transform_final_reply(astranaut_traverse_monad:run(NodeM, Init), Opts).

transform_final_reply(Reply, Opts) when is_map(Opts) ->
    IsFinal = maps:get(final, Opts, true),
    transform_final_reply(Reply, IsFinal);

transform_final_reply({ok, Reply, [], []}, true) ->
    Reply;
transform_final_reply({ok, Reply, [], Warnings}, true) ->
    {warning, Reply, Warnings};
transform_final_reply({ok, _Reply, Errors, Warnings}, true) ->
    {error, Errors, Warnings};
transform_final_reply(Reply, _IsFinal) ->
    Reply.

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
      fun(continue) ->
              astranaut_monad:return(XNode, Monad);
         ({continue, YNode}) ->
              astranaut_monad:return(YNode, Monad);
         (YNode) ->
              case erl_syntax:subtrees(YNode) of
                  [] ->
                      astranaut_monad:return(YNode, Monad);
                  Subtrees ->
                      NodeType = erl_syntax:type(YNode),
                      astranaut_monad:bind(
                        map_m_subtrees(F, Subtrees, Monad, NodeType, Attrs),
                        fun(NSubTrees) ->
                                ZNode = erl_syntax:revert(erl_syntax:update_tree(YNode, NSubTrees)),
                                F(ZNode, Attrs#{step => post})
                        end, Monad)
              end
      end, Monad).

map_m_subtrees(F, Nodes, Monad, _NodeType, #{node := pattern} = Attrs) ->
    map_m_1(F, Nodes, Monad, Attrs);
map_m_subtrees(F, [Pattern|Rest], Monad, NodeType, Attrs) 
  when (NodeType == match_expr) or (NodeType == clause) ->
    %% if node type is match_expr or clause 
    %% make first subtree pattern, make rest expression
    astranaut_monad:bind(
      map_m_1(F, Pattern, Monad, Attrs#{node => pattern}),
      fun(NHead) ->
              astranaut_monad:bind(
                map_m_1(F, Rest, Monad, Attrs#{node => expression}),
                fun(NRest) ->
                        astranaut_monad:return([NHead|NRest], Monad)
                end, Monad)
      end, Monad);
map_m_subtrees(F, Nodes, Monad, _NodeType, Attrs) ->
    map_m_1(F, Nodes, Monad, Attrs).

%%====================================================================
%% Internal functions
%%====================================================================
transform_mapfold_f(F) ->
    fun(Node, Attr) ->
            astranaut_traverse_monad:bind(
              astranaut_traverse_monad:get(),
              fun(State) ->
                      Reply = F(Node, State, Attr),
                      reply_to_monad(Reply, astranaut_traverse_monad:return(Node))
              end)
    end.

%% transform user sytle traverse return to astranaut_traverse_monad
reply_to_monad(#{error := Error} = Reply, MA) ->
    MB = astranaut_traverse_monad:then(MA, astranaut_traverse_monad:error(Error)),
    NReply = maps:remove(error, Reply),
    reply_to_monad(NReply, MB);
reply_to_monad(#{errors := Errors} = Reply, MA) ->
    MB = astranaut_traverse_monad:then(MA, astranaut_traverse_monad:errors(Errors)),
    NReply = maps:remove(errors, Reply),
    reply_to_monad(NReply, MB);
reply_to_monad(#{warning := Warning} = Reply, MA) ->
    MB = astranaut_traverse_monad:then(MA, astranaut_traverse_monad:warning(Warning)),
    NReply = maps:remove(warning, Reply),
    reply_to_monad(NReply, MB);
reply_to_monad(#{warnings := Warnings} = Reply, MA) ->
    MB = astranaut_traverse_monad:then(MA, astranaut_traverse_monad:warnings(Warnings)),
    NReply = maps:remove(warnings, Reply),
    reply_to_monad(NReply, MB);
reply_to_monad(#{state := State} = Reply, MA) ->
    MB = astranaut_traverse_monad:then(MA, astranaut_traverse_monad:put(State)),
    NReply = maps:remove(state, Reply),
    reply_to_monad(NReply, MB);
reply_to_monad(#{continue := true, node := Node} = Reply, MA) ->
    MB = astranaut_traverse_monad:then(MA, astranaut_traverse_monad:return({continue, Node})),
    NReply = maps:remove(continue, maps:remove(node, Reply)),
    reply_to_monad(NReply, MB);
reply_to_monad(#{node := Node} = Reply, MA) ->
    MB = astranaut_traverse_monad:then(MA, astranaut_traverse_monad:return(Node)),
    NReply = maps:remove(node, Reply),
    reply_to_monad(NReply, MB);
reply_to_monad(continue, MA) ->
    astranaut_traverse_monad:then(MA, astranaut_traverse_monad:return(continue));
reply_to_monad({error, Reason}, MA) ->
    astranaut_traverse_monad:then(MA, astranaut_traverse_monad:fail(Reason));
reply_to_monad({Node, State}, MA) ->
    astranaut_traverse_monad:then(MA, astranaut_traverse_monad:state(fun(_) -> {Node, State} end)).

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
