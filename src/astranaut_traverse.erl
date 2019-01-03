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
-export([map_m/3]).
-export([map_traverse_return/2, format_error/1]).

-type traverse_node() :: tuple().
-type traverse_error() :: term().
-type traverse_state() :: term().
-type traverse_opts() :: #{traverse => traverse_style(), parse_transform => boolean(),
                           node => node_type(), formatter => module() }.
-type traverse_map_m_opts() :: #{traverse => traverse_style(), parse_transform => boolean(),
                                 node => node_type(), formatter => module(),
                                 monad_class := module(), monad := term()
                                }.
-type node_type() :: module | file | export | import | type | spec | pattern | expression | guard | form | atom().
-type traverse_style() :: traverse_step() | all.
-type traverse_step() :: pre | post | leaf.
-type traverse_attr() :: #{step := traverse_step(), node := node_type()}.
-type traverse_fun_return() :: #{node => traverse_node(), state => traverse_state(),
                                 continue => boolean(),
                                 error => traverse_error(), warning => traverse_error(),
                                 errors => [traverse_error()], warings => [traverse_error()]} |
                               {error, traverse_error()} | continue |
                               {traverse_node(), traverse_state()}| traverse_node().

-type traverse_return(ReturnType) :: ReturnType | 
                                     {ok, ReturnType, traverse_return_error(), traverse_return_error()} |
                                     {error, traverse_return_error(), traverse_return_error()}.

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
    map_traverse_return(
      fun({_NNode, State}) ->
              State
      end, mapfold(NF, Init, TopNode, Opts)).

-spec map_with_state(traverse_state_fun(), State, Node, traverse_opts()) -> traverse_final_return({Node, State}).
map_with_state(F, Init, Node, Opts) ->
    Reply = mapfold(F, Init, Node, Opts),
    NReply = map_traverse_return(
               fun({NNode, _State}) ->
                       NNode
               end, Reply),
    parse_transform_return(NReply, Node, Opts).

-spec mapfold(traverse_state_fun(), State, Node, traverse_opts()) -> traverse_return({Node, State}).
mapfold(F, Init, Node, Opts) ->
    SimplifyReturn = maps:get(simplify_return, Opts, true),
    Return = mapfold_1(F, Init, Node, Opts),
    simplify_return(Return, SimplifyReturn).

-spec map_traverse_return(fun((A) -> B), traverse_return(A)) -> parse_transform_return(B).
map_traverse_return(F, {ok, Reply, Errors, Warnings}) ->
    {ok, F(Reply), Errors, Warnings};
map_traverse_return(_F, {error, Errors, Warnings}) ->
    {error, Errors, Warnings};
map_traverse_return(F, {warning, Reply, Warnings}) ->
    {warning, F(Reply), Warnings};
map_traverse_return(F, Reply) ->
    F(Reply).

-spec map_m(traverse_fun(), Node, traverse_map_m_opts()) -> astranaut_monad:monadic(M, Node) when M :: astranaut_monad:monad().
map_m(F, Nodes, #{monad_class := _MonadClass, monad := _Monad} = Opts) ->
    NOpts = maps:merge(#{formatter => ?MODULE, traverse => all, node => form}, Opts),
    NF = transform_f(F, NOpts),
    map_m_1(NF, Nodes, NOpts).

format_error(Message) ->
    case io_lib:deep_char_list(Message) of
        true -> Message;
        _    -> io_lib:write(Message)
    end.
%%====================================================================
%% Internal functions
%%====================================================================
mapfold_1(F, Init, Node, Opts) ->
    Monad = astranaut_traverse_monad:new(),
    NOpts = Opts#{monad => Monad, monad_class => astranaut_monad},
    NF = transform_mapfold_f(F, NOpts),
    NodeM = map_m(NF, Node, NOpts),
    astranaut_traverse_monad:run(NodeM, Init).

map_m_1(F, Nodes, Opts) when is_list(Nodes) ->
    monad_map_m(
      fun(Subtree) ->
              map_m_1(F, Subtree, Opts)
      end, Nodes, Opts);
map_m_1(F, NodeA, #{node := NodeType} = Opts) ->
    PreType = 
        case erl_syntax:subtrees(NodeA) of
            [] ->
                leaf;
            _Subtrees ->
                pre
        end,
    %% do form
    %% do([Monad ||
    %%           NodeB <- F(NodeA, Attrs),
    %%           SubTreesB = erl_syntax:subtrees(NodeB),
    %%           SubtreesC <- map_m(F, SubTreesB, Opts),
    %%           NodeC = erl_syntax:revert(erl_syntax:update_tree(NodeB, SubtreesC)),
    %%           F(NodeD, Attrs)
    %%    ]).
    bind_with_continue(
      NodeA, 
      F(NodeA, #{step => PreType, node => NodeType}),
      fun(NodeB) ->
              case erl_syntax:subtrees(NodeB) of
                  [] ->
                      monad_return(NodeB, Opts);
                  Subtrees ->
                      SyntaxType = erl_syntax:type(NodeB),
                      monad_bind(
                        map_m_subtrees(F, Subtrees, SyntaxType, Opts),
                        fun(NSubTrees) ->
                                NodeC = erl_syntax:revert(erl_syntax:update_tree(NodeB, NSubTrees)),
                                bind_with_continue(
                                  NodeC, 
                                  F(NodeC, #{step => post, node => NodeType}),
                                  fun(NodeD) ->
                                          monad_return(NodeD, Opts)
                                  end, Opts)
                        end, Opts)
              end
      end, Opts).

bind_with_continue(NodeA, MNodeB, BMC, Opts) ->
    monad_bind(
      MNodeB,
      fun(continue) ->
              monad_return(NodeA, Opts);
         ({continue, NodeB}) ->
              monad_return(NodeB, Opts);
         (NodeB) ->
              BMC(NodeB)
      end, Opts).

map_m_subtrees(F, Nodes, _NodeType, #{node := pattern} = Opts) ->
    map_m_1(F, Nodes, Opts);
map_m_subtrees(F, [Patterns, Expressions], NodeType, Opts) 
  when (NodeType == match_expr) or (NodeType == clause) ->
    %% if node type is match_expr or clause 
    %% make first subtree pattern, make second subtree expression
    monad_bind(
      map_m_1(F, Patterns, Opts#{node => pattern}),
      fun(NPatterns) ->
              monad_bind(
                map_m_1(F, Expressions, Opts#{node => expression}),
                fun(NExpressions) ->
                        monad_return([NPatterns, NExpressions], Opts)
                end, Opts)
      end, Opts);
map_m_subtrees(F, [Patterns, Guards, Expressions], clause, Opts) ->
    %% if node type is clause contains guards 
    %% make first subtree pattern, make second subtree guard, make third subtree expression
    monad_bind(
      map_m_1(F, Patterns, Opts#{node => pattern}),
      fun(NPatterns) ->
              monad_bind(
                map_m_1(F, Guards, Opts#{node => guard}),
                fun(NGuards) ->
                        monad_bind(
                          map_m_1(F, Expressions, Opts#{node => expression}),
                          fun(NExpressions) ->
                                  monad_return([NPatterns, NGuards, NExpressions], Opts)
                          end, Opts)
                end, Opts)
      end, Opts);
map_m_subtrees(F, [[NameTree], BodyTrees], attribute, Opts) ->
    Name = attribute_name(NameTree),
    case Name of
        export ->
            % do not traverse export attribute
            monad_return([[NameTree], BodyTrees], Opts);
        import ->
            % do not traverse import attribute
            monad_return([[NameTree], BodyTrees], Opts);
        _ ->
            Bodies = lists:map(fun(BodyTree) -> erl_syntax:revert(BodyTree) end, BodyTrees),
            monad_bind(
              map_m_1(F, Bodies, Opts#{node => Name}),
              fun(NBodies) ->
                      monad_return([[NameTree], NBodies], Opts)
              end, Opts)
    end;
map_m_subtrees(F, Nodes, _NodeType, Opts) ->
    map_m_1(F, Nodes, Opts).

attribute_name({tree, atom, _, Name}) ->
    Name.

monad_bind(A, AFB, #{monad_class := MonadClass, monad := Monad}) ->
    MonadClass:bind(A, AFB, Monad).
monad_return(A, #{monad_class := MonadClass, monad := Monad}) ->
    MonadClass:return(A, Monad).
monad_map_m(F, MAs, #{monad_class := MonadClass, monad := Monad}) ->
    MonadClass:map_m(F, MAs, Monad).

transform_mapfold_f(F, Opts) ->
    fun(Node, Attr) ->
            astranaut_traverse_monad:bind(
              astranaut_traverse_monad:get(),
              fun(State) ->
                      Reply = F(Node, State, Attr),
                      Line = erl_syntax:get_pos(Node),
                      reply_to_monad(Reply, astranaut_traverse_monad:return(Node), Opts#{line => Line})
              end)
    end.

%% transform user sytle traverse return to astranaut_traverse_monad
reply_to_monad(#{error := Error} = Reply, MA, Opts) ->
    NError = format_error(Error, Opts),
    MB = astranaut_traverse_monad:left_then(MA, astranaut_traverse_monad:error(NError)),
    NReply = maps:remove(error, Reply),
    reply_to_monad(NReply, MB, Opts);
reply_to_monad(#{errors := Errors} = Reply, MA, Opts) ->
    NErrors = format_errors(Errors, Opts),
    MB = astranaut_traverse_monad:left_then(MA, astranaut_traverse_monad:errors(NErrors)),
    NReply = maps:remove(errors, Reply),
    reply_to_monad(NReply, MB, Opts);
reply_to_monad(#{warning := Warning} = Reply, MA, Opts) ->
    NWarning = format_error(Warning, Opts),
    MB = astranaut_traverse_monad:left_then(MA, astranaut_traverse_monad:warning(NWarning)),
    NReply = maps:remove(warning, Reply),
    reply_to_monad(NReply, MB, Opts);
reply_to_monad(#{warnings := Warnings} = Reply, MA, Opts) ->
    NWarnings = format_errors(Warnings, Opts),
    MB = astranaut_traverse_monad:left_then(MA, astranaut_traverse_monad:warnings(NWarnings)),
    NReply = maps:remove(warnings, Reply),
    reply_to_monad(NReply, MB, Opts);
reply_to_monad(#{state := State} = Reply, MA, Opts) ->
    MB = astranaut_traverse_monad:left_then(MA, astranaut_traverse_monad:put(State)),
    NReply = maps:remove(state, Reply),
    reply_to_monad(NReply, MB, Opts);
reply_to_monad(#{continue := true, node := Node} = Reply, MA, Opts) ->
    MB = astranaut_traverse_monad:then(MA, astranaut_traverse_monad:return({continue, Node})),
    NReply = maps:remove(continue, maps:remove(node, Reply)),
    reply_to_monad(NReply, MB, Opts);
reply_to_monad(#{node := Node} = Reply, MA, Opts) ->
    MB = astranaut_traverse_monad:then(MA, astranaut_traverse_monad:return(Node)),
    NReply = maps:remove(node, Reply),
    reply_to_monad(NReply, MB, Opts);
reply_to_monad(#{}, MA, _Opts) ->
    MA;
reply_to_monad(continue, MA, _Opts) ->
    astranaut_traverse_monad:then(MA, astranaut_traverse_monad:return(continue));
reply_to_monad({error, Reason}, MA, Opts) ->
    NReason = format_error(Reason, Opts),
    astranaut_traverse_monad:then(MA, astranaut_traverse_monad:fail(NReason));
reply_to_monad({Node, State}, MA, _Opts) ->
    astranaut_traverse_monad:then(MA, astranaut_traverse_monad:state(fun(_) -> {Node, State} end)).

format_errors(Errors, Opts) ->
    lists:map(fun(Error) -> format_error(Error, Opts) end, Errors).

format_error({Line1, Error}, #{formatter := Module}) when is_integer(Line1) ->
    {Line1, Module, Error};
format_error({Line1, Module1, Error}, #{}) when is_integer(Line1) ->
    {Line1, Module1, Error}; 
format_error(Error, #{line := Line, formatter := Module}) ->
    {Line, Module, Error}.

transform_f(F, #{traverse := pre} = Opts) ->
    fun(Node, #{step := pre  } = Attr) -> F(Node, Attr);
       (Node, #{step := leaf } = Attr) -> F(Node, Attr);
       (Node, #{step := post }) -> monad_return(Node, Opts)
    end;
transform_f(F, #{traverse := post} = Opts) ->
    fun(Node, #{step := pre  }) -> monad_return(Node, Opts);
       (Node, #{step := leaf } = Attr) -> F(Node, Attr);
       (Node, #{step := post } = Attr) -> F(Node, Attr)
    end;
transform_f(F, #{traverse := leaf} = Opts) ->
    fun(Node, #{step := pre  }) -> monad_return(Node, Opts);
       (Node, #{step := leaf } = Attr) -> F(Node, Attr);
       (Node, #{step := post }) -> monad_return(Node, Opts)
    end;
transform_f(F, _) ->
    F.

map_walk_return(F, WalkReturn) ->
    case WalkReturn of
        Map when is_map(Map) ->
            Map;
        {error, Reason} ->
            {error, Reason};
        {ok, WalkReturn} ->
            {ok, F(WalkReturn)};
        WalkReturn ->
            F(WalkReturn)
    end.

simplify_return({ok, Reply, [], []}, true) ->
    Reply;
simplify_return(Other, _) ->
    Other.

parse_transform_return(Reply, Forms, Opts) when is_map(Opts) ->
    ParseTransForm = maps:get(parse_transform, Opts, false),
    parse_transform_return(Reply, Forms, ParseTransForm);
parse_transform_return({ok, Reply, [], []}, _Forms, true) ->
    Reply;
parse_transform_return({ok, Reply, [], Warnings}, Forms, true) ->
    File = file(Forms),
    {warning, Reply, [{File, Warnings}]};
parse_transform_return({ok, _Reply, Errors, Warnings}, Forms, true) ->
    File = file(Forms),
    {error, [{File, Errors}], [{File, Warnings}]};
parse_transform_return({error, Errors, Warnings}, Forms, true) ->
    File = file(Forms),
    {error, [{File, Errors}], [{File, Warnings}]};
parse_transform_return(Reply, _Forms, _) ->
    Reply.

file(Forms) when is_list(Forms) ->
    case astranaut:attributes(file, Forms) of
        [{File, _}|_] ->
            File;
        _ ->
            ""
    end;
file(_) ->
    "".
