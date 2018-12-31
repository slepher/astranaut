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
-export([map_traverse_return/2, format_error/1]).

-type traverse_node() :: tuple().
-type traverse_error() :: term().
-type traverse_state() :: term().
-type traverse_opts() :: #{traverse := traverse_style(), final := boolean()}.
-type traverse_style() :: pre | post | leaf | all.
-type traverse_attr() :: #{step := pre | post | lef, node := form | pattern | expression}.
-type traverse_fun_return() :: #{node := traverse_node(), state := traverse_state(), 
                                 continue := boolean(), 
                                 error := traverse_error(), warning := traverse_error(),
                                 errors := [traverse_error()], warings := [traverse_error()]} |
                               {error, traverse_error()} | continue |
                               {traverse_node(), traverse_state()}| traverse_node().

-type traverse_return(ReturnType) :: ReturnType |  {warning, ReturnType, [traverse_return_error()]} |
                                     {error, [traverse_return_error()], [traverse_return_error()]}.

-type traverse_fun() :: fun((traverse_node(), traverse_attr()) -> traverse_fun_return()).
-type traverse_state_fun() :: fun((traverse_node(), traverse_state(), traverse_attr()) -> traverse_fun_return()).
-type parse_transform_module() :: module().
-type compile_file() :: string().
-type line() :: integer().
-type traverse_return_error() :: [{compile_file(), [{line(), parse_transform_module(), traverse_error()}]}].

%%%===================================================================
%%% API
%%%===================================================================
-spec map(traverse_fun(), Node) -> traverse_return(Node).
map(F, TopNode) ->
    map(F, TopNode, #{}).

-spec reduce(traverse_state_fun(), State, _Node) -> traverse_return(State).
reduce(F, Init, TopNode) ->
    reduce(F, Init, TopNode, #{}).

-spec map_with_state(traverse_state_fun(), State, Node) -> traverse_return({Node, State}).
map_with_state(F, Init, Form) ->
    map_with_state(F, Init, Form, #{}).

-spec mapfold(traverse_state_fun(), State, Node) -> traverse_return({Node, State}).
mapfold(F, Init, Node) ->
    mapfold(F, Init, Node, #{}).

-spec map(traverse_fun(), Node, traverse_opts()) -> traverse_return(Node).
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

-spec map_with_state(traverse_state_fun(), State, Node, traverse_opts()) -> traverse_return({Node, State}).
map_with_state(F, Init, Node, Opts) ->
    Reply = mapfold(F, Init, Node, Opts),
    NReply = map_traverse_return(
               fun({NNode, _State}) ->
                       NNode
               end, Reply),
    parse_transform_return(NReply, Node, Opts).

mapfold(F, Init, Node, Opts) ->
    SimplifyReturn = maps:get(simplify_return, Opts, true),
    Return = mapfold_1(F, Init, Node, Opts),
    simplify_return(Return, SimplifyReturn).

map_traverse_return(F, {ok, Reply, Errors, Warnings}) ->
    {ok, F(Reply), Errors, Warnings};
map_traverse_return(_F, {error, Errors, Warnings}) ->
    {error, Errors, Warnings};
map_traverse_return(F, {warning, Reply, Warnings}) ->
    {warning, F(Reply), Warnings};
map_traverse_return(F, Reply) ->
    F(Reply).

-spec mapfold(traverse_state_fun(), State, Node, traverse_opts()) -> traverse_return({Node, State}).
mapfold_1(F, Init, Node, Opts) ->
    NOpts = maps:merge(#{module => ?MODULE, traverse => all}, Opts),
    Monad = astranaut_traverse_monad:new(),
    NF = transform_mapfold_f(F, NOpts),
    NodeM = map_m(NF, Node, Monad, NOpts),
    astranaut_traverse_monad:run(NodeM, Init).

format_error(Message) ->
    case io_lib:deep_char_list(Message) of
        true -> Message;
        _    -> io_lib:write(Message)
    end.

-spec map_m(traverse_fun(), Node, M, traverse_opts()) -> astranaut_monad:monadic(M, Node) when M :: astranaut_monad:monad().
map_m(F, Nodes, Monad, Opts) ->
    map_m(F, Nodes, Monad, Opts, #{node => form}).

map_m(F, Nodes, Monad, Opts, Attrs) ->
    TraverseStyle = maps:get(traverse, Opts),
    NF = transform_f(F, Monad, TraverseStyle),
    map_m_1(NF, Nodes, Monad, Attrs).

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
    %%         
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
    MB = astranaut_traverse_monad:then(astranaut_traverse_monad:error(NError), MA),
    NReply = maps:remove(error, Reply),
    reply_to_monad(NReply, MB, Opts);
reply_to_monad(#{errors := Errors} = Reply, MA, Opts) ->
    NErrors = format_errors(Errors, Opts),
    MB = astranaut_traverse_monad:then(astranaut_traverse_monad:errors(NErrors), MA),
    NReply = maps:remove(errors, Reply),
    reply_to_monad(NReply, MB, Opts);
reply_to_monad(#{warning := Warning} = Reply, MA, Opts) ->
    NWarning = format_error(Warning, Opts),
    MB = astranaut_traverse_monad:then(astranaut_traverse_monad:warning(NWarning), MA),
    NReply = maps:remove(warning, Reply),
    reply_to_monad(NReply, MB, Opts);
reply_to_monad(#{warnings := Warnings} = Reply, MA, Opts) ->
    NWarnings = format_errors(Warnings, Opts),
    MB = astranaut_traverse_monad:then(astranaut_traverse_monad:warnings(NWarnings), MA),
    NReply = maps:remove(warnings, Reply),
    reply_to_monad(NReply, MB, Opts);
reply_to_monad(#{state := State} = Reply, MA, Opts) ->
    MB = astranaut_traverse_monad:then(astranaut_traverse_monad:put(State), MA),
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

map_walk_return(F, WalkReturn) ->
    case WalkReturn of
        Map when is_map(Map) ->
            Map;
        {error, Reason} ->
            {error, Reason};
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
