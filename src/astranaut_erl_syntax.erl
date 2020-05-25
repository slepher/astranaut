%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2020, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 20 May 2020 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_erl_syntax).

%% API
-export([type/1]).
-export([subtrees/2]).
-export([update_subtrees/2]).
-export([get_pos/1]).
-export([node_type/2]).
%%%===================================================================
%%% API
%%%===================================================================
type(Node) ->
    erl_syntax:type(Node).

get_pos(Node) ->
    erl_syntax:get_pos(Node).

subtrees(Node, Opts) ->
    Type = erl_syntax:type(Node),
    Subtrees = erl_syntax:subtrees(Node),
    up_subtrees(Subtrees, Opts#{parent => Type}).

update_subtrees(Node, Subtrees) ->
    revert_root(erl_syntax:update_tree(Node, Subtrees)).

node_type(_Node, #{node := NodeType}) ->
    NodeType;
node_type({attribute, _, _AttrName, _AttrValue}, #{}) ->
    form;
node_type({function, _, _AttrName, _AttrValue}, #{}) ->
    form;
node_type(_Node, #{}) ->
    expression.
    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
up_subtrees(Subtrees, #{node := pattern}) ->
    Subtrees;
up_subtrees([NameTrees, Clauses], #{parent := named_fun_expr}) ->
    Names = lists:map(fun(NameTree) -> erl_syntax:revert(NameTree) end, NameTrees),
    [{pattern, Names}, {expression, Clauses}];
up_subtrees([Patterns, Expressions], #{parent := Parent}) 
  when (Parent == match_expr) or (Parent == clause) ->
    [{pattern, Patterns}, {expression, Expressions}];
up_subtrees([Patterns, Guards, Expressions], #{parent := clause}) ->
    [{pattern, Patterns}, {guard, Guards}, {expression, Expressions}];
up_subtrees([Patterns, Expressions], #{parent := Parent}) 
  when (Parent == generator) or (Parent == binary_generator) ->
    [{pattern, Patterns}, {expression, Expressions}];
up_subtrees([[NameTree], BodyTrees], #{parent := attribute}) ->
    Name = attribute_name(NameTree),
    BodyTrees1 = 
        case Name of
            export ->
                {skip, BodyTrees};
            import ->
                %% do not traverse import attribute
                {skip, BodyTrees};
            _ ->
                Bodies = lists:map(fun(BodyTree) -> revert_root(BodyTree) end, BodyTrees),
                {#{node => attribute, attribute => Name}, Bodies}
        end,
    [[NameTree], BodyTrees1];
up_subtrees([NameTrees, Clauses], #{parent := function} ) ->
    Names = lists:map(fun(NameTree) -> revert_root(NameTree) end, NameTrees),
    [Names, Clauses];
up_subtrees([ExprLeft, Op, ExprRight], #{parent := infix_expr}) ->
    [ExprLeft, {skip, Op}, ExprRight];
up_subtrees([Op, ExprRight], #{parent := prefix_expr}) ->
    [{skip, Op}, ExprRight];
up_subtrees(Subtrees, #{}) ->
    Subtrees.

attribute_name({tree, atom, _, Name}) ->
    Name.

-ifdef(OTP_RELEASE).
  -if(?OTP_RELEASE >= 22).
revert_root(Node) ->
    erl_syntax_22:revert_root(Node).
  -else.
revert_root(Node) ->
    erl_syntax_21:revert_root(Node).
  -endif.
-else.
revert_root(Node) ->
    erl_syntax_20:revert_root(Node).
-endif.
