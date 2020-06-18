%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2020, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 20 May 2020 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_erl_syntax).

-include("stacktrace.hrl").

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
    try erl_syntax:type(Node) of
        Type ->
            Subtrees = erl_syntax:subtrees(Node),
            up_subtrees(Subtrees, Opts#{parent => Type})
    catch
        EType:{badarg, _}?CAPTURE_STACKTRACE ->
            erlang:raise(EType, {invalid_node, Node, Opts}, ?GET_STACKTRACE);
        EType:Exception?CAPTURE_STACKTRACE ->
            erlang:raise(EType, Exception, ?GET_STACKTRACE)
    end.

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
    [{up_node, pattern, Names}, {up_node, expression, Clauses}];
up_subtrees([Patterns, Expressions], #{parent := Parent}) 
  when (Parent == match_expr) or (Parent == clause) ->
    [{up_node, pattern, Patterns}, {up_node, expression, Expressions}];
up_subtrees([Patterns, Guards, Expressions], #{parent := clause}) ->
    [{up_node, pattern, Patterns}, {up_node, guard, Guards}, {up_node, expression, Expressions}];
up_subtrees([Patterns, Expressions], #{parent := Parent}) 
  when (Parent == generator) or (Parent == binary_generator) ->
    [{up_node, pattern, Patterns}, {up_node, expression, Expressions}];
up_subtrees([[NameTree], BodyTrees], #{parent := attribute}) ->
    Name = attribute_name(NameTree),
    BodyTrees1 = update_attribute_body_trees(Name, BodyTrees),
    [{skip, [NameTree]}, BodyTrees1];
up_subtrees([NameTrees, Clauses], #{parent := function}) ->
    Names = lists:map(fun(NameTree) -> revert_root(NameTree) end, NameTrees),
    [Names, Clauses];
up_subtrees([ExprLeft, Op, ExprRight], #{parent := infix_expr}) ->
    [ExprLeft, {skip, Op}, ExprRight];
up_subtrees([Op, ExprRight], #{parent := prefix_expr}) ->
    [{skip, Op}, ExprRight];
up_subtrees(Subtrees, #{}) ->
    Subtrees.

update_attribute_body_trees(export, BodyTrees) ->
    {skip, BodyTrees};
update_attribute_body_trees(import, BodyTrees) ->
    {skip, BodyTrees};
update_attribute_body_trees(spec = Name, [SpecTree]) ->
    case erl_syntax:concrete(SpecTree) of
        {FunName, Types} ->
            T = fun(Types1) ->
                        [erl_syntax:abstract({FunName, Types1})]
                end,
            {up_attr, #{node => type, attribute => Name}, {transformer, Types, T}};
        _ ->
            [SpecTree]
    end;
update_attribute_body_trees(Name, [TypeTree]) when (Name == type); (Name == opaque) ->
    case erl_syntax:concrete(TypeTree) of
        {TypeName, Type, Variables} ->
            T = fun([Type2|Variables2]) ->
                        [erl_syntax:abstract({TypeName, Type2, Variables2})]
                end,
            Type1 = {up_attr, #{node => type, attribute => Name}, Type},
            Variables1 = lists:map(
                           fun(Variable) -> {up_attr, #{node => type_variable, attribute => Name}, Variable} 
                           end, Variables),
            {transformer, [Type1|Variables1], T};
        _ ->
            [TypeTree]
    end;
update_attribute_body_trees(_ = Name, BodyTrees) ->
    default_revert_body_trees(Name, BodyTrees).

attribute_name({tree, atom, _, Name}) ->
    Name.

default_revert_body_trees(Name, BodyTrees) ->
  Bodies = lists:map(fun(BodyTree) -> revert_root(BodyTree) end, BodyTrees),
  {up_attr, #{node => attribute, attribute => Name}, Bodies}.

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
