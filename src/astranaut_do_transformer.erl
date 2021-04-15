%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2020, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  2 Jul 2020 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_do_transformer).

%% API
-export([parse_transform/2]).
-export([format_error/1]).

%%%===================================================================
%%% API
%%%===================================================================
parse_transform(Forms, _Opts) ->
    TransformOpts = #{formatter => ?MODULE, traverse => post, simplify_return => false},
    Return = astranaut_traverse:map(fun walk_node/2, Forms, TransformOpts),
    astranaut_return_m:to_compiler(Return).

format_error(Reason) ->
    astranaut_do:format_error(Reason).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
walk_node({call, _Line, {atom, __Line1, do},
           [{lc, _Line2, _Monad, _Comprehensions} = LCNode]}, #{}) ->
    astranaut_do:do(LCNode, #{monad => astranaut_monad, monad_fail => astranaut_monad});
walk_node(Node, _Attrs) ->
    Node.
