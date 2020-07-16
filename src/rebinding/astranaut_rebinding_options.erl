%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2019, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 25 Sep 2019 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_rebinding_options).

-include_lib("astranaut/include/astranaut_do.hrl").

%% API
-export([rebinding_options/1]).
-export([match_rebinding/3]).
-export([keys/0]).
-export([format_error/1]).

-record(rebinding_options, {all_options, fun_options}).
%%%===================================================================
%%% API
%%%===================================================================
rebinding_options(Forms) ->
    do([astranaut_return_m ||
           AllOptions <- rebinding_all_options(Forms),
           FunOptions <- rebinding_fun_options(Forms),
           return(#rebinding_options{fun_options = FunOptions, all_options = AllOptions})
       ]).

match_rebinding(Name, Arity, RebindingOptionsRec) ->
    case find_rebinding_options(Name, Arity, RebindingOptionsRec) of
        {ok, RebindingOptions} ->
            case maps:get(non_rebinding, RebindingOptions, false) of
                true ->
                    error;
                false ->
                    {ok, RebindingOptions}
            end;
        error ->
            error
    end.

keys() ->
    [clause_pinned, strict].

format_error({invalid_rebinding_fun_options, Value}) ->
    io_lib:format("invalid rebinding function options ~p", [Value]);
format_error({invalid_rebinding_fun, Value}) ->
    io_lib:format("invalid rebinding function ~p", [Value]);
format_error(Other) ->
    astranaut_traverse:format_error(Other).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
rebinding_fun_options(Forms) ->
    astranaut_options:with_attribute(
      fun(Attr, Acc) ->
              io:format("~p get attr ~p~n", [?MODULE, Attr]),
              add_fun_options(Attr, Acc)
      end, #{}, Forms, rebinding_fun, #{formatter => ?MODULE, simplify_return => false, deep_attr => true}).

rebinding_all_options(Forms) ->
    astranaut_options:with_attribute(
      fun(Attr, Acc) ->
              add_all_options(Attr, Acc)
      end, #{}, Forms, rebinding_all, #{formatter => ?MODULE, simplify_return => false}).

add_all_options(Options, Acc) ->
    do([astranaut_base_m ||
           Options1 <- validate_options(Options),
           astranaut_base_m:return(maps:merge(Acc, Options1))
       ]).

add_fun_options({FName, Arity}, Acc) when is_atom(FName), is_integer(Arity) ->
    add_fun_options({FName, Arity}, #{}, Acc);
add_fun_options({Function, Options}, Acc) ->
    io:format("get functions ~p ~p~n", [Function, Options]),
    do([astranaut_base_m ||
           Options1 <- validate_options(Options),
           io:format("get functions options ~p ~p~n", [Function, Options1]),

           add_fun_options(Function, Options1, Acc)
       ]);
add_fun_options(Function, Acc) ->
    add_fun_options(Function, #{}, Acc).

add_fun_options(Functions, Options, Acc) when is_list(Functions) ->
    io:format("~p get funs ~p~n", [?MODULE, Functions]),
    astranaut_monad:foldl_m(
      fun(Function, Acc1) ->
              io:format("~p get fun ~p~n", [?MODULE, Function]),
              add_fun_options(Function, Options, Acc1)
      end, Acc, Functions, astranaut_base_m);
add_fun_options({FName, Arity}, Options, Acc) when is_atom(FName), is_integer(Arity) ->
    merge_fun_options({FName, Arity}, Options, Acc);
add_fun_options(FName, Options, Acc) when is_atom(FName) ->
    merge_fun_options(FName, Options, Acc);
add_fun_options(Other, _Options, Acc) ->
    do([astranaut_base_m ||
           astranaut_base_m:warning({invalid_rebinding_fun, Other}),
           astranaut_base_m:return(Acc)
       ]).

merge_fun_options(Function, Options, Acc) ->
    io:format("merge fun options ~p ~p~n", [Function, Options]),
    FAcc = maps:get(Function, Acc, #{}),
    astranaut_base_m:return(maps:put(Function, maps:merge(FAcc, Options), Acc)).

validate_options(Options) ->
    Validator = #{clause_pinned => boolean, strict => boolean, debug => boolean, non_rebinding => boolean},
    astranaut_options:validate(Validator, Options).

find_rebinding_options(Name, Arity, #rebinding_options{fun_options = FunOptions, all_options = AllOptions}) ->
    case maps:find(Name, FunOptions) of
        {ok, Options} ->
            {ok, Options};
        error ->
            case maps:find({Name, Arity}, FunOptions) of
                {ok, Options} ->
                    {ok, Options};
                error ->
                    {ok, AllOptions}
            end
    end.
