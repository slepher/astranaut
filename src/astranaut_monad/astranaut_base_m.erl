%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2020, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  1 Jul 2020 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_base_m).

-erlando_type(?MODULE).

-export_type([astranaut_base_m/0]).
-opaque astranaut_base_m() :: #{'__struct__' => ?MODULE, return => any(), errors => [any()], warnings => [any()]}.

-astranaut_future_behaviour(monad).

%% API
-export([astranaut_base_m/1, to_monad/1]).
-export([to_struct_base/2, up_struct_base/1]).
-export([new/0, run/1]).
-export([bind/2, then/2, return/1]).
-export(['>>='/3, return/2]).
-export([error/1, warning/1, errors/1, warnings/1]).

%%%===================================================================
%%% Construct astranaut_base_m by map
astranaut_base_m(#{} = Map) ->
    Map1 = up_struct_base(Map),
    default(Map1).
%%%===================================================================
%%% convert traverse return value to astranaut_base_m
%%%===================================================================
to_monad(A) ->
    to_monad(ok, A).

to_monad(A, Return) ->
    case to_struct_base(A, Return) of
        {ok, StructBase} ->
            Return = maps:get(return, StructBase, A),
            astranaut_base_m(StructBase#{return => Return});
        error ->
            to_monad_1(Return)
    end.

to_monad_1(#{'__struct__' := ?MODULE} = BaseM) ->
    BaseM;
to_monad_1(B) ->
    astranaut_base_m(#{return => B}).

to_struct_base(A, {warning, Warning}) ->
    {ok, #{return => A, warnings => [Warning]}};
to_struct_base(A, {warnings, Warnings}) ->
    {ok, #{return => A, warnings => Warnings}};
to_struct_base(_A, {warning, B, Warning}) ->
    {ok, #{return => B, warnings => [Warning]}};
to_struct_base(_A, {warnings, B, Warnings}) ->
    {ok, #{return => B, warnings => Warnings}};
to_struct_base(_A, {error, Error}) ->
    {ok, #{errors => [Error]}};
to_struct_base(_A, {errors, Errors}) when is_list(Errors) ->
    {ok, #{errors => Errors}};
to_struct_base(_A, {error, B, Error}) ->
    {ok, #{return => B, errors => [Error]}};
to_struct_base(_A, {errors, B, Errors}) when is_list(Errors) ->
    {ok, #{return => B, errors => Errors}};
to_struct_base(_A, {ok, B}) ->
    {ok, #{return => B}};
to_struct_base(_A, _Other) ->
    error.

up_struct_base(#{warning := Warning} = Map) ->
    Map1 = maps:remove(warning, Map),
    Warnings = maps:get(warnings, Map, []),
    Map1#{warnings => [Warning|Warnings]};
up_struct_base(#{error := Error} = Map) ->
    Map1 = maps:remove(error, Map),
    Errors = maps:get(warnings, Map, []),
    Map1#{errors => [Error|Errors]};
up_struct_base(#{errors := Errors}) when not is_list(Errors) ->
    exit({errors_should_be_list, Errors});
up_struct_base(#{warnings := Warnings}) when not is_list(Warnings) ->
    exit({warnings_should_be_list, Warnings});
up_struct_base(#{} = Map) ->
    case maps:get(errors, Map, []) of
        [] ->
            case maps:is_key(return, Map) of
                false ->
                    exit({no_return_without_errors, Map});
                true ->
                    Map
            end;
        _ ->
            Map
    end.

%%%===================================================================
%%% fill struct default values
%%%===================================================================
default(#{} = Map) ->
    maps:merge(#{'__struct__' => ?MODULE, return => ok, errors => [], warnings => []}, Map).

run(#{warnings := Warnings, errors := Errors, return := A}) ->
    {A, Errors, Warnings}.

new() ->
    ?MODULE.
%%%===================================================================
%%% API
%%%===================================================================
bind(MA, KMB) ->
    #{'__struct__' := ?MODULE, return := A, errors := Errors0, warnings := Warnings0} = to_monad(MA),
    #{'__struct__' := ?MODULE, return := B, errors := Errors1, warnings := Warnings1} = to_monad(A, KMB(A)),
    default(#{return => B, errors => Errors0 ++ Errors1, warnings => Warnings0 ++ Warnings1}).

then(MA, MB) ->
    bind(MA, fun(_) -> MB end).

return(A) ->
    default(#{return => A}).

'>>='(MA, KMB, ?MODULE) ->
    bind(MA, KMB).

return(A, ?MODULE) ->
    return(A).

error(Error) ->
    errors([Error]).

warning(Warning) ->
    warnings([Warning]).
    
errors(Errors) when is_list(Errors) ->
    default(#{errors => Errors}).

warnings(Warnings) when is_list(Warnings) ->
    default(#{warnings => Warnings}).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
