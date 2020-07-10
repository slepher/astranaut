%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2020, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  9 Jul 2020 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_error_ctx).

-export_type([astranaut_error_ctx/0]).

-opaque astranaut_error_ctx() :: #{'__struct__' => ?MODULE, file => string(), errors => [any()], warnings => [any()]}.

%% API
-export([new/0, new/1, new/2, new/3]).
-export([errors/1, warnings/1, file/1]).
-export([append/3, append_error/2, append_errors/2, append_warning/2, append_warnings/2, update_file/2]).

%%%===================================================================
%%% API
%%%===================================================================
new() ->
    new([], []).

new(File) ->
    new(File, [], []).

new(Errors, Warnings) ->
    new(undefined, Errors, Warnings).

new(File, Errors, Warnings) ->
    #{'__struct__' => ?MODULE, file => File, errors => Errors, warnings => Warnings}.

errors(#{'__struct__' := ?MODULE, errors := Errors}) ->
    lists:reverse(Errors).

warnings(#{'__struct__' := ?MODULE, warnings := Warnings}) ->
    lists:reverse(Warnings).

file(#{'__struct__' := ?MODULE, file := File}) ->
    File.

append(Errors, Warnings, Ctx) ->
    Ctx1 = append_errors(Errors, Ctx),
    Ctx2 = append_warnings(Warnings, Ctx1),
    Ctx2.
    
append_error(Error, Ctx) ->
    append_errors([Error], Ctx).

append_errors(Errors1, #{errors := Errors0} = Ctx) when is_list(Errors1) ->
    Errors2 = append(Errors0, Errors1),
    Ctx#{errors => Errors2}.

append_warning(Warning, Ctx) ->
    append_warnings([Warning], Ctx).

append_warnings(Warnings1, #{warnings := Warnings0} = Ctx) when is_list(Warnings1) ->
    Warnings2 = append(Warnings0, Warnings1),
    Ctx#{warnings => Warnings2}.

update_file(File, Ctx) ->
    Ctx#{file => File}.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
append(Errors0, Errors1) ->
    lists:foldl(
      fun(Error, Acc) ->
              [Error|Acc]
      end, Errors0, Errors1).
