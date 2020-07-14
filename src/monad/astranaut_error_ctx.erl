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
-export([errors/1, warnings/1, file/1, set_empty/1]).
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
    EndoErrors = astranaut_endo:endo(Errors),
    EndoWarnings = astranaut_endo:endo(Warnings),
    #{'__struct__' => ?MODULE, file => File, errors => EndoErrors, warnings => EndoWarnings}.

errors(#{'__struct__' := ?MODULE, errors := Errors}) ->
    run_endo(Errors).

warnings(#{'__struct__' := ?MODULE, warnings := Warnings}) ->
    run_endo(Warnings).

file(#{'__struct__' := ?MODULE, file := File}) ->
    File.

append(Errors, Warnings, Ctx) ->
    Ctx1 = append_errors(Errors, Ctx),
    Ctx2 = append_warnings(Warnings, Ctx1),
    Ctx2.

set_empty(Ctx) ->
    Ctx#{errors => astranaut_endo:empty(), warnings => astranaut_endo:empty()}.
    
append_error(Error, Ctx) ->
    append_errors([Error], Ctx).

append_errors(Errors1, #{errors := Errors0} = Ctx) when is_list(Errors1) ->
    Errors2 = astranaut_endo:append(Errors0, Errors1),
    Ctx#{errors => Errors2}.

append_warning(Warning, Ctx) ->
    append_warnings([Warning], Ctx).

append_warnings(Warnings1, #{warnings := Warnings0} = Ctx) when is_list(Warnings1) ->
    Warnings2 = astranaut_endo:append(Warnings0, Warnings1),
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
run_endo(List) when is_list(List) ->
    List;
run_endo(List) ->
    astranaut_endo:run(List).
