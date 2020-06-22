%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2020, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 22 Jun 2020 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_traverse_error_state).

%% API
-export([new/0]).
-export([realize/1]).
-export([update_file/2]).
-export([warning/2, warnings/2, error/2, errors/2]).

%%%===================================================================
%%% API
%%%===================================================================
new() ->
    #{errors => [], warnings => [], file_errors => #{}, file_warnings => #{}, file => undefined}.

realize(#{file_errors := FileErrors, file_warnings := FileWarnings}) ->
    {maps:to_list(FileErrors), maps:to_list(FileWarnings)}.

update_file(File, #{errors := Errors, warnings := Warnings, 
                    file_errors := ErrorsWithFile, 
                    file_warnings := WarningsWithFile,
                    file := File1} = State) ->
    case File == File1 of
        true ->
            State;
        false ->
            ErrorsWithFile1 = merge_errors_file(File1, Errors, ErrorsWithFile),
            WarningsWithFile1 = merge_errors_file(File1, Warnings, WarningsWithFile),
            State#{errors => [], warnings => [], 
                   file_errors => ErrorsWithFile1, 
                   file_warnings => WarningsWithFile1,
                   file => File}
    end.

warning(Warning, State) ->
    warnings([Warning], State).
    
warnings(Warnings, #{warnings := Warnings0} = State) ->
    State#{warnings => lists:reverse(Warnings) ++ Warnings0}.

error(Error, State) ->
    errors([Error], State).
    
errors(Errors, #{errors := Errors0} = State) ->
    State#{errors => lists:reverse(Errors) ++ Errors0}.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
merge_errors_file(_File, [], ErrorsWithFile) ->
    ErrorsWithFile;
merge_errors_file(File, Errors, ErrorsWithFile) ->
    case maps:find(File, ErrorsWithFile) of
        {ok, Errors1} ->
            maps:put(File, Errors1 ++ lists:reverse(Errors), ErrorsWithFile);
        error ->
            maps:put(File, lists:reverse(Errors), ErrorsWithFile)
    end.
