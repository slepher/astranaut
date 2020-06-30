%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2020, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 29 Jun 2020 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_fail_compiler).

%% API
-export([compile/2]).
-export([copy/2, copy/3]).

-include_lib("common_test/include/ct.hrl").

%%%===================================================================
%%% API
%%%===================================================================
compile(Basename, Config) ->
    From = Basename ++ ".erlf",
    To = Basename ++ ".erl",
    Filename = copy(From, To, Config),
    compile:file(Filename, [return_errors]).

copy(From, Config) ->
    copy(From, From, Config).

copy(From, To, Config) ->
    PrivDir = ?config(priv_dir, Config),
    DataDir = ?config(data_dir, Config),
    FilenameBefore = filename:join(DataDir, From),
    FilenameAfter = filename:join(PrivDir, To),
    case filelib:is_file(FilenameBefore) of
        true ->
            file:copy(FilenameBefore, FilenameAfter),
            FilenameAfter;
        false ->
            exit({file_not_exists, FilenameBefore})
    end.

%% parse_file(File, Opts) ->
%%     epp:parse_file(File,
%%                    [{includes,[".",Dir|inc_paths(Opts)]},
%%                     {source_name, SourceName},
%%                     {macros,pre_defs(Opts)},
%%                     {default_encoding,DefEncoding},
%%                     extra]).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
