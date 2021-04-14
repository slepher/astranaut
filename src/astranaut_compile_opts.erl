%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2021, Chen Slepher
%%% @doc
%%% add compile_opt() which returns the compile options when compile that file
%%% helps to compile file in test directory data_dir *_SUITE_data
%%% @end
%%% Created : 19 Feb 2021 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_compile_opts).

%% API
-export([parse_transform/2, format_error/1]).

%%%===================================================================
%%% API
%%%===================================================================
parse_transform(BaseForms, Opts) ->
    OptsAst = astranaut_lib:abstract_form(Opts),
    CompileOptsForms = astranaut_lib:gen_exported_function(compile_opts, OptsAst),
    astranaut_syntax:insert_forms(CompileOptsForms, BaseForms).

format_error(Error) ->
    erl_af:format_error(Error).
