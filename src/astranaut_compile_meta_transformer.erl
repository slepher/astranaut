%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2020, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 29 Jun 2020 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_compile_meta_transformer).

-include("quote.hrl").
-include("astranaut_do.hrl").

%% API
-export([parse_transform/2]).
-export([format_error/1]).

%%%===================================================================
%%% API
%%%===================================================================
parse_transform(Forms, Opts) ->
    File = astranaut:file(Forms),
    MA =
        do([astranaut_return_m ||
               Forms1 <- parse_file(File, Opts),
               MetaOpts <- meta_options(Forms1),
               compile(File, Forms1, Forms, Opts, MetaOpts)
           ]),
    astranaut_return_m:to_compiler(MA).

format_error({undefined_transformer, Transformer}) ->
    io_lib:format("transformer ~p is not compiled or undefined", [Transformer]);
format_error(Error) ->
    astranaut_traverse:format_error(Error).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%===================================================================
%%% parse options functions
%%%===================================================================
meta_options(Forms) ->
    astranaut_traverse:with_attributes(
      fun(Opts, Acc) ->
              {Opts1, Warnings} = astranaut:validate_options(fun compile_meta_options_validator/2, Opts),
              Warnings1 = astranaut:update_option_warnings(astranaut_compile_meta, Warnings),
              Acc1 = merge_options(Acc, Opts1),
              astranaut_walk_return:new(
                #{node => ok, state => Acc1, warnings => Warnings1})
      end, maps:new(), astranaut_compile_meta, Forms, #{formatter => astranaut_traverse}).

merge_options(Options1, Options2) ->
    maps:fold(
      fun(transformers, Transformers, Acc) ->
              Transformers0 = maps:get(transformers, Acc, []),
              Transformers1 = unique(Transformers0 ++ Transformers),
              maps:put(transformers, Transformers1, Acc);
         (Key, Value, Acc) ->
              maps:put(Key, Value, Acc)
      end, Options1, Options2).

unique(List) ->
    unique(List, []).

unique([H|T], Acc) ->
    case lists:member(H, Acc) of
        true ->
            unique(T, Acc);
        false ->
            unique(T, [H|Acc])
    end;
unique([], Acc) ->
    lists:reverse(Acc).

compile_meta_options_validator(silent_error, Boolean) when is_boolean(Boolean) ->
    ok;
compile_meta_options_validator(silent_warning, Boolean) when is_boolean(Boolean) ->
    ok;
compile_meta_options_validator(silent, Boolean) when is_boolean(Boolean) ->
    ok;
compile_meta_options_validator(errors_export, ErrorsExport) when is_atom(ErrorsExport) ->
    ok;
compile_meta_options_validator(warnings_export, WarningsExport) when is_atom(WarningsExport) ->
    ok;
compile_meta_options_validator(forms_export, FormsExport) when is_atom(FormsExport) ->
    ok;
compile_meta_options_validator(transformers, Transformers) when is_list(Transformers) ->
    case lists:all(fun is_atom/1, Transformers) of
        true ->
            ok;
        false ->
            error
    end;
compile_meta_options_validator(_Other, _) ->
    error.

silient_error(#{silent_error := Silent}) ->
    Silent;
silient_error(#{silent := Silent}) ->
    Silent;
silient_error(#{}) ->
    false.

silient_warning(#{silent_warning := Silent}) ->
    Silent;
silient_warning(#{silent := Silent}) ->
    Silent;
silient_warning(#{}) ->
    false.

%%%===================================================================
%%% parse file functions from Erlang Otp lib/compiler/src/compile.erl
%%%===================================================================
parse_file(File, Opts) ->
    Dir = filename:dirname(File),
    SourceName0 = proplists:get_value(source, Opts, File),
    SourceName = case lists:member(deterministic, Opts) of
                     true -> filename:basename(SourceName0);
                     false -> SourceName0
                 end,
    case epp:parse_file(File,
                        [{includes,[".",Dir|inc_paths(Opts)]},
                         {source_name, SourceName},
                         {macros,pre_defs(Opts)},
                         {default_encoding, utf8},
                         extra]) of
	{ok,Forms,Extra} ->
	    Encoding = proplists:get_value(encoding, Extra),
	    case find_invalid_unicode(Forms, File) of
		none ->
		    Forms;
		{invalid_unicode, File, Line} ->
		    case Encoding of
			none ->
                            Es = [{File,[{Line, compile, reparsing_invalid_unicode}]}],
                            {error, Es, []};
			_ ->
			    Forms
		    end
	    end;
	{error,E} ->
	    Es = [{File,[{none,compile,{epp,E}}]}],
	    {error, Es, []}
    end.

find_invalid_unicode([H|T], File0) ->
    case H of
	{attribute,_,file,{File,_}} ->
	    find_invalid_unicode(T, File);
	{error,{Line,file_io_server,invalid_unicode}} ->
	    {invalid_unicode,File0,Line};
	_Other ->
	    find_invalid_unicode(T, File0)
    end;
find_invalid_unicode([], _) -> none.
            

inc_paths(Opts) ->
    [ P || {i,P} <- Opts, is_list(P) ].

pre_defs([{d,M,V}|Opts]) ->
    [{M,V}|pre_defs(Opts)];
pre_defs([{d,M}|Opts]) ->
    [M|pre_defs(Opts)];
pre_defs([_|Opts]) ->
    pre_defs(Opts);
pre_defs([]) -> [].

%%%===================================================================
%%% compile form functions
%%%===================================================================
fold_transformers(File, Transformers, Forms, Opts) ->
    to_compiler(
      astranaut_return_m:to_compiler(
        astranaut_monad:foldl_m(
          fun(Transformer, FormsAcc) ->
                  astranaut_return_m:to_monad(
                    apply_transformer(File, Transformer, FormsAcc, Opts))
          end, Forms, Transformers, astranaut_return_m))).

to_compiler(Forms) when is_list(Forms) ->
    {ok, Forms, []};
to_compiler({warning, Forms, Warnings}) ->
    {ok, Forms, Warnings};
to_compiler({error, Warnings, Errors}) ->
    {error, Warnings, Errors}.

apply_transformer(File, Transformer, Forms, Opts) ->
    try Transformer:parse_transform(Forms, Opts) of
        Return ->
            Return
    catch
        _:undef ->
            Warnings = [{File, [{0, ?MODULE, {undefined_transformer, Transformer}}]}],
            {warning, Forms, Warnings}
    end.

compile(File, OriginForms, _Forms, Opts, MetaOpts) ->
    Transformers = maps:get(transformers, MetaOpts, []),
    {OriginForms1, BTransformers, Transformers1, ATransformers} = 
        remove_transformers(OriginForms, Transformers),
    %% Warnings1 is already printed, do not report twice,
    {ok, OriginForms2, Warnings1} = 
        fold_transformers(File, BTransformers, OriginForms1, Opts),
    case fold_transformers(File, Transformers1, OriginForms2, Opts) of
        {error, Errors, Warnings2} ->
            Forms1 = append_forms([], OriginForms, Errors, Warnings1 ++ Warnings2, MetaOpts),
            report_errors(Forms1, Errors, Warnings2, MetaOpts);
        {ok, OriginForms3, Warnings2} ->
            Opts1 = Opts ++ lists:map(fun(Transformer) -> {parse_transform, Transformer} end, ATransformers),
            case compile:forms(OriginForms3, Opts1) of
                {error, Errors, Warnings3} ->
                    Warnings4 = Warnings1 ++ Warnings2 ++ Warnings3,
                    Forms1 = append_forms([], OriginForms, Errors, Warnings4, MetaOpts),
                    %% when body is empty, Warnings3 will not be generated,
                    report_errors(Forms1, Errors, Warnings2 ++ Warnings3, MetaOpts);
                {ok, _Module, _Beam, Warnings3} ->
                    %% Warnings3 will be printed, do not report it twice,
                    Warnings4 = Warnings1 ++ Warnings2 ++ Warnings3,
                    Forms1 = append_forms(OriginForms3, OriginForms, [], Warnings4, MetaOpts),
                    report_errors(Forms1, [], Warnings2, MetaOpts)
            end
    end.            

remove_transformers(Forms, Transformers) ->
    Init = #{forms => [], before_ts => [], after_ts => [], meet => false},
    #{forms := Forms1, before_ts := BTransformers, after_ts := ATransformers} = 
        lists:foldl(
          fun({attribute, _Line, compile, {parse_transform, ?MODULE}}, Acc) ->
                  Acc#{meet => true};
             ({attribute, _Line, compile, {parse_transform, Transformer}}, 
              #{before_ts := BTs, meet := false} = Acc) ->
                  Acc#{before_ts => [Transformer|BTs]};
             ({attribute, _Line, compile, {parse_transform, Transformer}}, 
              #{after_ts := ATs, meet := true} = Acc) ->
                  Acc#{after_ts => [Transformer|ATs]};
             (Node, #{forms := FormsAcc} = Acc) ->
                  Acc#{forms := [Node|FormsAcc]}
          end, Init, Forms),
    Transformers1 = Transformers -- BTransformers -- ATransformers,
    {lists:reverse(Forms1), lists:reverse(BTransformers), Transformers1, lists:reverse(ATransformers)}.

append_forms(Forms, OrigForms, Errors, Warnings, Opts) ->
    BaseForms = base_forms(Forms, OrigForms),
    [Eof|BaseForms1] = lists:reverse(BaseForms),
    BaseForms2 = lists:reverse(BaseForms1),
    MetaForms = meta_forms(OrigForms, Errors, Warnings, Opts),
    astranaut:reorder_attributes(
      BaseForms2 ++ MetaForms ++ [Eof],
      #{docks => [file, module, export], spec_as_fun => true, dock_spec => true}).

base_forms([], OrigForms) ->
    [Eof|_T] = lists:reverse(OrigForms),
    [FileAttr|_T0] = astranaut:attribute_nodes(file, OrigForms),
    [ModuleAttr|_T1] = astranaut:attribute_nodes(module, OrigForms),
    [FileAttr, ModuleAttr, Eof];
base_forms(Forms, _OrigForms) ->
    Forms.

meta_forms(Forms, Errors, Warnings, Opts) ->
    FormsExport = maps:get(forms_export, Opts, forms),
    ErrorsExport = maps:get(errors_export, Opts, errors),
    WarningsExport = maps:get(warnings_export, Opts, warnings),
    FormsForms = meta_fun_forms(FormsExport, Forms),
    ErrorsForms = meta_fun_forms(ErrorsExport, Errors),
    WarningsForms = meta_fun_forms(WarningsExport, Warnings),
    FormsForms ++ ErrorsForms ++ WarningsForms.

meta_fun_forms(MetaFun, Forms) ->
    try astranaut:abstract(Forms) of
        AbsForms ->
            astranaut:exported_function(
              MetaFun,
              quote(
                fun() ->
                        unquote(AbsForms)
                end))
    catch
        _:_Exception ->
            exit(Forms)
    end.

report_errors(Forms, Errors, Warnings, Opts) ->
    Errors1 = errors(Errors, Opts),
    Warnings1 = warnings(Warnings, Opts),
    report_warnings(Forms, Errors1 ++ Warnings1).

report_warnings(Forms, []) ->
    Forms;
report_warnings(Forms, Warnings) ->
    {warning, Forms, Warnings}.

errors(Errors, Opts) ->
    case silient_error(Opts) of
        true ->
            [];
        false ->
            Errors
    end.

warnings(Warnings, Opts) ->
    case silient_warning(Opts) of
        true ->
            [];
        false ->
            Warnings
    end.
