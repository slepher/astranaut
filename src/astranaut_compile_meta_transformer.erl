%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2020, Chen Slepher
%%% @doc
%%% Compatibility parse_transform for compile metadata collection.
%%% It records transformed forms, compile errors, and warnings into
%%% generated exported functions.
%%% @end
%%%-------------------------------------------------------------------
-module(astranaut_compile_meta_transformer).

-include("stacktrace.hrl").

%% API
-export([parse_transform/2, format_error/1]).

parse_transform(Forms, Opts) ->
    File = astranaut_lib:analyze_forms_file(Forms),
    MetaOpts = meta_options(Forms),
    BaseForms = strip_parse_transform_attrs(Forms),
    Transformers = compile_transformers(Forms, MetaOpts),
    {TransformedForms, TErrors, TWarnings} =
        apply_transformers(File, Transformers, BaseForms, Opts),
    {CErrors, CWarnings} = compile_check(TransformedForms, Opts),
    Errors = TErrors ++ CErrors,
    Warnings = TWarnings ++ CWarnings,
    FormsForModule =
        case Errors of
            [] ->
                TransformedForms;
            _ ->
                base_forms(Forms)
        end,
    OutputForms = append_meta_forms(FormsForModule, Forms, Errors, Warnings, MetaOpts),
    report(OutputForms, Errors, Warnings, MetaOpts).

format_error({undefined_transformer, Transformer}) ->
    io_lib:format("transformer ~p is not compiled or undefined", [Transformer]);
format_error(Error) ->
    astranaut:format_error(Error).

%%%===================================================================
%%% meta options
%%%===================================================================
meta_options(Forms) ->
    Values = astranaut_lib:analyze_forms_attributes(astranaut_compile_meta, Forms),
    lists:foldl(fun merge_meta_value/2, #{}, Values).

merge_meta_value(Value, Acc) ->
    maps:merge(Acc, to_option_map(Value)).

to_option_map(Value) when is_atom(Value) ->
    #{Value => true};
to_option_map({Key, Val}) when is_atom(Key) ->
    #{Key => Val};
to_option_map(List) when is_list(List) ->
    lists:foldl(
      fun(Item, Acc) ->
              maps:merge(Acc, to_option_map(Item))
      end, #{}, List);
to_option_map(_) ->
    #{}.

silent_error(#{silent_error := Silent}) ->
    Silent;
silent_error(#{silent := Silent}) ->
    Silent;
silent_error(#{}) ->
    false.

silent_warning(#{silent_warning := Silent}) ->
    Silent;
silent_warning(#{silent := Silent}) ->
    Silent;
silent_warning(#{}) ->
    false.

%%%===================================================================
%%% transformers
%%%===================================================================
compile_transformers(Forms, MetaOpts) ->
    AttrTransformers =
        lists:foldl(
          fun({attribute, _Line, compile, {parse_transform, ?MODULE}}, Acc) ->
                  Acc;
             ({attribute, _Line, compile, {parse_transform, Transformer}}, Acc) when is_atom(Transformer) ->
                  [Transformer|Acc];
             (_Node, Acc) ->
                  Acc
          end, [], Forms),
    ExtraTransformers = maps:get(transformers, MetaOpts, []),
    unique(lists:reverse(AttrTransformers) ++ ExtraTransformers).

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

apply_transformers(File, Transformers, Forms, Opts) ->
    lists:foldl(
      fun(Transformer, {FormsAcc, ErrorsAcc, WarningsAcc}) ->
              {Forms1, Errors1, Warnings1} = apply_transformer(File, Transformer, FormsAcc, Opts),
              {Forms1, ErrorsAcc ++ Errors1, WarningsAcc ++ Warnings1}
      end, {Forms, [], []}, Transformers).

apply_transformer(File, Transformer, Forms, Opts) ->
    try Transformer:parse_transform(Forms, Opts) of
        Forms1 when is_list(Forms1) ->
            {Forms1, [], []};
        {warning, Forms1, Warnings} when is_list(Forms1) ->
            {Forms1, [], Warnings};
        {error, Errors, Warnings} ->
            {Forms, Errors, Warnings};
        Other ->
            Warn = warning_tuple(File, {invalid_transformer_return, Transformer, Other}),
            {Forms, [], [Warn]}
    catch
        error:undef ->
            Warn = warning_tuple(File, {undefined_transformer, Transformer}),
            {Forms, [], [Warn]};
        Class:Reason?CAPTURE_STACKTRACE ->
            Error = parse_transform_error(File, Transformer, {Class, Reason, ?GET_STACKTRACE}),
            {Forms, [Error], []}
    end.

compile_check(Forms, Opts) ->
    Opts1 =
        [return_errors, return_warnings] ++
        [Opt || Opt <- Opts, not is_parse_transform_opt(Opt), Opt =/= report_errors, Opt =/= report_warnings],
    case compile:forms(Forms, Opts1) of
        {ok, _Module, _Binary, Warnings} ->
            {[], Warnings};
        {ok, _Module, _Binary} ->
            {[], []};
        {error, Errors, Warnings} ->
            {Errors, Warnings}
    end.

is_parse_transform_opt({parse_transform, _}) ->
    true;
is_parse_transform_opt(_) ->
    false.

strip_parse_transform_attrs(Forms) ->
    lists:filter(
      fun({attribute, _Line, compile, {parse_transform, _}}) ->
              false;
         (_Node) ->
              true
      end, Forms).

%%%===================================================================
%%% forms/meta output
%%%===================================================================
append_meta_forms(FormsForModule, FormsValue, ErrorsValue, WarningsValue, MetaOpts) ->
    FormsExport = maps:get(forms_export, MetaOpts, forms),
    ErrorsExport = maps:get(errors_export, MetaOpts, errors),
    WarningsExport = maps:get(warnings_export, MetaOpts, warnings),
    MetaForms =
        meta_fun_forms(FormsExport, FormsValue) ++
        meta_fun_forms(ErrorsExport, ErrorsValue) ++
        meta_fun_forms(WarningsExport, WarningsValue),
    astranaut_syntax:insert_forms(MetaForms, strip_parse_transform_attrs(FormsForModule)).

meta_fun_forms(MetaFun, Value) when is_atom(MetaFun) ->
    astranaut_lib:gen_exported_function(MetaFun, astranaut_lib:abstract_form(Value));
meta_fun_forms(_, _) ->
    [].

base_forms(OrigForms) ->
    FileAttrs =
        lists:filter(
          fun({attribute, _Pos, file, _}) -> true;
             (_) -> false
          end, OrigForms),
    ModuleAttrs =
        lists:filter(
          fun({attribute, _Pos, module, _}) -> true;
             (_) -> false
          end, OrigForms),
    Eof = last_eof(OrigForms),
    lists:append([FileAttrs, ModuleAttrs, [Eof]]).

last_eof(Forms) ->
    case lists:reverse(Forms) of
        [{eof, _} = Eof|_T] ->
            Eof;
        _ ->
            {eof, 0}
    end.

report(Forms, Errors, Warnings, MetaOpts) ->
    Errors1 =
        case silent_error(MetaOpts) of
            true -> [];
            false -> Errors
        end,
    Warnings1 =
        case silent_warning(MetaOpts) of
            true -> [];
            false -> Warnings
        end,
    case Errors1 ++ Warnings1 of
        [] ->
            Forms;
        EW ->
            {warning, Forms, EW}
    end.

warning_tuple(File, Reason) ->
    {File, [{0, ?MODULE, Reason}]}.

parse_transform_error(File, Transformer, Reason) ->
    {File, [{none, compile, {parse_transform, Transformer, Reason}}]}.
