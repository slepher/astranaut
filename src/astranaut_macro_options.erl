%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2019, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 26 Jan 2019 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_macro_options).

%% API
-export([add/7]).

%%%===================================================================
%%% API
%%%===================================================================

add(MFA, Options, LocalModule, File, Line, Forms, {AllMacros, Warnings}) ->
    {Options1, Warnings1} = validate_options(Options, Line, Warnings),
    case kmfa_options(Options1, MFA, LocalModule) of
        {ok, Options2} ->
            Options3 = update_alias(Options2),
            Options4 = Options3#{local_module => LocalModule, file => File, line => Line},
            Options5 = merge_attrs(Options4, Forms),
            Options6 = formatter_opts(Options5),
            case validate_formatter(Options6, Forms) of
                ok ->
                    {[Options6|AllMacros], Warnings1};
                {error, Reason} ->
                    Options7 = Options6#{formatter => astranaut_macro},
                    {[Options7|AllMacros], [{Line, astranaut_macro, Reason}|Warnings1]}
            end;
        {error, Reason} ->
            Warnings2 = [{Line, astranaut_macro, Reason}|Warnings1],
            {AllMacros, Warnings2}
    end.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
kmfa_options(Options, {Function, Arity}, LocalModule) ->
    Module = astranaut_macro_local:module(LocalModule),
    Options1 = Options#{key => Function, module => Module, function => Function, arity => Arity, local => true},
    {ok, Options1};
kmfa_options(Options, {Module, Function, Arity}, _LocalModule) ->
    Options1 = Options#{key => {Module, Function}, module => Module, function => Function,
                        arity => Arity, local => false},
    case get_exports(Module) of
        {ok, Exports} ->
            case lists:member({Function, Arity}, Exports) of
                true ->
                    {ok, Options1};
                false ->
                    {error, {unexported_macro, Module, Function, Arity}}
            end;
        {error, undef} ->
            {error, {unloaded_macro_module, Module}}
    end.

merge_attrs(#{attrs := Attrs, local_module := Module, file := File, line := Line} = Opts, Forms) ->
    AttributesMap = 
        lists:foldl(
          fun(module, Acc) ->
                  Acc;
             (file, Acc) ->
                  Acc;
             (line, Acc) ->
                  Acc;
             (Attr, Acc) ->
                  Attributes = astranaut:attributes(Attr, Forms),
                  maps:put(Attr, Attributes, Acc)
          end, maps:new(), Attrs),
    Opts#{attributes => maps:merge(#{module => Module, file => File, line => Line}, AttributesMap)};
merge_attrs(#{} = Opts, _Forms) ->
    Opts.

formatter_opts(#{formatter := Module, local_module := Module} = Options) ->
    Options#{formatter => astranaut_macro_local:module(Module), local_formatter => true};
formatter_opts(#{formatter := true, module := Module, local := true} = Options) ->
    Options#{formatter => Module, local_formatter => true};
formatter_opts(#{formatter := true, module := Module} = Options) ->
    Options#{formatter => Module};
formatter_opts(#{formatter := Formatter} = Options) ->
    Options#{formatter => Formatter};
formatter_opts(#{} = Options) ->
    Options#{formatter => astranaut_traverse}.

validate_formatter(#{local_formatter := true, local_module := Module}, Forms) ->
    Exports = astranaut:attributes(export, Forms),
    case lists:member({format_error, 1}, lists:flatten(Exports)) of
        true ->
            ok;
        false ->
            {error, {non_exported_formatter, Module}}
    end;
validate_formatter(#{formatter := Module}, _Forms) ->
    case get_exports(Module) of
        {ok, Exports} ->
            case lists:member({format_error, 1}, Exports) of
                true ->
                    ok;
                false ->
                    {error, {non_exported_formatter, Module}}
            end;
        {error, undef} ->
            {error, {unloaded_formatter_module, Module}}
    end.

update_alias(#{alias := Alias} = Options) ->
    Options#{macro => Alias};
update_alias(#{key := Key} = Options) ->
    Options#{macro => Key}.



validate_options(Opts, Line, Warnings) when is_map(Opts) ->
    maps:fold(
        fun(Key, Value, {OptsAcc, WarningAcc} = Acc) ->
                case validate_option_key(Key, Value) of
                    ok ->
                        Acc;
                    error ->
                        NWarningsAcc = [{Line, astranaut_macro, {invalid_option_value, Key, Value}}|WarningAcc],
                        NOptsAcc = maps:remove(Key, OptsAcc),
                        {NOptsAcc, NWarningsAcc}
                end
        end, {Opts, Warnings}, Opts);
validate_options(Opts, Line, Warnings) when is_list(Opts) ->
    {Opts1, Warnings1} = 
        lists:foldl(
          fun({Key, Value}, {OptsAcc, WarningsAcc}) when is_atom(Key) ->
                  {maps:put(Key, Value, OptsAcc), WarningsAcc};
             (Key, {OptsAcc, WarningsAcc}) when is_atom(Key) ->
                  {maps:put(Key, true, OptsAcc), WarningsAcc};
             (Value, {OptsAcc, WarningsAcc}) ->
                  {OptsAcc, [{Line, astranaut_macro, {invalid_option_value, Value}}|WarningsAcc]}
          end, {maps:new(), Warnings}, Opts),
    validate_options(Opts1, Line, Warnings1);
validate_options(Opts, Line, Warnings) ->
    {maps:new(), [{Line, astranaut_macro, {invalid_option, Opts}}|Warnings]}.

validate_option_key(attrs, Attrs) when is_atom(Attrs) ->
    ok;
validate_option_key(attrs, Attrs) when is_list(Attrs) ->
    case lists:filter(
           fun(Attr) ->
                   not is_atom(Attr)
           end, Attrs) of
        [] ->
            ok;
        _InvalidAttrs ->
            error
    end;
validate_option_key(order, pre) ->
    ok;
validate_option_key(order, post) ->
    ok;
validate_option_key(formatter, Formatter) when is_atom(Formatter) ->
    ok;
validate_option_key(as_attr, AsAttr) when is_atom(AsAttr) ->
    ok;
validate_option_key(auto_export, Bool) when is_boolean(Bool) ->
    ok;
validate_option_key(group_args, Bool) when is_boolean(Bool) ->
    ok;
validate_option_key(merge_function, head) ->
    ok;
validate_option_key(merge_function, tail) ->
    ok;
validate_option_key(merge_function, Bool) when is_boolean(Bool) ->
    ok;
validate_option_key(debug, Value) when is_boolean(Value) ->
    ok;
validate_option_key(debug_ast, Value) when is_boolean(Value) ->
    ok;
validate_option_key(alias, Value) when is_atom(Value) ->
    ok;
validate_option_key(_Key, _Value) ->
    error.


get_exports(Module) ->
    try
        {ok, Module:module_info(exports)}
    catch
        _:undef ->
            {error, undef}
    end.
