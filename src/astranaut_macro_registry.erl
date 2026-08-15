%%%-------------------------------------------------------------------
%%% @doc Macro declarations and source-ordered macro environments.
%%%
%%% This module owns macro descriptor construction, directive validation,
%%% checked registry updates and AttributeEnv-based environment resolution.
%%% It does not own the source queue or execute macro calls.
%%% @end
%%%-------------------------------------------------------------------

-module(astranaut_macro_registry).

-include("do.hrl").

-export([
    new/3,
    default_options/0,
    validate_definition_attribute/3,
    macro_definition_validator/0,
    remove_undefined_macros/2,
    apply_directive/2,
    add_macro_definitions/2,
    prepare_exports/1,
    note_form/2,
    declaration_macro_environment/1,
    resolve_attribute_target/2,
    final_macro_environment/2,
    effective_macro_map/1,
    attribute_macro_index/1,
    global_macro_opts/1,
    context/1
]).

-type macro_map() :: map().
-type attribute_env() ::
    #{
        module := module(),
        file := file:filename(),
        values := #{atom() => [term()]}
    }.
-type macro_environment() ::
    #{
        macro_map := macro_map(),
        macro_options := map(),
        function_call_analysis => map()
    }.
-type state() ::
    #{
        module := module(),
        file := file:filename(),
        global_macro_opts := map(),
        missing_formatters := ordsets:ordset(module()),
        module_macro_maps := map(),
        macro_map := macro_map(),
        effective_macro_map := macro_map(),
        attribute_macro_index := map(),
        attribute_env := attribute_env()
    }.

-export_type([state/0, macro_environment/0]).

%%%===================================================================
%%% Public API
%%%===================================================================

-spec new(module(), file:filename(), map()) -> state().
new(Module, File, GlobalMacroOpts) ->
    #{
        module => Module,
        file => File,
        global_macro_opts => GlobalMacroOpts,
        missing_formatters => ordsets:new(),
        module_macro_maps => #{},
        macro_map => #{},
        effective_macro_map => #{},
        attribute_macro_index => #{},
        attribute_env => new_attribute_env(Module, File)
    }.

-spec default_options() -> astranaut_return:struct(map()).
default_options() ->
    astranaut_lib:validate(global_macro_validator(), []).

-spec validate_definition_attribute(atom(), term(), map()) ->
    astranaut_return:struct({[term()], map()}).
validate_definition_attribute(AttrName, Attr, Validator) ->
    validate_macro_attribute(
        fun macro_without_module_attr/1,
        Validator,
        AttrName,
        Attr
    ).

-spec macro_definition_validator() -> map().
macro_definition_validator() ->
    common_macro_definition_validator().

-spec apply_directive(term(), state()) ->
    astranaut_return:struct({keep | consume, state()}).
apply_directive(
    {attribute, _Pos, import_macro, ProviderModule} = Form,
    #{
        global_macro_opts := GlobalMacroOpts,
        module := Module,
        file := File,
        macro_map := MacroMap,
        effective_macro_map := EffectiveMacroMap,
        missing_formatters := MissingFormatters
    } = State
) ->
    case import_macro_form(GlobalMacroOpts, Form) of
        {ok, ModuleMacroMap, FormatterProtocol} ->
            Effective = effective_module_macro_maps(
                File, Module, ModuleMacroMap
            ),
            NewMap = uniform_imported_macro_map(Effective),
            do([
                return
             || Merged <- merge_macro_maps(MacroMap, NewMap),
                EffectiveMerged <- merge_macro_maps(
                    EffectiveMacroMap, NewMap
                ),
                ModuleMacroMaps = maps:merge(
                    maps:get(module_macro_maps, State),
                    Effective
                ),
                MissingFormatters1 = note_missing_formatter(
                    FormatterProtocol,
                    ProviderModule,
                    MissingFormatters
                ),
                State1 = set_effective_macro_map(
                    EffectiveMerged,
                    State#{
                        module_macro_maps => ModuleMacroMaps,
                        macro_map => Merged,
                        missing_formatters =>
                            MissingFormatters1
                    }
                ),
                maybe_missing_formatter_warning(
                    FormatterProtocol, ProviderModule, MissingFormatters
                ),
                return({consume, State1})
            ]);
        {error, Error} ->
            astranaut_return:error_fail(Error)
    end;
apply_directive(
    {attribute, _Pos, use_macro, _Attr} = Form,
    #{
        module := Module,
        file := File,
        macro_map := MacroMap,
        effective_macro_map := EffectiveMacroMap
    } = State
) ->
    ImportedMacros = module_macro_maps_from_uniform(MacroMap),
    do([
        return
     || UsedMacros <- used_macros(
            File, Module, ImportedMacros, [Form]
        ),
        NewMap = uniform_imported_macro_map(UsedMacros),
        Merged <- merge_macro_maps(MacroMap, NewMap),
        EffectiveMerged <- merge_macro_maps(EffectiveMacroMap, NewMap),
        ModuleMacroMaps0 = maps:get(module_macro_maps, State),
        ModuleMacroMaps1 = maps:fold(
            fun(M, Macros, Acc) ->
                M1 = maps:get(M, Acc, #{}),
                maps:put(
                    M, maps:merge(M1, Macros), Acc
                )
            end,
            ModuleMacroMaps0,
            UsedMacros
        ),
        return(
            {consume,
                set_effective_macro_map(
                    EffectiveMerged,
                    State#{
                        module_macro_maps => ModuleMacroMaps1,
                        macro_map => Merged
                    }
                )}
        )
    ]);
apply_directive(
    {attribute, _Pos, macro_options, Attr},
    #{global_macro_opts := GlobalMacroOpts} = State
) ->
    do([
        return
     || MacroOpts <- astranaut_lib:validate(
            global_macro_update_validator(), Attr
        ),
        return(
            {keep, State#{
                global_macro_opts =>
                    maps:merge(GlobalMacroOpts, MacroOpts)
            }}
        )
    ]).

-spec add_macro_definitions(map(), state()) ->
    astranaut_return:struct(state()).
add_macro_definitions(
    ModuleMacros,
    #{
        module := Module,
        file := File,
        effective_macro_map := EffectiveMap
    } = State
) ->
    New = update_module_macros(File, Module, ModuleMacros),
    do([
        return
     || EffectiveMap1 <- merge_macro_maps(EffectiveMap, New),
        return(set_effective_macro_map(EffectiveMap1, State))
    ]).

-spec prepare_exports([term()]) -> astranaut_return:struct([term()]).
prepare_exports(Forms) ->
    ClausesMap = function_clauses_map(Forms, #{}),
    exported_macros(Forms, ClausesMap).

-spec note_form(term(), state()) -> state().
note_form(Form, #{attribute_env := AttributeEnv} = State) ->
    State#{attribute_env => add_attribute_form(Form, AttributeEnv)}.

-spec declaration_macro_environment(state()) -> macro_environment().
declaration_macro_environment(
    #{
        effective_macro_map := MacroMap,
        global_macro_opts := GlobalOpts,
        attribute_env := AttributeEnv
    }
) ->
    macro_environment(
        resolve_macro_environment(MacroMap, AttributeEnv),
        GlobalOpts
    ).

-spec resolve_attribute_target(map(), state()) -> map().
resolve_attribute_target(
    #{macro := Macro} = Target,
    #{attribute_env := AttributeEnv}
) ->
    Target#{macro => resolve_macro_attributes(Macro, AttributeEnv)}.

-spec final_macro_environment([term()], state()) ->
    macro_environment().
final_macro_environment(
    Forms,
    #{
        module := Module,
        file := File,
        global_macro_opts := GlobalMacroOpts,
        effective_macro_map := EffectiveMacroMap
    }
) ->
    ResolvedFinalMacroMap = resolve_macro_environment(
        EffectiveMacroMap,
        attribute_env_from_forms(
            Module, File, Forms
        )
    ),
    macro_environment(ResolvedFinalMacroMap, GlobalMacroOpts).

-spec effective_macro_map(state()) -> macro_map().
effective_macro_map(#{effective_macro_map := MacroMap}) -> MacroMap.

-spec attribute_macro_index(state()) -> map().
attribute_macro_index(#{attribute_macro_index := Index}) -> Index.

-spec global_macro_opts(state()) -> map().
global_macro_opts(#{global_macro_opts := Options}) -> Options.

-spec context(state()) -> map().
context(#{
    module := Module,
    file := File,
    global_macro_opts := GlobalMacroOpts
}) ->
    #{
        module => Module,
        file => File,
        global_macro_opts => GlobalMacroOpts
    }.

%%%===================================================================
%%% Attribute environments
%%%===================================================================

macro_environment(MacroMap, MacroOptions) ->
    #{
        macro_map => MacroMap,
        macro_options => MacroOptions
    }.

new_attribute_env(Module, File) ->
    #{module => Module, file => File, values => #{}}.

attribute_env_from_forms(Module, File, Forms) ->
    lists:foldl(
        fun add_attribute_form/2,
        new_attribute_env(Module, File),
        Forms
    ).

add_attribute_form(
    {attribute, _Pos, Name, Value},
    #{values := Values} = AttributeEnv
) when
    Name =/= module, Name =/= file, Name =/= pos
->
    Values1 = maps:update_with(
        Name,
        fun(Existing) -> [Value | Existing] end,
        [Value],
        Values
    ),
    AttributeEnv#{values => Values1};
add_attribute_form(_Form, AttributeEnv) ->
    AttributeEnv.

resolve_macro_environment(MacroMap, AttributeEnv) ->
    maps:map(
        fun(_MacroKey, Macro) ->
            resolve_macro_attributes(Macro, AttributeEnv)
        end,
        MacroMap
    ).

resolve_macro_attributes(
    #{inject_attrs := InjectAttrs} = Macro, AttributeEnv
) ->
    AttributeNames = normalize_inject_attrs(InjectAttrs),
    Attributes = select_injected_attributes(
        AttributeNames, AttributeEnv
    ),
    maps:remove(inject_attrs, Macro#{attributes => Attributes});
resolve_macro_attributes(Macro, _AttributeEnv) ->
    Macro.

normalize_inject_attrs(true) -> [];
normalize_inject_attrs(Name) when is_atom(Name) -> [Name];
normalize_inject_attrs(Names) when is_list(Names) -> Names.

select_injected_attributes(
    Names, #{module := Module, file := File, values := Values}
) ->
    Selected =
        lists:foldl(
            fun
                (module, Acc) ->
                    Acc;
                (file, Acc) ->
                    Acc;
                (pos, Acc) ->
                    Acc;
                (Name, Acc) ->
                    maps:put(
                        Name,
                        lists:reverse(maps:get(Name, Values, [])),
                        Acc
                    )
            end,
            #{},
            Names
        ),
    maps:merge(#{module => Module, file => File}, Selected).

set_effective_macro_map(MacroMap, State) ->
    State#{
        effective_macro_map => MacroMap,
        attribute_macro_index =>
            astranaut_macro_expander:attribute_macro_index(MacroMap)
    }.

%%%===================================================================
%%% Export declarations
%%%===================================================================

exported_macros(Forms, ClausesMap) ->
    astranaut:map(
        fun
            ({attribute, Pos, export_macro, Attr} = Form) ->
                astranaut_return:bind(
                    exported_macro_forms(Attr, Pos, ClausesMap),
                    fun
                        ([]) ->
                            astranaut_return:return(Form);
                        (GeneratedForms) ->
                            astranaut_return:return([Form | GeneratedForms])
                    end
                );
            (Form) ->
                astranaut_return:return(Form)
        end,
        Forms,
        #{traverse => none, formatter => astranaut_macro}
    ).

exported_macro_forms(Attrs, Pos, ClausesMap) when is_list(Attrs) ->
    astranaut_return:foldl_m(
        fun(Attr, Acc) ->
            astranaut_return:lift_m(
                fun(Forms) -> Forms ++ Acc end,
                exported_macro_forms(Attr, Pos, ClausesMap)
            )
        end,
        [],
        Attrs
    );
exported_macro_forms(Attr, Pos, ClausesMap) ->
    do([
        return
     || Validator = export_macro_validator(),
        {FAs, Options} <- validate_macro_attribute(
            fun macro_without_module_attr/1,
            Validator,
            export_macro,
            Attr
        ),
        FAs1 <- remove_undefined_macros(FAs, ClausesMap),
        case FAs1 of
            [] ->
                astranaut_return:return([]);
            _ ->
                ExportedMacroAttribute =
                    astranaut_lib:gen_attribute_node(
                        exported_macro, Pos, [{FAs, Options}]
                    ),
                ExportAttribute =
                    astranaut_lib:gen_attribute_node(export, Pos, FAs),
                astranaut_return:return(
                    [ExportAttribute, ExportedMacroAttribute]
                )
        end
    ]).

remove_undefined_macros(FAs, ClausesMap) ->
    astranaut_return:foldl_m(
        fun({Function, Arity} = FA, Acc) ->
            case maps:is_key(FA, ClausesMap) of
                true ->
                    astranaut_return:return([FA | Acc]);
                false ->
                    astranaut_return:error_ok(
                        {undefined_macro, Function, Arity}, Acc
                    )
            end
        end,
        [],
        FAs
    ).

%%%===================================================================
%%% Imported and used macros
%%%===================================================================

module_macro_maps_from_uniform(MacroMap) ->
    maps:fold(
        fun(Key, #{macro_module := MacroModule} = Macro, Acc) ->
            ModuleMacroMap = maps:get(MacroModule, Acc, #{}),
            maps:put(
                MacroModule,
                maps:put(Key, Macro, ModuleMacroMap),
                Acc
            )
        end,
        #{},
        MacroMap
    ).

import_macro_form(
    GlobalMacroOpts,
    {attribute, _Pos, import_macro, Module}
) when is_atom(Module) ->
    case ensure_loaded(Module) of
        true ->
            Macros = analyze_module_macros(Module),
            Exports = Module:module_info(exports),
            {GlobalMacroOpts1, FormatterProtocol} = formatter_opts(
                Module,
                Exports,
                GlobalMacroOpts
            ),
            Macros1 =
                maps:fold(
                    fun({Function, Arity}, MacroOptions, Acc) ->
                        MacroOptions1 = maps:merge(
                            GlobalMacroOpts1,
                            MacroOptions
                        ),
                        MacroOptions2 =
                            MacroOptions1#{
                                module => Module,
                                macro_module => Module,
                                macro => {Module, Function},
                                function => Function,
                                arity => Arity
                            },
                        maps:put({Function, Arity}, MacroOptions2, Acc)
                    end,
                    #{},
                    Macros
                ),
            {ok, #{Module => Macros1}, FormatterProtocol};
        false ->
            {error, {import_macro_failed, Module}}
    end;
import_macro_form(
    _GlobalMacroOpts,
    {attribute, _Pos, import_macro, Attr}
) ->
    {error, {invalid_import_macro_attr, Attr}}.

formatter_opts(Module, Functions, MacroOpts) ->
    case lists:member({format_error, 1}, Functions) of
        true -> {MacroOpts#{formatter => Module}, present};
        false -> {MacroOpts#{formatter => astranaut_macro}, missing}
    end.

note_missing_formatter(present, _Module, MissingFormatters) ->
    MissingFormatters;
note_missing_formatter(missing, Module, MissingFormatters) ->
    ordsets:add_element(Module, MissingFormatters).

maybe_missing_formatter_warning(present, _Module, _MissingFormatters) ->
    astranaut_return:return(ok);
maybe_missing_formatter_warning(missing, Module, MissingFormatters) ->
    case ordsets:is_element(Module, MissingFormatters) of
        true -> astranaut_return:return(ok);
        false -> astranaut_return:warning({missing_macro_formatter, Module})
    end.

ensure_loaded(Module) ->
    case code:ensure_loaded(Module) of
        {module, Module} ->
            true;
        {error, Reason} ->
            log_macro_load(Module, Reason),
            false
    end.

log_macro_load(Module, Reason) ->
    io:format(
        standard_error,
        "astranaut_macro: load ~p failed: ~p~nwhere_is_file=~p~npaths=~p~n",
        [
            Module,
            Reason,
            code:where_is_file(atom_to_list(Module) ++ ".beam"),
            [P || P <- code:get_path(), string:find(P, "erlando") =/= nomatch]
        ]
    ).

update_module_macros(File, Module, ModuleMacros) ->
    maps:fold(
        fun(_MFA, MacroOptions, Acc) ->
            MacroOptions1 = MacroOptions#{
                file => File,
                local_module => Module
            },
            MacroOptions2 = update_as_attr(MacroOptions1),
            #{macro := Macro, call_arity := CallArity} =
                MacroOptions3 = update_call_arity(MacroOptions2),
            maps:put({Macro, CallArity}, MacroOptions3, Acc)
        end,
        #{},
        ModuleMacros
    ).

used_macros(File, Module, ImportedMacros, Forms) ->
    ImportedMacroMap = effective_module_macro_maps(
        File, Module, ImportedMacros
    ),
    astranaut_lib:with_attribute(
        fun(Attr, UsedMacroMapAcc) ->
            do([
                return
             || Validator = use_macro_validator(),
                {MFAs, Options} <- validate_macro_attribute(
                    fun macro_attr/1,
                    Validator,
                    use_macro,
                    Attr
                ),
                case MFAs of
                    {ImportedModule, FAs} ->
                        case
                            maps:is_key(
                                ImportedModule, UsedMacroMapAcc
                            )
                        of
                            true ->
                                update_used_macro_maps(
                                    File,
                                    Module,
                                    ImportedModule,
                                    FAs,
                                    Options,
                                    UsedMacroMapAcc,
                                    fun({Function, Arity}) ->
                                        {unexported_macro, ImportedModule, Function, Arity}
                                    end
                                );
                            false ->
                                astranaut_return:error_fail(
                                    {unimported_macro_module, ImportedModule}
                                )
                        end;
                    FAs ->
                        update_used_macro_maps(
                            File,
                            Module,
                            Module,
                            FAs,
                            Options,
                            UsedMacroMapAcc,
                            fun({Function, Arity}) ->
                                {undefined_macro, Function, Arity}
                            end
                        )
                end
            ])
        end,
        ImportedMacroMap,
        Forms,
        use_macro,
        #{formatter => astranaut_macro}
    ).

effective_module_macro_maps(File, Module, ModuleMacros) ->
    maps:map(
        fun(_MacroModule, Macros) ->
            update_module_macros(File, Module, Macros)
        end,
        ModuleMacros
    ).

update_used_macro_maps(
    File,
    Module,
    MacroModule,
    FAs,
    UsedMacroOptions,
    UsedMacroMapAcc,
    MissingFun
) ->
    astranaut_return:foldl_m(
        fun(FA, Acc) ->
            ModuleMacroMap = maps:get(MacroModule, Acc, #{}),
            case find_used_macro(FA, ModuleMacroMap) of
                {ok, MacroKey, MacroOptions} ->
                    MacroOptions1 = maps:merge(
                        MacroOptions, UsedMacroOptions
                    ),
                    MacroOptions2 = update_alias(MacroOptions1),
                    CurrentMacroMap = update_module_macros(
                        File,
                        Module,
                        #{FA => MacroOptions2}
                    ),
                    ModuleMacroMapWithoutCurrent =
                        maps:remove(MacroKey, ModuleMacroMap),
                    ExistingUsedMacroMap = maps:put(
                        MacroModule,
                        ModuleMacroMapWithoutCurrent,
                        Acc
                    ),
                    ExistingMacroMap = uniform_imported_macro_map(
                        ExistingUsedMacroMap
                    ),
                    do([
                        return
                     || assert_macro_map_no_overrides(
                            CurrentMacroMap, ExistingMacroMap
                        ),
                        ModuleMacroMap1 = maps:merge(
                            ModuleMacroMapWithoutCurrent,
                            CurrentMacroMap
                        ),
                        return(
                            maps:put(
                                MacroModule, ModuleMacroMap1, Acc
                            )
                        )
                    ]);
                error ->
                    astranaut_return:error_ok(MissingFun(FA), Acc)
            end
        end,
        UsedMacroMapAcc,
        FAs
    ).

find_used_macro({Function, Arity}, ModuleMacroMap) ->
    maps:fold(
        fun
            (
                MacroKey,
                #{function := Function1, arity := Arity1} = MacroOptions,
                error
            ) when Function =:= Function1, Arity =:= Arity1 ->
                {ok, MacroKey, MacroOptions};
            (_MacroKey, _MacroOptions, Acc) ->
                Acc
        end,
        error,
        ModuleMacroMap
    ).

assert_macro_map_no_overrides(MacroMap, ExistingMacroMap) ->
    astranaut_return:foldl_m(
        fun({MacroKey, Macro}, ExistingMacroMapAcc) ->
            case maps:find(MacroKey, ExistingMacroMapAcc) of
                {ok, ExistingMacro} ->
                    case maps:get(force_override, Macro, false) of
                        true ->
                            astranaut_return:return(
                                maps:put(
                                    MacroKey, Macro, ExistingMacroMapAcc
                                )
                            );
                        false ->
                            macro_override_fail(
                                MacroKey, ExistingMacro, Macro
                            )
                    end;
                error ->
                    astranaut_return:return(
                        maps:put(MacroKey, Macro, ExistingMacroMapAcc)
                    )
            end
        end,
        ExistingMacroMap,
        maps:to_list(MacroMap)
    ).

uniform_imported_macro_map(UsedMacroMap) ->
    maps:fold(
        fun(_MacroModule, MacroMap, Acc) -> maps:merge(Acc, MacroMap) end,
        #{},
        UsedMacroMap
    ).

update_alias(#{alias := true, function := Function} = Options) ->
    Options#{macro => Function};
update_alias(#{alias := Alias} = Options) ->
    Options#{macro => Alias};
update_alias(#{} = Options) ->
    Options.

update_as_attr(#{as_attr := true, function := Function} = Options) ->
    Options#{as_attr => Function};
update_as_attr(#{} = Options) ->
    Options.

update_call_arity(Opts) ->
    Opts#{call_arity => call_arity(Opts)}.

call_arity(#{group_args := true} = Opts) ->
    call_arity(maps:remove(group_args, Opts#{arity => 1}));
call_arity(#{arity := Arity} = Opts) ->
    case maps:get(inject_attrs, Opts, false) of
        false -> Arity;
        _ -> Arity - 1
    end.

analyze_module_macros(Module) ->
    ModuleMacroAttributes =
        astranaut_lib:analyze_module_attributes(exported_macro, Module),
    Insert =
        fun(FAs, Opts, Acc0) ->
            lists:foldl(
                fun({Function, Arity}, Acc1) ->
                    maps:put({Function, Arity}, Opts, Acc1)
                end,
                Acc0,
                FAs
            )
        end,
    lists:foldl(
        fun
            ({FAs, Opts}, Acc) -> Insert(FAs, Opts, Acc);
            (FAs, Acc) when is_list(FAs) -> Insert(FAs, #{}, Acc);
            (FA, Acc) -> Insert([FA], #{}, Acc)
        end,
        #{},
        lists:flatten(ModuleMacroAttributes)
    ).

%%%===================================================================
%%% Validation and checked updates
%%%===================================================================

validate_macro_attribute(Fun, Validator, AttrName, Attr) ->
    case Fun(Attr) of
        invalid_attr ->
            astranaut_return:error_fail(
                {invalid_attr, AttrName, Attr}
            );
        {MFAs, Options} ->
            do([
                return
             || validate_mfas(MFAs),
                Options1 <- astranaut_lib:validate(Validator, Options),
                return({MFAs, Options1})
            ])
    end.

use_macro_validator() ->
    #{
        debug => boolean,
        debug_ast => boolean,
        alias => atom,
        force_override => boolean
    }.

global_macro_validator() ->
    #{
        debug => boolean,
        debug_ast => boolean,
        debug_module => boolean,
        debug_module_ast => boolean,
        max_depth => [uinteger, {default, 100}]
    }.

global_macro_update_validator() ->
    #{
        debug => boolean,
        debug_ast => boolean,
        debug_module => boolean,
        debug_module_ast => boolean,
        max_depth => uinteger
    }.

common_macro_definition_validator() ->
    #{
        as_attr => atom,
        order => {one_of, [outer, inner]},
        inject_attrs => {'or', [atom, {list_of, atom}]},
        group_args => boolean,
        force_override => boolean,
        max_depth => uinteger
    }.

export_macro_validator() ->
    common_macro_definition_validator().

validate_mfas({Module, FAs}) when is_atom(Module) ->
    validate_fas(FAs);
validate_mfas(FAs) when is_list(FAs) ->
    validate_fas(FAs).

validate_fas([{Function, Arity} | T]) when
    is_atom(Function), is_integer(Arity), Arity >= 0
->
    validate_fas(T);
validate_fas([FA | _T]) ->
    astranaut_return:error_fail(
        {invalid_function_with_arity, FA}
    );
validate_fas([]) ->
    astranaut_return:return(ok).

macro_attr({Module, FAs}) when is_atom(Module), is_list(FAs) ->
    {{Module, FAs}, []};
macro_attr({Module, FA}) when is_atom(Module), not is_integer(FA) ->
    {{Module, [FA]}, []};
macro_attr({Module, FAs, Options}) when
    is_atom(Module), is_list(FAs)
->
    {{Module, FAs}, Options};
macro_attr({Module, FA, Options}) when is_atom(Module) ->
    {{Module, [FA]}, Options};
macro_attr(Attr) ->
    macro_without_module_attr(Attr).

macro_without_module_attr({FA}) ->
    {[FA], []};
macro_without_module_attr({FAs, Options}) when is_list(FAs) ->
    {FAs, Options};
macro_without_module_attr({Function, Arity}) when is_integer(Arity) ->
    {[{Function, Arity}], []};
macro_without_module_attr({FA, Options}) ->
    {[FA], Options};
macro_without_module_attr(FAs) when is_list(FAs) ->
    {FAs, []};
macro_without_module_attr(_Other) ->
    invalid_attr.

merge_macro_maps(First, Second) ->
    case merge_macro_maps_pure(First, Second) of
        {ok, Merged} ->
            astranaut_return:return(Merged);
        {error, {macro_override, MacroKey, ExistingMacro, OverridingMacro}} ->
            macro_override_fail(
                MacroKey, ExistingMacro, OverridingMacro
            )
    end.

merge_macro_maps_pure(First, Second) ->
    merge_macro_maps_pure_loop(maps:to_list(Second), First).

merge_macro_maps_pure_loop([{MacroKey, Macro} | T], Acc) ->
    case maps:find(MacroKey, Acc) of
        {ok, ExistingMacro} ->
            case ExistingMacro =:= Macro of
                true ->
                    merge_macro_maps_pure_loop(T, Acc);
                false ->
                    case maps:get(force_override, Macro, false) of
                        true ->
                            merge_macro_maps_pure_loop(
                                T, maps:put(MacroKey, Macro, Acc)
                            );
                        false ->
                            {error, {macro_override, MacroKey, ExistingMacro, Macro}}
                    end
            end;
        error ->
            merge_macro_maps_pure_loop(
                T, maps:put(MacroKey, Macro, Acc)
            )
    end;
merge_macro_maps_pure_loop([], Acc) ->
    {ok, Acc}.

macro_override_fail(MacroKey, ExistingMacro, OverridingMacro) ->
    astranaut_return:error_fail(
        {macro_override, MacroKey, ExistingMacro, OverridingMacro}
    ).

function_clauses_map(
    [{function, _Pos, Name, Arity, Clauses} | T], Acc
) ->
    function_clauses_map(
        T, maps:put({Name, Arity}, Clauses, Acc)
    );
function_clauses_map([_H | T], Acc) ->
    function_clauses_map(T, Acc);
function_clauses_map([], Acc) ->
    Acc.
