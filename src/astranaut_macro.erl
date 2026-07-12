%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2018, Chen Slepher
%%% @end
%%% Created : 18 Nov 2018 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------

%% @doc Parse transform for Astranaut macros.
%%
%% Include `macro.hrl' in a module to enable this transformer.
%%
%% Supported module attributes:
%% <ul>
%% <li>`-import_macro(ModuleOrSpec).' imports macros exported by another module.</li>
%% <li>`-use_macro(UseSpec).' selects imported or local macros and may add use-site options such as `alias' or `force_override'.</li>
%% <li>`-macro_options(Options).' updates global macro options for later macro imports and expansion.</li>
%% <li>`-export_macro(MacroSpec).' exports local functions as macros for other modules.</li>
%% <li>`-local_macro(MacroSpec).' declares local functions as macros without exporting them.</li>
%% <li>`-exec_macro(Call).' expands a macro call in attribute position.</li>
%% </ul>
%%
%% `MacroSpec' accepts a function, a function list, or either form paired with
%% definition options. Definition options include `order', `as_attr',
%% `inject_attrs', `group_args', `extra_functions', and `internal_function'.
%% `extra_functions' explicitly adds helper functions to a local macro closure.
%% `internal_function' controls which functions in a macro definition closure are
%% treated as direct internal calls instead of macro calls.
%%
%% Expansion uses an ordered scan followed by final body traversal:
%% <ol>
%% <li>Unified attribute scan. External and ready local attribute macros are
%% processed left-to-right; generated forms are spliced at the current queue
%% position. Import, use and option forms affect only later forms.</li>
%% <li>Local macro declarations are registered with `astranaut_local_macro'
%% using the materialised source view. First local attribute use loads its
%% callable generation without leaving the scan.</li>
%% <li>Local-macro finalization supplies the final skip set, then function
%% bodies are recursively expanded with the completed macro environment.</li>
%% </ol>
%%
%% The final forms are sorted before returning to the compiler so generated
%% attributes remain in Erlang-valid form order.
%% @end

-module(astranaut_macro).

-include("do.hrl").
-include("stacktrace.hrl").

%% API
-export([parse_transform/2, format_error/1]).
%%%===================================================================
%%% API
%%%===================================================================
-spec parse_transform(astranaut:forms(), compile:option()) -> astranaut:parse_transform_return().
parse_transform(Forms, Options) ->
    astranaut_return:to_compiler(
      do([ return ||
             Module = astranaut_lib:analyze_forms_module(Forms),
             File = astranaut_lib:analyze_forms_file(Forms),
             GlobalMacroOpts0 <- astranaut_lib:validate(global_macro_validator(), []),
             {AttributeForms, FunctionEnv} <-
                 run_attribute_pass(Module, File, GlobalMacroOpts0, Forms, Options),
             FunctionForms <- run_function_macro_pass(AttributeForms, FunctionEnv),
             format_forms(FunctionForms, maps:get(global_macro_opts, FunctionEnv)),
             return(FunctionForms)
         ])).

-spec format_error(term()) -> term().
format_error({import_macro_failed, Module}) ->
    io_lib:format("could not import macro from module ~p, update compile file order in Makefile or add to erl_first_files in rebar.config to make it compile first.", [Module]);
format_error({invalid_import_macro_attr, Macro}) ->
    io_lib:format("~p is not a valid module in -import_macro(~p). ", [Macro, Macro]);
format_error({unimported_macro_module, Module}) ->
    io_lib:format("-import_macro(~p). is required before use of -use_macro", [Module]);
format_error({unexported_macro, Module, Function, Arity}) ->
    io_lib:format("unexported macro ~p:~p/~p.", [Module, Function, Arity]);
format_error({undefined_macro, Function, Arity}) ->
    io_lib:format("macro ~p/~p undefined.", [Function, Arity]);
format_error({invalid_use_macro, Opts}) ->
    io_lib:format("invalid use macro ~p.", [Opts]);
format_error({macro_override, MacroKey, ExistingMacro, OverridingMacro}) ->
    io_lib:format("macro ~p is already defined by ~p and cannot be overridden by ~p without force_override.",
                  [MacroKey, format_macro_ref(ExistingMacro), format_macro_ref(OverridingMacro)]);
format_error({non_exported_formatter, Module}) ->
    io_lib:format("format_error/1 is not exported from module ~p.", [Module]);
format_error({unloaded_formatter_module, Module}) ->
    io_lib:format("formatter module ~p could not be loaded.", [Module]);
format_error(invalid_macro_attribute) ->
    io_lib:format("invalid attribute macro call: macro not found", []);
format_error({max_macro_expansion_depth_exceeded, {MacroModule, Function}, Arguments}) ->
    io_lib:format("maximum macro expansion depth exceeded when applying macro ~p:~p with arguments ~p.",
        [MacroModule, Function, Arguments]);
format_error({max_macro_expansion_depth_exceeded, Function, Arguments}) ->
    io_lib:format("maximum macro expansion depth exceeded when applying macro ~p with arguments ~p.",
        [Function, Arguments]);
format_error({macro_exception, MFA, Arguments, Exception}) ->
    io_lib:format("apply macro ~s ~p failed:~n~s",
                  [format_mfa(MFA), Arguments, eunit_lib:format_exception(Exception)]);
format_error({invalid_macro_return, Detail}) ->
    io_lib:format("macro ~s returned invalid AST: ~p",
                  [format_mfa(invalid_macro_return_mfa(Detail)), Detail]);
format_error({invalid_extra_functions, Functions}) ->
    io_lib:format("extra_functions contains undefined functions: ~p", [Functions]);
format_error({duplicate_local_macro_declaration, Function}) ->
    io_lib:format("duplicate local macro declaration for ~p", [Function]);
format_error({conflicting_internal_function_policy, Function, Policies}) ->
    io_lib:format("conflicting internal_function policy for ~p: ~p", [Function, Policies]);
format_error({conflicting_local_macro_closure_environment, FormId}) ->
    io_lib:format("local macro closure has conflicting expansion environments for ~p", [FormId]);
format_error({illegal_locked_form_mutation, Form}) ->
    io_lib:format("local macro expansion modified frozen form: ~p", [Form]);
format_error(local_macro_module_in_use) ->
    io_lib:format("local macro module is in use and cannot be safely replaced", []);
format_error({illegal_macro_environment_mutation, Form}) ->
    io_lib:format("local macro expansion generated illegal macro environment form: ~p", [Form]);
format_error({illegal_local_macro_definition_mutation, Form}) ->
    io_lib:format("local macro expansion modified locked local macro snapshot form: ~p", [Form]);
format_error(Error) ->
    astranaut:format_error(Error).

format_macro_ref(#{macro_module := Module, function := Function, arity := Arity}) ->
    {Module, Function, Arity};
format_macro_ref(Macro) ->
    Macro.

%%%===================================================================
%%% ===== Attribute pass
%%%===================================================================
run_attribute_pass(Module, File, GlobalMacroOpts0, Forms, CompileOpts) ->
    do([ return ||
           {ScannedForms, ExternalEnv} <-
               scan_attribute_forms(Module, File, GlobalMacroOpts0, Forms, CompileOpts),
           #{macro_map := ExternalMacroMap,
             global_macro_opts := GlobalMacroOpts,
             local_macro_state := ScanLocalState,
             scan_local_macro_map := ScanLocalMacroMap,
             scan_local_declarations := ScanLocalDeclarations} = ExternalEnv,
           AttributeForms = drop_local_declarations(
                                 ScannedForms, ScanLocalDeclarations),
           PreparedForms <- prepare_exports(AttributeForms),
           %% export_macro only publishes a macro for import by other modules.
           %% The current module's execution map is defined solely by
           %% local_macro declarations collected during the scan.
           finalize_attribute_macro_pass(Module, GlobalMacroOpts, ExternalMacroMap, ScanLocalMacroMap,
                                         ScanLocalState, PreparedForms, CompileOpts)
       ]).

%%%===================================================================
%%% ===== Attribute pass, substep 1: unified attribute scan and splice =====
%%%===================================================================
scan_attribute_forms(Module, File, GlobalMacroOpts0, Forms, CompileOpts) ->
    InitState = #{global_macro_opts => GlobalMacroOpts0,
                  module_macro_maps => #{},
                  module => Module,
                  file => File,
                  compile_opts => CompileOpts,
                  passed_forms => [],
                  local_macro_state => astranaut_local_macro:new(),
                  scan_local_macro_map => #{},
                  scan_local_declarations => #{},
                  macro_map => #{}},
    astranaut_traverse:run(
      astranaut:map_forms_splice(fun scan_form/1, Forms, #{traverse => none, queue_state => true}),
      ?MODULE, #{node => form, validator => {role, form}}, InitState).

scan_form(Form) ->
    do([ traverse ||
           State <- astranaut_traverse:get(),
           case Form of
               {attribute, _Pos, local_macro, _Attr} -> scan_local_macro(Form, State);
               _ ->
                   case is_external_env_form(Form) of
                       true -> scan_env_form(Form, State);
                       false -> scan_attribute(Form, State)
                   end
           end
       ]).

scan_local_macro({attribute, Pos, local_macro, Attr} = Form,
                               #{local_macro_state := LocalState,
                                 scan_local_macro_map := ScanLocalMap,
                                 scan_local_declarations := ScanLocalDeclarations,
                                 remaining_forms := Queue} = State) ->
    SourceView = astranaut_local_macro:source_view(passed_forms(State), Queue),
    ClauseMap = function_clauses_map(SourceView, #{}),
    do([ traverse ||
           LocalState1 <- register_local_declaration(
                            Attr, Pos, ClauseMap, SourceView,
                            scan_env_snapshot(State), ScanLocalMap,
                            local_macro_ops(), LocalState),
           {FAs, Options} <- astranaut:traverse_return(
                               validate_local_macro_attribute(#{clause_map => ClauseMap}, Attr)),
           ScanLocalMap1 <- astranaut:traverse_return(
                              add_local_macro(State, SourceView, ClauseMap,
                                                             FAs, Options, ScanLocalMap)),
           %% Registration and map construction happen exactly here.  The
           %% later cleanup only removes this already-scanned declaration; it
           %% never validates or registers a local macro again.  Retained local
           %% helpers intentionally remain subject to Erlang unused warnings.
           astranaut_traverse:put(
             note_passed_form(
               Form, State#{local_macro_state => LocalState1,
                            scan_local_macro_map => ScanLocalMap1,
                            scan_local_declarations => maps:put(Form, {Pos, FAs}, ScanLocalDeclarations)})),
           return(Form)
       ]).

add_local_macro(#{module := Module, file := File, global_macro_opts := GlobalOpts,
                                 macro_map := ExternalMap}, SourceView, ClauseMap,
                                 FAs, Options, Existing) ->
    Ctx = #{module => Module, file => File, forms => SourceView,
            global_macro_opts => local_macro_global_opts(Module, ClauseMap, GlobalOpts),
            external_macro_map => ExternalMap, clause_map => ClauseMap},
    New = build_local_macro_map(Ctx, macro_options_by_fa(FAs, Options#{macro_source => local_macro})),
    do([ return ||
           assert_macro_map_no_overrides(New, ExternalMap),
           merge_macro_maps(Existing, New)
       ]).

register_local_declaration(Attr, Pos, ClauseMap, SourceView, ExternalEnv,
                           CandidateLocalMap, MacroOps, LocalState) ->
    %% Declaration preparation remains the source of diagnostics while both
    %% passes coexist.  `run/1' intentionally keeps error_ok diagnostics out
    %% of this scan transaction, so one bad FA cannot roll back prior local
    %% registrations or duplicate its compiler error.
    case astranaut_return:run(
           validate_local_macro_attribute(#{clause_map => ClauseMap}, Attr)) of
        {just, {FAs, Options}} ->
            case astranaut_local_macro:register(
                   FAs, Options, SourceView, ExternalEnv,
                   CandidateLocalMap, MacroOps, LocalState) of
                {ok, LocalState1} -> astranaut_traverse:return(LocalState1);
                {error, Error} -> astranaut_traverse:update_pos(Pos, ?MODULE, astranaut_traverse:fail(Error))
            end;
        nothing ->
            astranaut_traverse:return(LocalState)
    end.

scan_env_snapshot(#{macro_map := MacroMap, global_macro_opts := GlobalOpts}) ->
    #{macro_map => MacroMap, global_macro_opts => GlobalOpts}.

scan_env_form({attribute, _Pos, import_macro, _Attr} = Form, State) ->
    #{global_macro_opts := GlobalMacroOpts} = State,
    case import_macro_form(GlobalMacroOpts, Form) of
        {ok, ModuleMacroMap} ->
            #{module := Module, file := File, macro_map := MacroMap} = State,
            PassedForms = passed_forms(State),
            Effective = effective_module_macro_maps(File, Module, PassedForms, ModuleMacroMap),
            NewMap = uniform_imported_macro_map(Effective),
            case merge_macro_maps_pure(MacroMap, NewMap) of
                {ok, Merged} ->
                    ModuleMacroMaps = maps:merge(maps:get(module_macro_maps, State, #{}), Effective),
                    do([ traverse ||
                           astranaut_traverse:put(State#{module_macro_maps => ModuleMacroMaps,
                                                         macro_map => Merged}),
                           return({splice, []})
                       ]);
                {error, Reason} ->
                    astranaut_traverse:fail(Reason)
            end;
        {error, Error} ->
            astranaut_traverse:fail(Error)
    end;
scan_env_form({attribute, _Pos, use_macro, _Attr} = Form, State) ->
    #{module := Module, file := File, macro_map := MacroMap} = State,
    ImportedMacros = module_macro_maps_from_uniform(MacroMap),
    PassedForms = passed_forms(State),
    do([ traverse ||
           UsedMacros <- astranaut:traverse_return(
                           used_macros(File, Module, ImportedMacros, [Form], PassedForms)),
           NewMap = uniform_imported_macro_map(UsedMacros),
           Merged <- case merge_macro_maps_pure(MacroMap, NewMap) of
                         {ok, MergedMap} ->
                             astranaut_traverse:return(MergedMap);
                         {error, Reason} ->
                             astranaut_traverse:fail(Reason)
                     end,
           ModuleMacroMaps0 = maps:get(module_macro_maps, State, #{}),
           ModuleMacroMaps1 = maps:fold(
                                fun(M, Macros, Acc) ->
                                        M1 = maps:get(M, Acc, #{}),
                                        maps:put(M, maps:merge(M1, Macros), Acc)
                                end, ModuleMacroMaps0, UsedMacros),
           astranaut_traverse:put(State#{module_macro_maps => ModuleMacroMaps1,
                                         macro_map => Merged}),
           return({splice, []})
       ]);
scan_env_form({attribute, _Pos, macro_options, Attr} = Form, State) ->
    #{global_macro_opts := GlobalMacroOpts} = State,
    do([ traverse ||
           MacroOpts <- astranaut:traverse_return(
                          astranaut_lib:validate(global_macro_update_validator(), Attr)),
           astranaut_traverse:put(note_passed_form(
                                    Form, State#{global_macro_opts => maps:merge(GlobalMacroOpts, MacroOpts)})),
           return(Form)
       ]);
scan_env_form(Form, _State) ->
    keep_scanned_form(Form).

scan_attribute(Form, State) ->
    #{macro_map := ExternalMacroMap, scan_local_macro_map := ScanLocalMap} = State,
    MacroMap = maps:merge(ExternalMacroMap, ScanLocalMap),
    ExecutionMacroMap = inject_macro_attributes(MacroMap, passed_forms(State)),
    AttributeMacroMap = attribute_macro_map(ExecutionMacroMap),
    case attribute_find_macro(Form, ExecutionMacroMap, AttributeMacroMap) of
        {ok, Macro} ->
            do([ traverse ||
                   State1 <- ensure_local_attribute_macro(Macro, State),
                   %% Macro lookup depends on the already-passed forms, which
                   %% ensuring availability does not change.  Only the local
                   %% macro module generation changes, so reuse Macro and
                   %% avoid a second, partial match-based lookup.
                   Expanded <- expand_macro(Macro, #{expected_role => form}),
                   ExpandedForms = to_list(Expanded),
                   assert_scan_frozen_forms(ExpandedForms, maps:get(local_macro_state, State1)),
                   return({splice, ExpandedForms})
               ]);
        error ->
            %% `error' means this is syntactically a macro invocation (an
            %% exec_macro or a registered attribute macro) but no executable
            %% macro matched it.  Keep the original form, but diagnose it here;
            %% the final local-attribute pass only passes it through to avoid a
            %% duplicate warning.
            do([ traverse ||
                   _ <- astranaut_traverse:warning(invalid_macro_attribute),
                   keep_scanned_form(Form)
               ]);
        not_macro ->
            keep_scanned_form(Form)
    end.

ensure_local_attribute_macro(
    #{macro_source := local_macro, function := Function, arity := Arity},
    #{local_macro_state := LocalState,
      scan_local_macro_map := LocalMap,
      compile_opts := CompileOpts,
      remaining_forms := Queue} = State) ->
    case maps:find({Function, Arity}, astranaut_local_macro:local_macros(LocalState)) of
        {ok, #{status := compiled}} -> astranaut_traverse:return(State);
        {ok, _} ->
            SourceView = astranaut_local_macro:source_view(passed_forms(State), Queue),
            do([ traverse ||
                   CompileContext = #{local_macro_map => LocalMap,
                                      source_view => SourceView,
                                      compile_opts => CompileOpts},
                   LocalState1 <- astranaut:traverse_return(
                                    astranaut_local_macro:ensure_available(
                                      {Function, Arity}, CompileContext,
                                      local_macro_ops(), LocalState)),
                   State1 = State#{local_macro_state => LocalState1},
                   astranaut_traverse:put(State1),
                   return(State1)
               ]);
        error -> astranaut_traverse:return(State)
    end;
ensure_local_attribute_macro(_Macro, State) -> astranaut_traverse:return(State).

assert_scan_frozen_forms(Forms, LocalState) ->
    case astranaut_local_macro:reject_locked_mutation(Forms, LocalState) of
        ok -> astranaut_traverse:return(ok);
        {error, Error} ->
            Form = hd([F || F <- Forms, scan_locked_form(F, LocalState)]),
            astranaut_traverse:update_pos(form_pos(Form), ?MODULE, astranaut_traverse:fail(Error))
    end.

scan_locked_form(Form, LocalState) ->
    case astranaut_local_macro:form_id(Form) of
        undefined -> false;
        Id -> ordsets:is_element(Id, astranaut_local_macro:frozen_ids(LocalState))
    end.

keep_scanned_form(Form) ->
    do([ traverse ||
           State <- astranaut_traverse:get(),
           astranaut_traverse:put(note_passed_form(Form, State)),
           return(Form)
       ]).

note_passed_form(Form, #{passed_forms := PassedForms} = State) ->
    State#{passed_forms => [Form | PassedForms]}.

passed_forms(#{passed_forms := PassedForms}) ->
    lists:reverse(PassedForms).

is_external_env_form({attribute, _Pos, import_macro, _Attr}) -> true;
is_external_env_form({attribute, _Pos, use_macro, _Attr}) -> true;
is_external_env_form({attribute, _Pos, macro_options, _Attr}) -> true;
is_external_env_form(_Form) -> false.

form_pos({attribute, Pos, _Name, _Value}) ->
    Pos;
form_pos(_Form) ->
    0.

module_macro_maps_from_uniform(MacroMap) ->
    maps:fold(
      fun(_Key, #{macro_module := MacroModule} = Macro, Acc) ->
              ModuleMacroMap = maps:get(MacroModule, Acc, #{}),
              maps:put(MacroModule, maps:put(_Key, Macro, ModuleMacroMap), Acc)
      end, #{}, MacroMap).

import_macro_form(GlobalMacroOpts, {attribute, _Pos, import_macro, Module}) when is_atom(Module) ->
    case is_loaded(Module) of
        {file, _} ->
            Macros = analyze_module_macros(Module),
            Exports = Module:module_info(exports),
            GlobalMacroOpts1 = formatter_opts(Module, Exports, GlobalMacroOpts),
            Macros1 =
                maps:fold(
                  fun({Function, Arity}, MacroOptions, Acc) ->
                          MacroOptions1 = maps:merge(GlobalMacroOpts1, MacroOptions),
                          MacroOptions2 = MacroOptions1#{module => Module,
                                                         macro_module => Module,
                                                         macro => {Module, Function},
                                                         function => Function,
                                                         arity => Arity},
                          maps:put({Function, Arity}, MacroOptions2, Acc)
                  end, #{}, Macros),
            {ok, #{Module => Macros1}};
        false ->
            {error, {import_macro_failed, Module}}
    end;
import_macro_form(_GlobalMacroOpts, {attribute, _Pos, import_macro, Attr}) ->
    {error, {invalid_import_macro_attr, Attr}}.

%%%===================================================================
%%% ===== Attribute pass, substep 2: exported declaration preparation =====
%%%===================================================================
%% Local declarations have already been registered by the unified scan.  They
%% are transformer directives, not compiler directives, so remove them from
%% the output without adding a nowarn_unused_function attribute.
drop_local_declarations(Forms, Declarations) ->
    [Form || Form <- Forms, not maps:is_key(Form, Declarations)].

prepare_exports(Forms) ->
    do([ return ||
           ClausesMap = function_clauses_map(Forms, maps:new()),
           {Forms1, _ExportedMacros} <- exported_macros(Forms, ClausesMap),
           return(Forms1)
       ]).

formatter_opts(Module, Functions, MacroOpts) ->
    FormatError = {format_error, 1},
    case lists:member(FormatError, Functions) of
        true ->
            MacroOpts#{formatter => Module};
        false ->
            MacroOpts#{formatter => astranaut_macro}
    end.

exported_macros(Forms, ClausesMap) ->
    astranaut_lib:forms_with_attribute(
      fun(Attr, Acc, #{pos := Pos}) ->
              do([ return ||
                     Validator = macro_definition_validator(),
                     {FAs, Options} <-
                         validate_macro_attribute(fun macro_without_module_attr/1, Validator, export_macro, Attr),
                     FAs1 <- remove_undefined_macros(FAs, ClausesMap),
                     Options1 <- validate_extra_functions_defined(Options, ClausesMap),
                     case FAs1 of
                         [] ->
                             astranaut_return:return({[], Acc});
                         _ ->
                             %% exported_macro options for external usage
                             ExportedMacroAttribute = astranaut_lib:gen_attribute_node(exported_macro, Pos, [{FAs, Options1}]),
                             ExportAttribute = astranaut_lib:gen_attribute_node(export, Pos, FAs),
                             astranaut_return:return({[ExportAttribute, ExportedMacroAttribute], Acc})
                     end
                 ])
      end, #{}, Forms, export_macro, #{formatter => ?MODULE}).

remove_undefined_macros(FAs, ClausesMap) ->
    astranaut_return:foldl_m(
      fun({Function, Arity} = FA, Acc) ->
              case maps:is_key(FA, ClausesMap) of
                  true ->
                      astranaut_return:return([FA|Acc]);
                  false ->
                      astranaut_return:error_ok({undefined_macro, Function, Arity}, Acc)
              end
      end, [], FAs).

is_loaded(Module) ->
    code:ensure_loaded(Module),
    code:is_loaded(Module).

validate_local_macro_attribute(#{clause_map := ClauseMap}, Attr) ->
    do([ return ||
           Validator = macro_definition_validator(),
           {FAs, Options} <- validate_macro_attribute(fun macro_without_module_attr/1, Validator, local_macro, Attr),
           FAs1 <- remove_undefined_macros(FAs, ClauseMap),
           Options1 <- validate_extra_functions_defined(Options, ClauseMap),
           return({FAs1, Options1})
       ]).

validate_extra_functions_defined(Options, ClauseMap) ->
    Missing = [FA || FA <- maps:get(extra_functions, Options, []),
                     not maps:is_key(FA, ClauseMap)],
    case Missing of
        [] ->
            astranaut_return:return(Options);
        _ ->
            astranaut_return:error_fail({invalid_extra_functions, Missing})
    end.

build_local_macro_map(#{file := File,
                        module := Module,
                        forms := Forms,
                        global_macro_opts := GlobalMacroOpts}, ModuleMacros) ->
    LocalModuleMacros =
        maps:map(
          fun({Function, Arity}, MacroOptions) ->
                  local_macro_options(Module, GlobalMacroOpts, Function, Arity, MacroOptions)
          end, ModuleMacros),
    update_module_macros(File, Module, Forms, LocalModuleMacros).

macro_options_by_fa(FAs, Options) ->
    lists:foldl(fun(FA, Acc) -> maps:put(FA, Options, Acc) end, #{}, FAs).

local_macro_global_opts(Module, ClauseMap, GlobalMacroOpts) ->
    case maps:is_key({format_error, 1}, ClauseMap) of
        true ->
            GlobalMacroOpts#{formatter => astranaut_local_macro:module_name(Module)};
        false ->
            GlobalMacroOpts#{formatter => astranaut_macro}
    end.

local_macro_options(Module, GlobalMacroOpts, Function, Arity, MacroOptions) ->
    MacroOptions1 = maps:merge(GlobalMacroOpts, MacroOptions),
    MacroOptions1#{module => astranaut_local_macro:module_name(Module),
                   macro_module => Module,
                   macro => Function,
                   function => Function,
                   arity => Arity}.

update_module_macros(File, Module, _Forms, ModuleMacros) ->
    maps:fold(
      fun(_MFA, MacroOptions, Acc) ->
              MacroOptions1 = MacroOptions#{file => File, local_module => Module},
              MacroOptions2 = update_as_attr(MacroOptions1),
              #{macro := Macro, call_arity := CallArity} = MacroOptions3 = update_call_arity(MacroOptions2),
              maps:put({Macro, CallArity}, MacroOptions3, Acc)
      end, #{}, ModuleMacros).

%% Injection belongs to the macro invocation, not macro import. Attribute
%% macros use the forms already passed by their scan; function macros use the
%% complete form list in the final expansion phase.
inject_macro_attributes(MacroMap, Forms) ->
    maps:map(fun(_MacroKey, Macro) -> inject_attrs(Macro, Forms) end, MacroMap).

used_macros(File, Module, ImportedMacros, Forms, MacroForms) ->
    ImportedMacroMap = effective_module_macro_maps(File, Module, MacroForms, ImportedMacros),
    astranaut_lib:with_attribute(
      fun(Attr, UsedMacroMapAcc) ->
              do([ return ||
                     Validator = use_macro_validator(),
                     {MFAs, Options}
                         <- validate_macro_attribute(fun macro_attr/1, Validator, use_macro, Attr),
                     case MFAs of
                         {ImportedModule, FAs} ->
                             case maps:is_key(ImportedModule, UsedMacroMapAcc) of
                                 true ->
                                     update_used_macro_maps(
                                       File, Module, MacroForms, ImportedModule, FAs, Options,
                                       UsedMacroMapAcc,
                                       fun({Function, Arity}) ->
                                               {unexported_macro, ImportedModule, Function, Arity}
                                       end);
                                 false ->
                                     astranaut_return:error_fail({unimported_macro_module, ImportedModule})
                             end;
                         FAs ->
                             update_used_macro_maps(
                               File, Module, MacroForms, Module, FAs, Options,
                               UsedMacroMapAcc,
                               fun({Function, Arity}) ->
                                       {undefined_macro, Function, Arity}
                               end)
                     end
                 ])
      end, ImportedMacroMap, Forms, use_macro, #{formatter => ?MODULE}).

effective_module_macro_maps(File, Module, Forms, ModuleMacros) ->
    maps:map(
      fun(_MacroModule, Macros) ->
              update_module_macros(File, Module, Forms, Macros)
      end, ModuleMacros).

update_used_macro_maps(File, Module, Forms, MacroModule, FAs, UsedMacroOptions, UsedMacroMapAcc, MissingFun) ->
    astranaut_return:foldl_m(
      fun(FA, Acc) ->
              ModuleMacroMap = maps:get(MacroModule, Acc, #{}),
              case find_used_macro(FA, ModuleMacroMap) of
                  {ok, MacroKey, MacroOptions} ->
                      MacroOptions1 = maps:merge(MacroOptions, UsedMacroOptions),
                      MacroOptions2 = update_alias(MacroOptions1),
                      CurrentMacroMap = update_module_macros(File, Module, Forms, #{FA => MacroOptions2}),
                      ModuleMacroMapWithoutCurrent = maps:remove(MacroKey, ModuleMacroMap),
                      ExistingUsedMacroMap = maps:put(MacroModule, ModuleMacroMapWithoutCurrent, Acc),
                      ExistingMacroMap = uniform_imported_macro_map(ExistingUsedMacroMap),
                      do([ return ||
                             assert_macro_map_no_overrides(CurrentMacroMap, ExistingMacroMap),
                             ModuleMacroMap1 = maps:merge(ModuleMacroMapWithoutCurrent, CurrentMacroMap),
                             return(maps:put(MacroModule, ModuleMacroMap1, Acc))
                         ]);
                  error ->
                      astranaut_return:error_ok(MissingFun(FA), Acc)
              end
      end, UsedMacroMapAcc, FAs).

find_used_macro({Function, Arity}, ModuleMacroMap) ->
    maps:fold(
      fun(MacroKey, #{function := Function1, arity := Arity1} = MacroOptions, error)
            when Function =:= Function1, Arity =:= Arity1 ->
              {ok, MacroKey, MacroOptions};
         (_MacroKey, _MacroOptions, Acc) ->
              Acc
      end, error, ModuleMacroMap).

assert_macro_map_no_overrides(MacroMap, ExistingMacroMap) ->
    astranaut_return:foldl_m(
      fun({MacroKey, Macro}, ExistingMacroMapAcc) ->
              case maps:find(MacroKey, ExistingMacroMapAcc) of
                  {ok, ExistingMacro} ->
                      case maps:get(force_override, Macro, false) of
                          true ->
                              astranaut_return:return(maps:put(MacroKey, Macro, ExistingMacroMapAcc));
                          false ->
                              macro_override_fail(MacroKey, ExistingMacro, Macro)
                      end;
                  error ->
                      astranaut_return:return(maps:put(MacroKey, Macro, ExistingMacroMapAcc))
              end
      end, ExistingMacroMap, maps:to_list(MacroMap)).

uniform_imported_macro_map(UsedMacroMap) ->
    maps:fold(
      fun(_MacroModule, MacroMap, Acc) ->
              maps:merge(Acc, MacroMap)
      end, #{}, UsedMacroMap).

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

inject_attrs(#{inject_attrs := true} = Options, Forms) ->
    inject_attrs(Options#{inject_attrs => []}, Forms);
inject_attrs(#{inject_attrs := Attr} = Options, Forms) when is_atom(Attr) ->
    inject_attrs(Options#{inject_attrs => [Attr]}, Forms);
inject_attrs(#{inject_attrs := Attrs, file := File, local_module := Module} = Opts, Forms) when is_list(Attrs) ->
    AttributesMap =
        lists:foldl(
          fun(module, Acc) ->
                  Acc;
             (file, Acc) ->
                  Acc;
             (pos, Acc) ->
                  Acc;
             (Attr, Acc) ->
                  Attributes = astranaut_lib:analyze_forms_attributes(Attr, Forms),
                  maps:put(Attr, Attributes, Acc)
          end, maps:new(), Attrs),
    Opts#{attributes => maps:merge(#{file => File, module => Module}, AttributesMap)};
inject_attrs(#{} = Opts, _Forms) ->
    Opts.

update_call_arity(Opts) ->
    CallArity = call_arity(Opts),
    Opts#{call_arity => CallArity}.

call_arity(#{group_args := true} = Opts) ->
    call_arity(maps:remove(group_args, Opts#{arity => 1}));
call_arity(#{arity := Arity} = Opts) ->
    case maps:get(inject_attrs, Opts, false) of
        false ->
            Arity;
        _ ->
            Arity - 1
    end.

analyze_module_macros(Module) ->
    ModuleMacroAttributes = astranaut_lib:analyze_module_attributes(exported_macro, Module),
    Insert =
        fun(FAs, Opts, Acc0) ->
                lists:foldl(
                  fun({Function, Arity}, Acc1) ->
                          maps:put({Function, Arity}, Opts, Acc1)
                  end, Acc0, FAs)
        end,
    lists:foldl(
      fun({FAs, Opts}, Acc) ->
              Insert(FAs, Opts, Acc);
         (FAs, Acc) when is_list(FAs) ->
              Insert(FAs, #{}, Acc);
         (FA, Acc) ->
              Insert([FA], #{}, Acc)
      end, #{}, lists:flatten(ModuleMacroAttributes)).

validate_macro_attribute(Fun, Validator, AttrName, Attr) ->
    case Fun(Attr) of
        invalid_attr ->
            astranaut_return:error_fail({invalid_attr, AttrName, Attr});
        {MFAs, Options} ->
            do([ return ||
                   validate_mfas(MFAs),
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

macro_definition_validator() ->
    #{as_attr => atom,
      order => {one_of, [outer, inner]},
      inject_attrs => {'or', [atom, {list_of, atom}]},
      group_args => boolean,
      force_override => boolean,
      max_depth => uinteger,
      extra_functions => {list_of, fun validate_function_with_arity/1},
      internal_function => fun validate_internal_function_policy/1
     }.

validate_function_with_arity({Function, Arity} = FA) when is_atom(Function), is_integer(Arity), Arity >= 0 ->
    {ok, FA};
validate_function_with_arity(FA) ->
    {error, {invalid_function_with_arity, FA}}.

validate_internal_function_policy(Value) when is_boolean(Value) ->
    {ok, Value};
validate_internal_function_policy(Functions) when is_list(Functions) ->
    astranaut_return:foldl_m(
      fun(Function, Acc) ->
              case validate_function_with_arity(Function) of
                  {ok, Function1} ->
                      astranaut_return:return([Function1|Acc]);
                  {error, Error} ->
                      astranaut_return:error(Error)
              end
      end, [], lists:reverse(Functions));
validate_internal_function_policy(Value) ->
    {error, {invalid_internal_function, Value}}.

validate_mfas({Module, FAs}) when is_atom(Module) ->
    validate_fas(FAs);
validate_mfas(FAs) when is_list(FAs) ->
    validate_fas(FAs).

validate_fas([{Function, Arity}|T]) when is_atom(Function), is_integer(Arity), Arity >= 0 ->
    validate_fas(T);
validate_fas([FA|_T]) ->
    astranaut_return:error_fail({invalid_function_with_arity, FA});
validate_fas([]) ->
    astranaut_return:return(ok).

macro_attr({Module, FAs}) when is_atom(Module), is_list(FAs) ->
    {{Module, FAs}, []};
macro_attr({Module, FA}) when is_atom(Module), not is_integer(FA) ->
    {{Module, [FA]}, []};
macro_attr({Module, FAs, Options}) when is_atom(Module), is_list(FAs) ->
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

%%%===================================================================
%%% ===== Attribute pass, substep 3: local macro closure and snapshots =====
%%%===================================================================
finalize_attribute_macro_pass(Module, GlobalMacroOpts, ExternalMacroMap, LocalMacroMap,
                              ScanLocalState, Forms, CompileOpts) ->
    do([ return ||
           FinalContext = #{local_macro_map => LocalMacroMap,
                            external_macro_map => ExternalMacroMap,
                            source_view => Forms,
                            compile_opts => CompileOpts},
           {FinalLocalEnv, FinalSkipIds, RetainedForms, _FinalLocalState} <-
               astranaut_local_macro:finalize(
                 retain_roots(Forms), FinalContext,
                 local_macro_ops(), ScanLocalState),
           Forms1 = astranaut_local_macro:materialize_forms(Forms, RetainedForms),
           Ctx0 = uniform_macro_context(Module, Forms1),
           Ctx = Ctx0#{prepared_function_ids => ordsets:from_list(
                                                [Id || Id = {function, _, _} <-
                                                           maps:keys(RetainedForms)])},
           FinalLocalMacroMap = compiled_local_macro_map(LocalMacroMap, FinalLocalEnv),
           FinalMacroMap <- merge_macro_maps(ExternalMacroMap, FinalLocalMacroMap),
           {UnsortedAttributeForms, FunctionEnv0} <-
               finalize_attribute_forms(Ctx, FinalMacroMap, FinalSkipIds),
           %% The attribute-pass output is sorted before the function pass sees it.
           AttributeForms = astranaut_syntax:sort_forms(UnsortedAttributeForms),
           FunctionEnv = FunctionEnv0#{global_macro_opts => GlobalMacroOpts},
           return({AttributeForms, FunctionEnv})
       ]).

%% The function pass must only see local macros that are actually compiled in
%% the final generation; declarations that failed compilation or were not yet
%% available are dropped from the macro map.
compiled_local_macro_map(LocalMacroMap, FinalLocalEnv) ->
    CompiledFAs = ordsets:from_list(maps:keys(FinalLocalEnv)),
    maps:filter(
      fun(_Key, #{function := Function, arity := Arity}) ->
              ordsets:is_element({Function, Arity}, CompiledFAs)
      end, LocalMacroMap).

uniform_macro_context(Module, Forms) ->
    #{module => Module, forms => Forms}.

finalize_attribute_forms(
  #{module := Module,
    forms := Forms} = Ctx, FinalMacroMap, FinalSkipIds) ->
    do([ return ||
           %% Attribute macros are expanded exclusively by scan_attribute_forms/5.
           %% Re-running them here would let later declarations backscan forms
           %% that were already passed under an earlier environment.
           Forms2 = remove_final_skip_forms(Forms, FinalSkipIds),
           FunctionMacroMap = inject_macro_attributes(FinalMacroMap, Forms2),
           PreparedFunctionIds = maps:get(prepared_function_ids, Ctx, ordsets:new()),
           FinalMacroCallers = find_function_macro_callers(
                                 Forms2, FunctionMacroMap, PreparedFunctionIds),
           FinalSkipFunctionIds = ordsets:from_list([{function, Name, Arity}
                                                      || {function, Name, Arity} <- FinalSkipIds]),
           FunctionEnv = #{module => Module,
                           macro_map => FunctionMacroMap,
                           callers => ordsets:subtract(FinalMacroCallers, FinalSkipFunctionIds)},
           return({Forms2, FunctionEnv})
       ]).

run_function_macro_pass(Forms, #{macro_map := MacroMap, callers := Callers}) ->
    expand_functions(
      MacroMap, Forms,
      [{Name, Arity} || {function, Name, Arity} <- Callers]).

remove_final_skip_forms(Forms, FinalSkipIds) ->
    Skip = ordsets:from_list(FinalSkipIds),
    lists:flatmap(fun(Form) -> remove_final_skip_form(Form, Skip) end, Forms).

remove_final_skip_form({attribute, Pos, compile, {nowarn_unused_function, FAs}}, Skip) ->
    RemainingFAs = [FA || FA = {Name, Arity} <- FAs,
                          not ordsets:is_element({function, Name, Arity}, Skip)],
    case RemainingFAs of
        [] ->
            [];
        _ ->
            [{attribute, Pos, compile, {nowarn_unused_function, RemainingFAs}}]
    end;
remove_final_skip_form(Form, Skip) ->
    case ordsets:is_element(astranaut_local_macro:form_id(Form), Skip) of
        true -> [];
        false -> [Form]
    end.

merge_macro_maps(First, Second) ->
    astranaut_return:foldl_m(
      fun({MacroKey, Macro}, Acc) ->
              case maps:find(MacroKey, Acc) of
                  {ok, ExistingMacro} ->
                      case ExistingMacro =:= Macro of
                          true ->
                              astranaut_return:return(Acc);
                          false ->
                              case maps:get(force_override, Macro, false) of
                                  true ->
                                      astranaut_return:return(maps:put(MacroKey, Macro, Acc));
                                  false ->
                                      macro_override_fail(MacroKey, ExistingMacro, Macro)
                              end
                      end;
                  error ->
                      astranaut_return:return(maps:put(MacroKey, Macro, Acc))
              end
      end, First, maps:to_list(Second)).

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
                            merge_macro_maps_pure_loop(T, maps:put(MacroKey, Macro, Acc));
                        false ->
                            {error, {macro_override, MacroKey, ExistingMacro, Macro}}
                    end
            end;
        error ->
            merge_macro_maps_pure_loop(T, maps:put(MacroKey, Macro, Acc))
    end;
merge_macro_maps_pure_loop([], Acc) ->
    {ok, Acc}.

macro_override_fail(MacroKey, ExistingMacro, OverridingMacro) ->
    astranaut_return:error_fail({macro_override, MacroKey, ExistingMacro, OverridingMacro}).

attribute_macro_map(MacroMap) ->
    AttributeMap =
        maps:fold(
          fun({_Function, Arity}, #{as_attr := Attr} = Macro, Acc) ->
                  maps:put({Attr, Arity}, Macro, Acc);
             (_Key, _Macro, Acc) ->
                  Acc
          end, #{}, MacroMap),
    maps:fold(
      fun({Name, Arity}, Macro, Acc) ->
              MacroNameMap = maps:get(Name, Acc, #{}),
              MacroNameMap1 = maps:put({Name, Arity}, Macro, MacroNameMap),
              maps:put(Name, MacroNameMap1, Acc)
      end, #{}, AttributeMap).

%%%===================================================================
%%% ===== Shared function expansion and local-macro operations =====
%%%===================================================================

local_macro_ops() ->
    #{resolve_local_references => fun resolve_local_references/2,
      expand_function => fun expand_function/4,
      merge_macro_maps => fun merge_macro_maps/2}.

%% Both local-macro compilation and the final ordinary function pass use this
%% operation. The caller supplies the complete effective MacroEnv; this code
%% contains no local-macro policy.
expand_function(MacroEnv, InjectForms, Forms, TargetFA) ->
    ExecutionEnv = inject_macro_attributes(MacroEnv, InjectForms),
    expand_functions(ExecutionEnv, Forms, [TargetFA]).

expand_functions(MacroEnv, Forms, TargetFAs) ->
    MacroCallers = find_function_macro_callers(Forms, MacroEnv, ordsets:new()),
    TargetIds = function_ids(TargetFAs),
    TransformIds = ordsets:intersection(MacroCallers, TargetIds),
    transform_functions_if_needed(uniform, MacroEnv, Forms, TransformIds).

%% TargetEnvs is prepared by astranaut_local_macro. Each target already has
%% internal_function and its own FA removed from the candidate local env.
resolve_local_references(TargetEnvs, Forms) ->
    lists:foldl(
      fun({TargetFA, CandidateEnv}, Acc) ->
              ordsets:union(referenced_local_fas(TargetFA, CandidateEnv, Forms), Acc)
      end, ordsets:new(), TargetEnvs).

referenced_local_fas({Name, Arity}, CandidateEnv, Forms) ->
    case [Clauses || {function, _Pos, Name0, Arity0, Clauses} <- Forms,
                     Name0 =:= Name, Arity0 =:= Arity] of
        [Clauses | _] ->
            astranaut:sreduce(
              fun(Node, Acc) ->
                      case call_find_macro(uniform, Node, CandidateEnv) of
                          {ok, #{macro_source := local_macro,
                                 function := Function, arity := MacroArity}} ->
                              ordsets:add_element({Function, MacroArity}, Acc);
                          _ -> Acc
                      end
              end, ordsets:new(), Clauses, #{traverse => pre});
        [] ->
            ordsets:new()
    end.

retain_roots(Forms) ->
    lists:foldl(
      fun({attribute, _Pos, local_macro_retain, Attr}, Acc) -> retain_fas(Attr, Acc);
         ({attribute, _Pos, export_macro, Attr}, Acc) -> retain_fas(Attr, Acc);
         ({attribute, _Pos, export, Attr}, Acc) -> retain_fas(Attr, Acc);
         (_Form, Acc) -> Acc
      end, [], Forms).

retain_fas({FAs, _Options}, Acc) when is_list(FAs) -> retain_fas(FAs, Acc);
retain_fas(FAs, Acc) when is_list(FAs) ->
    [FA || FA = {Name, Arity} <- FAs, is_atom(Name), is_integer(Arity)] ++ Acc;
retain_fas({Name, Arity} = FA, Acc) when is_atom(Name), is_integer(Arity) -> [FA | Acc];
retain_fas(_Other, Acc) -> Acc.

function_ids(Functions) ->
    lists:foldl(
      fun({Function, Arity}, Acc) ->
              ordsets:add_element({function, Function, Arity}, Acc)
      end, ordsets:new(), Functions).

transform_functions_if_needed(_Module, MacroMap, Forms, _TransformFunctions) when map_size(MacroMap) =:= 0 ->
    astranaut_return:return(Forms);
transform_functions_if_needed(_Module, _MacroMap, Forms, []) ->
    astranaut_return:return(Forms);
transform_functions_if_needed(Module, MacroMap, Forms, TransformFunctions) ->
    transform_functions(Module, MacroMap, Forms, TransformFunctions).

%%%===================================================================
%%% ===== Function pass: function body macro expansion =====
%%%===================================================================
-spec transform_functions(module(), map(), [astranaut:form()], all | {except, list()} | list()) -> term().
transform_functions(Module, MacroMap, Forms, TransformFunctions) ->
    RecordForms = record_forms(Forms),
    %% just traverse function clauses, other nodes return directly
    FunctionClausesUniplate = 
        fun({function, Pos, Name, Arity, Clauses}) ->
                { [Clauses], fun([NewClauses]) -> {function, Pos, Name, Arity, NewClauses} end };
            (Node) -> 
                {[[]], fun(_) -> Node end }
        end,
    Monad =
        astranaut:map_m(
          fun({function, _Pos, Name, Arity, _Clauses} = Function) ->
                  case should_transform_function(Name, Arity, TransformFunctions) of
                      false ->
                          astranaut_traverse:return(Function);
                      true ->
                          astranaut:map_m(
                            fun(Clause) ->
                                    transform_clause(Module, MacroMap, Clause, RecordForms)
                            end, Function, #{traverse => subtree, uniplate => FunctionClausesUniplate})
                  end;
             (Form) ->
                  astranaut_traverse:return(Form)
          end, Forms, #{traverse => none}),
    astranaut_traverse:eval(Monad, ?MODULE, #{}, 0).
transform_clause(Module, MacroMap, {clause, Pos, Patterns, Guards, Exprs}, RecordForms) ->
    do([ traverse ||
           %% counter reseted in every function clause
           astranaut_traverse:put(1),
           Guards1 <- transform_exprs(Module, MacroMap, Guards, #{depth => 0, expected_role => guard,
                                                                   forms => RecordForms}),
           Exprs1 <- transform_exprs(Module, MacroMap, Exprs, #{depth => 0, expected_role => expression,
                                                                 forms => RecordForms}),
           return({clause, Pos, Patterns, Guards1, Exprs1})
    ]).

record_forms(Forms) ->
    [Form || {attribute, _Anno, record, {_Name, _Fields}} = Form <- Forms].

transform_exprs(Module, MacroMap, Exprs, DepthOpts) ->
    ExpectedRole = maps:get(expected_role, DepthOpts, expression),
    InitAttr = #{node => ExpectedRole,
                 validator => {role, ExpectedRole}},
    Monad = astranaut:map_m(
        fun(Node) ->
            do([ traverse ||
                Attr = #{step := Step} <- astranaut_traverse:ask(),
                DepthOpts1 = DepthOpts#{rename_quoted_variables => true, step => Step,
                                        attr => Attr},
                case match_macro_call(Module, Node, MacroMap, Step) of
                    {ok, Macro} ->
                        expand_macro_recursive(Module, MacroMap, Macro, DepthOpts1);
                    error ->
                        astranaut_traverse:return(Node)
                end
            ])
        end, Exprs, #{traverse => all, normalize => false}),
    astranaut_traverse:local(fun(_) -> InitAttr end, Monad).

%%%===================================================================
%%% ===== Macro call lookup and invocation =====
%%%===================================================================
expand_macro_recursive(_Module, _MacroMap, #{ max_depth := MaxDepth } = Macro,
    #{depth := Depth} = DepthOpts) when Depth >= MaxDepth ->
    CurrentMacro = maps:get(origin_macro, DepthOpts, Macro),
    recover_macro_call(
      Macro,
      astranaut_traverse:fail(
        {max_macro_expansion_depth_exceeded,
         maps:get(macro, CurrentMacro),
         maps:get(arguments, CurrentMacro, [])}));
expand_macro_recursive(Module, MacroMap, Macro, #{step := post } = DepthOpts) ->
    DepthOpts1 = update_depth_opts(Macro, DepthOpts),
    expand_macro_with(
      Macro, DepthOpts1,
      fun(Node1) -> transform_exprs(Module, MacroMap, Node1, DepthOpts1) end);
expand_macro_recursive(_Module, _MacroMap, Macro, #{step := pre } = DepthOpts) ->
    DepthOpts1 = update_depth_opts(Macro, DepthOpts),
    %% A pre return is checked locally here.  Its children remain in the
    %% surrounding traversal and are expanded at their own pre/post steps.
    expand_macro(Macro, DepthOpts1).

match_macro_call(Module, Node, Macros, Step) ->
    case call_find_macro(Module, Node, Macros) of
        {ok, Macro} ->
            case match_macro_order(Macro, Step) of
                true ->
                    {ok, Macro};
                false ->
                    error
            end;
        error ->
            error
    end.

call_find_macro(_Module, {call, Pos1, {atom, _Pos2, Function}, Arguments} = Node, Macros) ->
    find_macro_with_arguments(Function, Arguments, Pos1, Macros, Node);
call_find_macro(_Module, {call, Pos1, {remote, _Pos2, {atom, _Pos3, RemoteModule}, {atom, _Pos4, Function}}, Arguments} = Node,
                Macros) ->
    find_macro_with_arguments({RemoteModule, Function}, Arguments, Pos1, Macros, Node);
call_find_macro(_Module, _Node, _Macros) ->
    error.

match_macro_order(Macro, Step) ->
    Order = maps:get(order, Macro, inner),
    ((Order =:= inner) and (Step =:= post))
        or ((Order =:= outer) and (Step =:= pre)).

update_depth_opts(Macro, #{depth := Depth} = Opts) ->
    Opts1 = update_macro_context(Macro, Opts),
    Opts1#{depth => Depth + 1}.

update_macro_context(Macro, #{depth := 0} = Opts) ->
    Opts#{origin_macro => Macro, current_macro => Macro};
update_macro_context(Macro, Opts) ->
    Opts#{current_macro => Macro}.

expand_macro(Macro, Opts) ->
    expand_macro_with(Macro, Opts, fun astranaut_traverse:return/1).

expand_macro_with(#{pos := Pos, formatter := Formatter} = Macro, Opts, Success) ->
    recover_macro_call(
      Macro,
      do([ traverse ||
             %% A user macro may return a traverse computation.  Run only that
             %% computation in private State; framework work below, including
             %% quoted-variable numbering, remains in the caller's State.
             Node <- astranaut_traverse:update_pos(
                       Pos, Formatter, invoke_macro_function(Macro)),
             %% Validate the returned tree while traversing it once for
             %% structure-preserving variable and position updates.
             Node1 <- process_macro_return(Node, Macro, Opts),
             format_node(Node1, Macro),
             Success(Node1)
         ])).

recover_macro_call(Macro, Monad) ->
    %% The original call is a temporary recovery value.  It keeps the parent
    %% tree traversable so sibling macro errors can still be collected; the
    %% outer traversal may delete the failed node after analysis completes.
    astranaut_traverse:catch_on_error(
      Monad,
      fun() -> astranaut_traverse:return(maps:get(call_ast, Macro)) end).

invoke_macro_function(#{module := Module, function := Function, arguments := Arguments} = Macro) ->
    try erlang:apply(Module, Function, Arguments) of
        Return ->
            %% Macro code owns a private State, but inherits the current
            %% traversal Attr so it can inspect the macro-call context.
            astranaut_traverse:scoped_state(ok, astranaut:traverse_return(Return))
    catch
        Class:Exception?CAPTURE_STACKTRACE ->
            StackTraces1 =
                lists:takewhile(
                  fun({M, F, A, _Pos}) -> 
                          {M, F, A} =/= {?MODULE, invoke_macro_function, 1};
                     (_Stack) ->
                          false
                  end, ?GET_STACKTRACE),
            Error = macro_exception(Arguments, Class, Exception, StackTraces1, Macro),
            astranaut_traverse:fail(Error)
    end.

process_macro_return(Return, Macro, Opts) ->
    ValidateOpts = #{record_defs => maps:get(forms, Opts, []), fail => collect},
    do([ traverse ||
           Attr <- astranaut_traverse:ask(),
           RenameContext <- macro_return_rename_context(Macro, Opts),
           ProcessReturn =
               astranaut_traverse:with_all_error(
                 fun({invalid_transform_normalization, Detail}) ->
                         {invalid_macro_return,
                          macro_return_detail(Macro, Opts, Detail)};
                    (Error) ->
                         Error
                 end,
                 astranaut:map_m(
                   fun(Node) ->
                           astranaut_traverse:return(
                             update_macro_return_node(
                               Node, RenameContext, maps:get(pos, Macro)))
                   end, Return,
                   #{traverse => post,
                     validate => input,
                     %% Preserve the inherited macro-call Attr as the root
                     %% environment of the return-tree traversal.
                     attr => Attr,
                     validate_opts => ValidateOpts})),
           Return1 <- astranaut_traverse:fail_on_error(ProcessReturn),
           commit_macro_return_counter(Opts),
           return(Return1)
       ]).

macro_return_detail(Macro, Opts, Detail) ->
    Current = macro_call_ref(Macro),
    Origin = macro_call_ref(maps:get(origin_macro, Opts, Macro)),
    case same_macro_call_ref(Current, Origin) of
        true ->
            Detail#{macro => Current};
        false ->
            Detail#{origin_macro => Origin, current_macro => Current}
    end.

same_macro_call_ref(#{mfa := MFA, arguments := Arguments, ast := Ast},
                    #{mfa := MFA, arguments := Arguments, ast := Ast}) ->
    true;
same_macro_call_ref(_Current, _Origin) ->
    false.

macro_call_ref(Macro) ->
    #{mfa => macro_mfa(Macro),
      arguments => maps:get(arguments, Macro, []),
      ast => maps:get(call_ast, Macro, undefined)}.

macro_mfa(#{macro := {Module, Function}, arguments := Arguments}) ->
    #{module => Module, function => Function, arity => length(Arguments)};
macro_mfa(#{module := LocalModule, macro_module := Module, macro := Function, arguments := Arguments})
  when LocalModule =/= Module ->
    #{function => Function, arity => length(Arguments), local => true};
macro_mfa(#{module := Module, function := Function, arguments := Arguments}) ->
    #{module => Module, function => Function, arity => length(Arguments)};
macro_mfa(#{function := Function, arity := Arity} = Macro) ->
    case maps:find(macro_module, Macro) of
        {ok, Module} ->
            #{module => Module, function => Function, arity => Arity};
        error ->
            #{function => Function, arity => Arity, local => true}
    end.

invalid_macro_return_mfa(#{macro := #{mfa := MFA}}) ->
    MFA;
invalid_macro_return_mfa(#{current_macro := #{mfa := MFA}}) ->
    MFA.

macro_exception(Arguments, Class, Exception, StackTraces, #{macro := {Module, Function}}) ->
    MFA = #{module => Module, function => Function, arity => length(Arguments)},
    {macro_exception, MFA, Arguments, {Class, Exception, StackTraces}};
%% replace `module`__local_macro with module in stacktrace
macro_exception(Arguments, Class, Exception, StackTraces,
                #{module := LocalModule, macro_module := Module, macro := Function}) ->
    StackTraces1 =
        lists:map(
          fun({M, F, A, Pos}) when M =:= LocalModule ->
                  {Module, F, A, Pos};
             (Val) ->
                  Val
          end, StackTraces),
    MFA = #{function => Function, arity => length(Arguments), local => true},
    {macro_exception, MFA, Arguments, {Class, Exception, StackTraces1}}.
    
should_transform_function(_Function, _Arity, all) ->
    true;
should_transform_function(Function, Arity, {except, Functions}) ->
    not ordsets:is_element({Function, Arity}, Functions);
should_transform_function(Function, Arity, LocalMacroCaller) ->
    ordsets:is_element({function, Function, Arity}, LocalMacroCaller).

%% for -exec_macro, if there is no macro found, error is returned
%% for other -Attr, if there is no macro with same name, not_macro is returned
%% for other -Attr, if there is macro with same name, but arity not matched, error is returned
attribute_find_macro({attribute, Pos, exec_macro, {Function, Arguments}}, Macros, _AttributeMacros) ->
    find_macro_with_arguments(Function, Arguments, Pos, Macros,
                              {attribute, Pos, exec_macro, {Function, Arguments}});
attribute_find_macro({attribute, Pos, exec_macro, {Module, Function, Arguments}}, Macros, _AttributeMacros) ->
    find_macro_with_arguments({Module, Function}, Arguments, Pos, Macros,
                              {attribute, Pos, exec_macro, {Module, Function, Arguments}});
attribute_find_macro({attribute, Pos, Attribute, Arguments}, _Macros, AttributeMacros) ->
    find_attribute_macro_with_arguments(Attribute, Arguments, Pos, AttributeMacros,
                                       {attribute, Pos, Attribute, Arguments});
attribute_find_macro(_Node, _Macros, _AttributeMacros) ->
    not_macro.

find_attribute_macro_with_arguments(Function, Arguments, Pos, AttributeMacroMap, CallAst) ->
    case maps:find(Function, AttributeMacroMap) of
        {ok, MacroMap} ->
            find_macro_with_arguments(Function, Arguments, Pos, MacroMap, CallAst);
        error ->
            not_macro
    end.



find_macro_with_arguments(MacroName, Arguments, Pos, Macros, CallAst) ->
    Arguments1 = to_list(Arguments),
    Arity = length(Arguments1),
    case find_macro(MacroName, Arity, Macros) of
        {ok, Macro} ->
            Macro1 = Macro#{pos => Pos, call_ast => CallAst},
            Arguments2 = group_arguments(Arguments1, Macro1),
            Arguments3 = append_attrs(Arguments2, Macro1),
            {ok, Macro1#{arguments => Arguments3}};
        error ->
            error
    end.

find_macro(MacroName, Arity, Macros) ->
    case maps:find({MacroName, Arity}, Macros) of
        {ok, Macro} ->
            {ok, Macro};
        error ->
            case maps:find({MacroName, 1}, Macros) of
                {ok, Macro} ->
                    case maps:get(group_args, Macro, false) of
                        false ->
                            error;
                        true ->
                            {ok, Macro}
                    end;
                error ->
                    error
            end
    end.

macro_name_str(#{module := Module, function := _Function, arity := _Arity}) ->
    atom_to_list(Module).

group_arguments(Arguments, #{group_args := true}) ->
    [Arguments];
group_arguments(Arguments, #{}) ->
    Arguments.

append_attrs(Arguments, #{attributes := Attrs, pos := Pos}) ->
    Arguments ++ [Attrs#{pos => Pos}];
append_attrs(Arguments, #{}) ->
    Arguments.

%%%===================================================================
%%% ===== Common helpers =====
%%%===================================================================

function_clauses_map([{function, _Pos, Name, Arity, Clauses}|T], Acc) ->
    NAcc = maps:put({Name, Arity}, Clauses, Acc),
    function_clauses_map(T, NAcc);
function_clauses_map([_H|T], Acc) ->
    function_clauses_map(T, Acc);
function_clauses_map([], Acc) ->
    Acc.

find_function_macro_callers(Forms, MacroMap, ExcludedFunctions) ->
    case maps:size(MacroMap) of
        0 ->
            ordsets:new();
        _ ->
            lists:foldl(
              fun({function, _Pos, Function, Arity, Clauses}, Acc) ->
                      case ordsets:is_element({Function, Arity}, ExcludedFunctions) of
                          true ->
                              Acc;
                          false ->
                              case has_macro_call(Clauses, MacroMap) of
                                  true ->
                                      ordsets:add_element({function, Function, Arity}, Acc);
                                  false ->
                                      Acc
                              end
                      end;
                 (_Form, Acc) ->
                      Acc
              end, ordsets:new(), Forms)
    end.

has_macro_call(Nodes, MacroMap) ->
    astranaut:sreduce(
      fun(_Node, true) ->
              true;
         (Node, false) ->
              case call_find_macro(uniform, Node, MacroMap) of
                  {ok, _Macro} ->
                      true;
                  error ->
                      false
              end
      end, false, Nodes, #{traverse => pre}).

to_list(Arguments) when is_list(Arguments) ->
    Arguments;
to_list(Arguments) ->
    [Arguments].


macro_return_rename_context(Macro, #{rename_quoted_variables := true}) ->
    do([ traverse ||
           Counter <- astranaut_traverse:get(),
           return({macro_name_str(Macro), integer_to_list(Counter)})
       ]);
macro_return_rename_context(_Macro, #{}) ->
    astranaut_traverse:return(undefined).

commit_macro_return_counter(#{rename_quoted_variables := true}) ->
    astranaut_traverse:modify(fun(Counter) -> Counter + 1 end);
commit_macro_return_counter(#{}) ->
    astranaut_traverse:return(ok).

update_macro_return_node(Node, RenameContext, Pos) ->
    Node1 = rename_quoted_variable_node(Node, RenameContext),
    Node2 = replace_pos_zero_node(Node1, Pos),
    astranaut_syntax:revert(Node2).

rename_quoted_variable_node({var, Pos, VarName} = Var,
                            {MacroNameStr, CounterStr}) ->
    case split_varname(atom_to_list(VarName)) of
        [Head, MacroNameStr1] when MacroNameStr =:= MacroNameStr1 ->
            VarName1 = list_to_atom(Head ++ "@" ++ MacroNameStr ++ "_" ++ CounterStr),
            {var, Pos, VarName1};
        _ ->
            Var
    end;
rename_quoted_variable_node(Node, _RenameContext) ->
    Node.

replace_pos_zero_node(Node, 0) ->
    Node;
replace_pos_zero_node(Node, Pos) ->
    case astranaut_syntax:get_pos(Node) of
        0 -> astranaut_syntax:set_pos(Node, Pos);
        _ -> Node
    end.

split_varname(String) ->
    case lists:splitwith(
           fun(Char) ->
                   Char /= $@
           end, String) of
        {Head, [$@|Tail]} ->
            [Head, Tail];
        {Head, []} ->
            [Head]
    end.

%%%===================================================================
%%% ===== Debug formatting helpers =====
%%%===================================================================
format_forms(Forms, Opts) ->
    case maps:get(debug_module, Opts, false) of
        true ->
            lists:map(
              fun(Form) ->
                      io:format("~s~n", [astranaut_lib:ast_safe_to_string(Form)])
              end, Forms);
        false ->
            ok
    end,
    case maps:get(debug_module_ast, Opts, false) of
        true ->
            io:format("~p~n", [Forms]);
        false ->
            ok
    end.

format_node(Nodes, Opts) when is_list(Nodes) ->
    lists:foreach(fun(Node) -> format_node(Node, Opts) end, Nodes);
format_node(Node, #{file := File, pos := Pos} = Opts) ->
    case maps:get(debug, Opts, false) of
        true ->
            io:format("from ~s:~p ~s~n", [filename:basename(File), Pos, format_mfa(Opts)]),
            io:format("~s~n", [astranaut_lib:ast_safe_to_string(Node)]);
        false ->
            ok
    end,
    case maps:get(debug_ast, Opts, false) of
        true ->
            io:format("from ~s:~p ~s~n", [filename:basename(File), Pos, format_mfa(Opts)]),
            io:format("~p~n", [Node]);
        false ->
            ok
    end;
format_node(_Node, _Opts) ->
    ok.

format_mfa(#{function := Function, arity := Arity, local := true}) ->
    io_lib:format("~p/~p", [Function, Arity]);
format_mfa(#{module := Module, function := Function, arity := Arity}) ->
    io_lib:format("~p:~p/~p", [Module, Function, Arity]).
