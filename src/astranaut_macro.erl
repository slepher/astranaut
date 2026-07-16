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
%% <li>`-import_macro(Module).' imports macros exported by another module.</li>
%% <li>`-use_macro(UseSpec).' selects imported or local macros and may add use-site options such as `alias' or `force_override'.</li>
%% <li>`-macro_options(Options).' updates source-ordered module options.
%% `debug', `debug_ast', and `max_depth' become defaults for later imported or
%% declared macros; `debug_module' and `debug_module_ast' control only final
%% module output.</li>
%% <li>`-export_macro(MacroSpec).' exports local functions as macros for other modules.</li>
%% <li>`-local_macro(MacroSpec).' declares local functions as macros without exporting them.</li>
%% <li>`-local_macro_retain(Functions).' retains matching local-macro closure
%% functions and specs in the transformed module.</li>
%% <li>`-exec_macro(Call).' expands a macro call in attribute position.</li>
%% </ul>
%%
%% `MacroSpec' accepts a function, a function list, or either form paired with
%% definition options. Definition options include `order', `as_attr',
%% `inject_attrs', `group_args', `force_override', `max_depth',
%% `extra_functions', and `internal_function'.
%% `extra_functions' explicitly adds helper functions to a local macro closure.
%% For a local macro, `internal_function' selects macros visible at the
%% declaration point whose calls remain ordinary function calls. A local
%% `Function/Arity' reference may resolve through a `use_macro' alias; that
%% call is restored to its original remote `Module:Function/Arity'.
%% These two closure options are accepted only by `local_macro'; an
%% `export_macro' declaration publishes a macro but does not construct its
%% local closure, and `macro_options' does not accept closure options.
%%
%% Expansion uses an ordered scan followed by final body traversal:
%% <ol>
%% <li>Unified attribute scan. External and ready local attribute macros are
%% processed left-to-right; generated forms are spliced at the current queue
%% position. Import, use and option forms, including generated ones, affect only
%% later forms and already processed attributes are not rescanned. Attribute
%% macro injection sees only forms already passed at the call site.</li>
%% <li>Local macro declarations are registered with `astranaut_macro_local'
%% using the materialised source view. Their closure forms are pre-expanded in
%% the macro environment and injection view captured before the declaration;
%% later forms participate only in closure discovery. Direct local calls are
%% discovered statically; indirect references such as `fun helper/1' require
%% `extra_functions'. A multi-function declaration shares this snapshot but is
%% stored as independent FA entries rather than as a persistent group.
%% A generation is loaded only when a real local dependency must become
%% callable, or when finalization introduces a new cumulative member set.</li>
%% <li>Local-macro finalization retains complete frozen closures rooted by
%% `local_macro_retain', `export', or `export_macro', and supplies the final
%% skip set. A retained frozen function is also a final module function, so it
%% is re-expanded from its original form in the final function context and
%% checked against its declaration-context canonical result. Ordinary function
%% bodies use the same final-context path with local whitelist validation
%% disabled.</li>
%% </ol>
%%
%% An explicit `local_macro_retain' entry produces separate warnings when its
%% FA is undefined or when the function exists outside every frozen closure.
%%
%% The final forms are sorted before returning to the compiler so generated
%% attributes remain in Erlang-valid form order.
%% @end

-module(astranaut_macro).

-include("do.hrl").

%% API
-export([parse_transform/2, format_error/1, expand_function/5]).
-export_type([local_macro_whitelist_control/0, function_expansion/0]).

-type fa() :: {atom(), non_neg_integer()}.
-type macro_map() :: map().
-type macro_runtime_context() :: #{macro_map := macro_map(),
                                   macro_options := map(),
                                   inject_forms := [term()]}.
-type local_macro_whitelist_control() ::
        astranaut_macro_expander:local_macro_whitelist_control().
-type function_expansion() ::
        astranaut_macro_expander:function_expansion().
-type local_macro_workflow_context() ::
        #{source_view := [term()],
          compile_opts := [compile:option()]}.
-type macro_ops() ::
        #{resolve_local_references :=
              fun(([{fa(), macro_map()}], [term()]) -> ordsets:ordset(fa())),
          expand_function :=
              fun((macro_map(), [term()], [term()], fa(),
                   local_macro_whitelist_control()) ->
                      astranaut_return:struct(function_expansion()))}.
-type scanner_state() ::
        #{global_macro_opts := map(),
          module_macro_maps := map(),
          module := module(),
          file := term(),
          compile_opts := [compile:option()],
          passed_forms := [term()],
          local_macro_state := map(),
          scan_local_declarations := #{term() => {term(), [fa()]}},
          macro_map := macro_map(),
          effective_macro_map := macro_map(),
          remaining_forms => [term()]}.
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
                  [astranaut_macro_expander:format_mfa(MFA), Arguments,
                   eunit_lib:format_exception(Exception)]);
format_error({invalid_macro_return, Detail}) ->
    io_lib:format("macro ~s returned invalid AST: ~p",
                  [astranaut_macro_expander:format_mfa(
                     invalid_macro_return_mfa(Detail)), Detail]);
format_error({invalid_extra_functions, Functions}) ->
    io_lib:format("extra_functions contains undefined functions: ~p", [Functions]);
format_error({undefined_internal_functions, Functions}) ->
    io_lib:format(
      "internal_function contains macros not visible at the declaration point: ~p",
      [Functions]);
format_error({undefined_local_macro_retain, Functions}) ->
    io_lib:format("local_macro_retain contains undefined functions: ~p",
                  [Functions]);
format_error({ineffective_local_macro_retain, Functions}) ->
    io_lib:format("local_macro_retain has no effect for functions outside every local macro closure: ~p",
                  [Functions]);
format_error({duplicate_local_macro_declaration, Function}) ->
    io_lib:format("duplicate local macro declaration for ~p", [Function]);
format_error({conflicting_internal_function_policy, Function, Policies}) ->
    io_lib:format("conflicting internal_function policy for ~p: ~p", [Function, Policies]);
format_error({conflicting_local_macro_closure_environment, FormId}) ->
    io_lib:format("local macro closure has conflicting expansion environments for ~p", [FormId]);
format_error({conflicting_local_macro_whitelist, FormId, Detail}) ->
    io_lib:format("local macro closure has conflicting whitelist for ~p: ~p",
                  [FormId, Detail]);
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
           {ScannedForms, ScanState} <-
               scan_attribute_forms(Module, File, GlobalMacroOpts0, Forms, CompileOpts),
           #{effective_macro_map := ScanEffectiveMacroMap,
             global_macro_opts := GlobalMacroOpts,
             local_macro_state := ScanLocalState,
             scan_local_declarations := ScanLocalDeclarations} = ScanState,
           AttributeForms = drop_local_declarations(
                                 ScannedForms, ScanLocalDeclarations),
           PreparedForms <- prepare_exports(AttributeForms),
           %% export_macro only publishes a macro for import by other modules.
           %% The current module's execution map is defined solely by
           %% local_macro declarations collected during the scan.
           finalize_attribute_macro_pass(Module, File, GlobalMacroOpts,
                                         ScanEffectiveMacroMap,
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
                  local_macro_state => astranaut_macro_local:new(),
                  scan_local_declarations => #{},
                  macro_map => #{},
                  effective_macro_map => #{}},
    astranaut_traverse:run(
      astranaut:map_forms_splice(fun scan_form/1, Forms, #{traverse => none, queue_state => true}),
      ?MODULE, #{node => form, validator => {role, form}}, InitState).

scan_form(Form) ->
    do([ traverse ||
           State <- astranaut_traverse:get(),
           case Form of
               {attribute, _Pos, local_macro, _Attr} ->
                   scan_local_macro(Form, State);
               {attribute, _Pos, import_macro, _Attr} ->
                   scan_env_form(Form, State);
               {attribute, _Pos, use_macro, _Attr} ->
                   scan_env_form(Form, State);
               {attribute, _Pos, macro_options, _Attr} ->
                   scan_env_form(Form, State);
               _ ->
                   scan_attribute_runtime(Form, State)
           end
       ]).

scan_local_macro({attribute, Pos, local_macro, Attr} = Form,
                               #{local_macro_state := LocalState,
                                 scan_local_declarations := ScanLocalDeclarations,
                                 remaining_forms := Queue} = State) ->
    SourceView = astranaut_macro_local:source_view(passed_forms(State), Queue),
    ClauseMap = function_clauses_map(SourceView, #{}),
    Validation = validate_local_macro_attribute(#{clause_map => ClauseMap}, Attr),
    case astranaut_return:run(Validation) of
        {just, {FAs, Options}} ->
            do([ traverse ||
                   %% Preserve warnings/error_ok carried by the single
                   %% validation computation before committing registration.
                   astranaut:traverse_return(Validation),
                   scan_valid_local_macro(
                     Form, Pos, FAs, Options, ClauseMap, SourceView,
                     State, LocalState,
                     ScanLocalDeclarations)
               ]);
        nothing ->
            %% Bridge the same validation result that was inspected above;
            %% do not execute a second validator pass merely to emit errors.
            astranaut:traverse_return(Validation)
    end.

scan_valid_local_macro(Form, Pos, FAs, Options, ClauseMap, SourceView,
                       State, LocalState,
                       ScanLocalDeclarations) ->
    do([ traverse ||
           RegisteredLocalState <- register_local_declaration(
                                     FAs, Options, Pos, SourceView,
                                     declaration_runtime_context(State),
                                     local_macro_ops(), LocalState),
           WorkflowContext = local_macro_workflow_context(
                               SourceView, maps:get(compile_opts, State)),
           LocalState1 <- astranaut:traverse_return(
                            astranaut_macro_local:prepare_declaration(
                              FAs, WorkflowContext, local_macro_ops(),
                              RegisteredLocalState)),
           EffectiveMacroMap1 <- astranaut:traverse_return(
                                  add_local_macro(
                                    State, SourceView, ClauseMap,
                                    FAs, Options)),
           %% Registration and map construction happen exactly here.  The
           %% later cleanup only removes this already-scanned declaration; it
           %% never validates or registers a local macro again.  Retained local
           %% helpers intentionally remain subject to Erlang unused warnings.
           astranaut_traverse:put(
             note_passed_form(
               Form, State#{local_macro_state => LocalState1,
                            effective_macro_map => EffectiveMacroMap1,
                            scan_local_declarations => maps:put(Form, {Pos, FAs}, ScanLocalDeclarations)})),
           return(Form)
       ]).

add_local_macro(#{module := Module, file := File, global_macro_opts := GlobalOpts,
                                 macro_map := ExternalMap,
                                 effective_macro_map := EffectiveMap}, SourceView, ClauseMap,
                                 FAs, Options) ->
    Ctx = #{module => Module, file => File, forms => SourceView,
            global_macro_opts => local_macro_global_opts(Module, ClauseMap, GlobalOpts),
            external_macro_map => ExternalMap, clause_map => ClauseMap},
    New = build_local_macro_map(Ctx, macro_options_by_fa(FAs, Options#{macro_source => local_macro})),
    merge_macro_maps(EffectiveMap, New).

register_local_declaration(FAs, Options, Pos, SourceView, RuntimeContext,
                           MacroOps, LocalState) ->
    case astranaut_macro_local:register(
           FAs, Options, SourceView, RuntimeContext,
           MacroOps, LocalState) of
        {ok, LocalState1} -> astranaut_traverse:return(LocalState1);
        {error, Error} ->
            astranaut_traverse:update_pos(
              Pos, ?MODULE, astranaut_traverse:fail(Error))
    end.

declaration_runtime_context(#{effective_macro_map := MacroMap,
                              global_macro_opts := GlobalOpts} = State) ->
    macro_runtime_context(MacroMap, GlobalOpts, passed_forms(State)).

scan_env_form({attribute, _Pos, import_macro, _Attr} = Form, State) ->
    #{global_macro_opts := GlobalMacroOpts} = State,
    case import_macro_form(GlobalMacroOpts, Form) of
        {ok, ModuleMacroMap} ->
            #{module := Module, file := File, macro_map := MacroMap,
              effective_macro_map := EffectiveMacroMap} = State,
            PassedForms = passed_forms(State),
            Effective = effective_module_macro_maps(File, Module, PassedForms, ModuleMacroMap),
            NewMap = uniform_imported_macro_map(Effective),
            case {merge_macro_maps_pure(MacroMap, NewMap),
                  merge_macro_maps_pure(EffectiveMacroMap, NewMap)} of
                {{ok, Merged}, {ok, EffectiveMerged}} ->
                    ModuleMacroMaps = maps:merge(maps:get(module_macro_maps, State, #{}), Effective),
                    do([ traverse ||
                           astranaut_traverse:put(State#{module_macro_maps => ModuleMacroMaps,
                                                         macro_map => Merged,
                                                         effective_macro_map => EffectiveMerged}),
                           return({splice, []})
                       ]);
                {{error, Reason}, _} ->
                    astranaut_traverse:fail(Reason);
                {_, {error, Reason}} ->
                    astranaut_traverse:fail(Reason)
            end;
        {error, Error} ->
            astranaut_traverse:fail(Error)
    end;
scan_env_form({attribute, _Pos, use_macro, _Attr} = Form, State) ->
    #{module := Module, file := File, macro_map := MacroMap,
      effective_macro_map := EffectiveMacroMap} = State,
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
           EffectiveMerged <- case merge_macro_maps_pure(EffectiveMacroMap, NewMap) of
                                  {ok, EffectiveMergedMap} ->
                                      astranaut_traverse:return(EffectiveMergedMap);
                                  {error, EffectiveReason} ->
                                      astranaut_traverse:fail(EffectiveReason)
                              end,
           ModuleMacroMaps0 = maps:get(module_macro_maps, State, #{}),
           ModuleMacroMaps1 = maps:fold(
                                fun(M, Macros, Acc) ->
                                        M1 = maps:get(M, Acc, #{}),
                                        maps:put(M, maps:merge(M1, Macros), Acc)
                                end, ModuleMacroMaps0, UsedMacros),
           astranaut_traverse:put(State#{module_macro_maps => ModuleMacroMaps1,
                                         macro_map => Merged,
                                         effective_macro_map => EffectiveMerged}),
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
       ]).
scan_attribute_runtime(Form, State) ->
    RuntimeContext = attribute_call_runtime_context(State),
    case astranaut_macro_expander:resolve_attribute_target(
           Form, RuntimeContext) of
        {ok, Target} ->
            run_attribute_macro(Target, RuntimeContext, State);
        error ->
            %% `error' means this is syntactically a macro invocation (an
            %% exec_macro or a registered attribute macro) but no executable
            %% macro matched it.  Keep the original form, but diagnose it here;
            %% the final local-attribute pass only passes it through to avoid a
            %% duplicate warning.
            do([ traverse ||
                   astranaut_traverse:warning(invalid_macro_attribute),
                   keep_scanned_form(Form)
               ]);
        not_macro ->
            keep_scanned_form(Form)
    end.

attribute_call_runtime_context(#{effective_macro_map := MacroMap} = State) ->
    macro_runtime_context(
      MacroMap, maps:get(global_macro_opts, State), passed_forms(State)).

-spec macro_runtime_context(macro_map(), map(), [term()]) ->
          macro_runtime_context().
macro_runtime_context(MacroMap, MacroOptions, InjectForms) ->
    #{macro_map => MacroMap,
      macro_options => MacroOptions,
      inject_forms => InjectForms}.

-spec local_macro_workflow_context([term()], [compile:option()]) ->
          local_macro_workflow_context().
local_macro_workflow_context(SourceView, CompileOpts) ->
    #{source_view => SourceView,
      compile_opts => CompileOpts}.

%% All attribute macros return to this path after any local compilation
%% prerequisite.  Argument grouping and inject_attrs therefore have exactly
%% one call-site implementation for external and local macros.
-spec run_attribute_macro(map(), macro_runtime_context(), scanner_state()) ->
          astranaut_traverse:struct(scanner_state(), {splice, [term()]}).
run_attribute_macro(Target, RuntimeContext, State) ->
    do([ traverse ||
           State1 <- ensure_attribute_target_callable(Target, State),
           Expanded <- astranaut_macro_expander:expand_attribute_target(
                         Target, RuntimeContext),
           ExpandedForms = to_list(Expanded),
           assert_scan_frozen_forms(
             ExpandedForms, maps:get(local_macro_state, State1)),
           return({splice, ExpandedForms})
       ]).

-spec ensure_attribute_target_callable(map(), scanner_state()) ->
          astranaut_traverse:struct(scanner_state(), scanner_state()).
ensure_attribute_target_callable(
    #{macro := #{macro_source := local_macro,
                 function := Function, arity := Arity}},
    #{local_macro_state := LocalState,
      compile_opts := CompileOpts,
      remaining_forms := Queue} = State) ->
    case maps:find({Function, Arity}, astranaut_macro_local:local_macros(LocalState)) of
        {ok, #{status := compiled}} -> astranaut_traverse:return(State);
        {ok, _} ->
            SourceView = astranaut_macro_local:source_view(passed_forms(State), Queue),
            do([ traverse ||
                   WorkflowContext = local_macro_workflow_context(
                                       SourceView, CompileOpts),
                   LocalState1 <- astranaut:traverse_return(
                                    astranaut_macro_local:need_callable(
                                      {Function, Arity}, WorkflowContext,
                                      local_macro_ops(), LocalState)),
                   State1 = State#{local_macro_state => LocalState1},
                   astranaut_traverse:put(State1),
                   return(State1)
               ]);
        error -> astranaut_traverse:return(State)
    end;
ensure_attribute_target_callable(_ExternalTarget, State) ->
    astranaut_traverse:return(State).

-spec assert_scan_frozen_forms([term()], map()) ->
          astranaut_traverse:struct(scanner_state(), ok).
assert_scan_frozen_forms(Forms, LocalState) ->
    case astranaut_macro_local:reject_locked_mutation(Forms, LocalState) of
        ok -> astranaut_traverse:return(ok);
        {error, {illegal_locked_form_mutation, Form} = Error} ->
            astranaut_traverse:update_pos(form_pos(Form), ?MODULE, astranaut_traverse:fail(Error))
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
                     Validator = export_macro_validator(),
                     {FAs, Options} <-
                         validate_macro_attribute(fun macro_without_module_attr/1, Validator, export_macro, Attr),
                     FAs1 <- remove_undefined_macros(FAs, ClausesMap),
                     case FAs1 of
                         [] ->
                             astranaut_return:return({[], Acc});
                         _ ->
                             %% exported_macro options for external usage
                             ExportedMacroAttribute = astranaut_lib:gen_attribute_node(exported_macro, Pos, [{FAs, Options}]),
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
           Validator = local_macro_validator(),
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
            GlobalMacroOpts#{formatter => astranaut_macro_local:module_name(Module)};
        false ->
            GlobalMacroOpts#{formatter => astranaut_macro}
    end.

local_macro_options(Module, GlobalMacroOpts, Function, Arity, MacroOptions) ->
    MacroOptions1 = maps:merge(GlobalMacroOpts, MacroOptions),
    MacroOptions1#{module => astranaut_macro_local:module_name(Module),
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

common_macro_definition_validator() ->
    #{as_attr => atom,
      order => {one_of, [outer, inner]},
      inject_attrs => {'or', [atom, {list_of, atom}]},
      group_args => boolean,
      force_override => boolean,
      max_depth => uinteger
     }.

export_macro_validator() ->
    common_macro_definition_validator().

local_macro_validator() ->
    maps:merge(
      common_macro_definition_validator(),
      #{extra_functions => {list_of, fun validate_function_with_arity/1},
        internal_function => fun validate_internal_function_policy/1}).

validate_function_with_arity({Function, Arity} = FA) when is_atom(Function), is_integer(Arity), Arity >= 0 ->
    {ok, FA};
validate_function_with_arity(FA) ->
    {error, {invalid_function_with_arity, FA}}.

validate_internal_function_policy(Value) when is_boolean(Value) ->
    {ok, Value};
validate_internal_function_policy(Functions) when is_list(Functions) ->
    astranaut_return:foldl_m(
      fun(Function, Acc) ->
              case validate_internal_function_ref(Function) of
                  {ok, Function1} ->
                      astranaut_return:return([Function1|Acc]);
                  {error, Error} ->
                      astranaut_return:error(Error)
              end
      end, [], lists:reverse(Functions));
validate_internal_function_policy(Value) ->
    {error, {invalid_internal_function, Value}}.

validate_internal_function_ref({Module, Function, Arity} = MFA)
  when is_atom(Module), is_atom(Function), is_integer(Arity), Arity >= 0 ->
    {ok, MFA};
validate_internal_function_ref(FA) ->
    validate_function_with_arity(FA).

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
-spec finalize_attribute_macro_pass(module(), file:filename(), map(), macro_map(), map(), [term()],
                                    [compile:option()]) ->
          astranaut_return:struct({[term()], map()}).
finalize_attribute_macro_pass(Module, File, GlobalMacroOpts, ScanEffectiveMacroMap,
                              ScanLocalState, Forms, CompileOpts) ->
    do([ return ||
           WorkflowContext = local_macro_workflow_context(
                               Forms, CompileOpts),
           RetainRoots = retain_roots(Forms),
           {FinalLocalEnv, FinalSkipIds, FinalLocalState} <-
               astranaut_macro_local:finalize(
                 RetainRoots, WorkflowContext,
                 local_macro_ops(), ScanLocalState),
           RetainWarnings = local_macro_retain_warnings(
                              Forms, FinalLocalState),
           FinalMacroMap = compiled_effective_macro_map(
                             ScanEffectiveMacroMap, FinalLocalEnv),
           {UnsortedAttributeForms, FunctionEnv0} <-
               finalize_attribute_forms(
                 Forms, Module, FinalMacroMap, FinalSkipIds, FinalLocalState,
                 GlobalMacroOpts),
           %% The attribute-pass output is sorted before the function pass sees it.
           AttributeForms = astranaut_syntax:sort_forms(UnsortedAttributeForms),
           FunctionEnv = FunctionEnv0#{global_macro_opts => GlobalMacroOpts},
           astranaut_return:then(
             file_formatted_warnings(File, RetainWarnings),
             return({AttributeForms, FunctionEnv}))
       ]).

file_formatted_warnings(File, Warnings) ->
    Error0 = astranaut_error:new(File),
    Error1 = astranaut_error:append_formatted_warnings(Warnings, Error0),
    astranaut_return:ok(ok, astranaut_error:eof(Error1)).

%% Preserve the source-ordered winner selected by the scan, while excluding
%% local declarations that are not callable in the final generation.
compiled_effective_macro_map(EffectiveMacroMap, FinalLocalEnv) ->
    CompiledFAs = ordsets:from_list(maps:keys(FinalLocalEnv)),
    maps:filter(
      fun(_Key, #{macro_source := local_macro,
                  function := Function, arity := Arity}) ->
              ordsets:is_element({Function, Arity}, CompiledFAs);
         (_Key, _ExternalMacro) ->
              true
      end, EffectiveMacroMap).

-spec finalize_attribute_forms([term()], module(), macro_map(), [term()], map(),
                               map()) ->
          astranaut_return:struct({[term()], map()}).
finalize_attribute_forms(
  Forms, Module, FinalMacroMap, FinalSkipIds,
  FinalLocalState, GlobalMacroOpts) ->
    do([ return ||
           %% Attribute macros are expanded exclusively by scan_attribute_forms/5.
           %% Re-running them here would let later declarations backscan forms
           %% that were already passed under an earlier environment.
           Forms2 = remove_final_skip_forms(Forms, FinalSkipIds),
           DetectedMacroCallers =
               astranaut_macro_expander:function_macro_callers(
                 Forms2, FinalMacroMap),
           RetainedFunctionIds = ordsets:from_list(
                                   [Id || Id = {function, _, _} <-
                                      astranaut_macro_local:retained_form_ids(
                                        FinalLocalState)]),
           FinalMacroCallers = ordsets:union(
                                 DetectedMacroCallers, RetainedFunctionIds),
           FinalSkipFunctionIds = ordsets:from_list([{function, Name, Arity}
                                                      || {function, Name, Arity} <- FinalSkipIds]),
           FunctionEnv = #{module => Module,
                           macro_map => FinalMacroMap,
                           runtime_context => final_function_runtime_context(
                                                FinalMacroMap,
                                                GlobalMacroOpts, Forms2),
                           local_macro_state => FinalLocalState,
                           callers => ordsets:subtract(FinalMacroCallers, FinalSkipFunctionIds)},
           return({Forms2, FunctionEnv})
       ]).

-spec run_function_macro_pass([term()], map()) ->
          astranaut_return:struct([term()]).
run_function_macro_pass(
  Forms, #{runtime_context := RuntimeContext,
           local_macro_state := LocalMacroState,
           callers := Callers}) ->
    do([ return ||
           {ExpandedForms, _FinalState} <-
               astranaut_macro_local:expand_final_functions(
                 Forms,
                 [{Name, Arity} || {function, Name, Arity} <- Callers],
                 RuntimeContext,
                 local_macro_ops(), LocalMacroState),
           return(ExpandedForms)
       ]).

-spec remove_final_skip_forms([term()], [term()]) -> [term()].
remove_final_skip_forms(Forms, FinalSkipIds) ->
    Skip = ordsets:from_list(FinalSkipIds),
    lists:flatmap(fun(Form) -> remove_final_skip_form(Form, Skip) end, Forms).

-spec remove_final_skip_form(term(), ordsets:ordset(term())) -> [term()].
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
    case ordsets:is_element(astranaut_macro_local:form_id(Form), Skip) of
        true -> [];
        false -> [Form]
    end.

-spec merge_macro_maps(macro_map(), macro_map()) ->
          astranaut_return:struct(macro_map()).
merge_macro_maps(First, Second) ->
    case merge_macro_maps_pure(First, Second) of
        {ok, Merged} ->
            astranaut_return:return(Merged);
        {error, {macro_override, MacroKey, ExistingMacro, OverridingMacro}} ->
            macro_override_fail(
              MacroKey, ExistingMacro, OverridingMacro)
    end.

-spec merge_macro_maps_pure(macro_map(), macro_map()) ->
          {ok, macro_map()} | {error, term()}.
merge_macro_maps_pure(First, Second) ->
    merge_macro_maps_pure_loop(maps:to_list(Second), First).

-spec merge_macro_maps_pure_loop([{term(), map()}], macro_map()) ->
          {ok, macro_map()} | {error, term()}.
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

-spec macro_override_fail(term(), map(), map()) ->
          astranaut_return:struct(no_return()).
macro_override_fail(MacroKey, ExistingMacro, OverridingMacro) ->
    astranaut_return:error_fail({macro_override, MacroKey, ExistingMacro, OverridingMacro}).

%%%===================================================================
%%% ===== Expander facade and local-macro operations =====
%%%===================================================================

-spec local_macro_ops() -> macro_ops().
local_macro_ops() ->
    #{resolve_local_references =>
          fun astranaut_macro_expander:resolve_local_references/2,
      expand_function => fun astranaut_macro_expander:expand_function/5}.

%% Compatibility facade. Internal local-macro operations call the expander
%% directly; callers of the historical public API keep the same contract.
-spec expand_function(macro_map(), [term()], [term()], fa(),
                      local_macro_whitelist_control()) ->
          astranaut_return:struct(function_expansion()).
expand_function(MacroEnv, InjectForms, Forms, TargetFA, WhitelistControl) ->
    astranaut_macro_expander:expand_function(
      MacroEnv, InjectForms, Forms, TargetFA, WhitelistControl).

retain_roots(Forms) ->
    lists:foldl(
      fun({attribute, _Pos, local_macro_retain, Attr}, Acc) -> retain_fas(Attr, Acc);
         ({attribute, _Pos, export_macro, Attr}, Acc) -> retain_fas(Attr, Acc);
         ({attribute, _Pos, export, Attr}, Acc) -> retain_fas(Attr, Acc);
         (_Form, Acc) -> Acc
      end, [], Forms).

local_macro_retain_warnings(Forms, LocalMacroState) ->
    DefinedFAs = ordsets:from_list(
                   maps:keys(function_clauses_map(Forms, #{}))),
    lists:flatmap(
      fun({attribute, Pos, local_macro_retain, Attr}) ->
              Roots = ordsets:from_list(retain_fas(Attr, [])),
              Undefined = ordsets:subtract(Roots, DefinedFAs),
              Existing = ordsets:subtract(Roots, Undefined),
              Nonclosure = astranaut_macro_local:nonclosure_retain_roots(
                             Existing, LocalMacroState),
              retain_root_warnings(Pos, Undefined, Nonclosure);
         (_Form) ->
              []
      end, Forms).

retain_root_warnings(Pos, Undefined, Nonclosure) ->
    [{Pos, ?MODULE, {undefined_local_macro_retain, Undefined}}
     || Undefined =/= []] ++
    [{Pos, ?MODULE, {ineffective_local_macro_retain, Nonclosure}}
     || Nonclosure =/= []].

retain_fas({FAs, _Options}, Acc) when is_list(FAs) -> retain_fas(FAs, Acc);
retain_fas(FAs, Acc) when is_list(FAs) ->
    [FA || FA = {Name, Arity} <- FAs, is_atom(Name), is_integer(Arity)] ++ Acc;
retain_fas({Name, Arity} = FA, Acc) when is_atom(Name), is_integer(Arity) -> [FA | Acc];
retain_fas(_Other, Acc) -> Acc.

final_function_runtime_context(MacroMap, MacroOptions, InjectForms) ->
    macro_runtime_context(MacroMap, MacroOptions, InjectForms).

invalid_macro_return_mfa(#{macro := #{mfa := MFA}}) ->
    MFA;
invalid_macro_return_mfa(#{current_macro := #{mfa := MFA}}) ->
    MFA.

to_list(Arguments) when is_list(Arguments) ->
    Arguments;
to_list(Arguments) ->
    [Arguments].

%%% ===== Common helpers =====
%%%===================================================================

function_clauses_map([{function, _Pos, Name, Arity, Clauses}|T], Acc) ->
    NAcc = maps:put({Name, Arity}, Clauses, Acc),
    function_clauses_map(T, NAcc);
function_clauses_map([_H|T], Acc) ->
    function_clauses_map(T, Acc);
function_clauses_map([], Acc) ->
    Acc.

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
