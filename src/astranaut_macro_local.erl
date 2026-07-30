%%%-------------------------------------------------------------------
%%% @doc State and planning for local macros.
%%%
%%% This module intentionally does not own the forms scan nor macro invocation.
%%% `astranaut_macro' supplies a materialised source view and performs generic
%%% expansion; this module owns the immutable declaration snapshots and the
%%% state transitions needed to build local macro generations.
%%%-------------------------------------------------------------------
-module(astranaut_macro_local).

-include("do.hrl").
-include("stacktrace.hrl").

-define(LOCAL_MACRO_OWNER_ATTR, astranaut_local_macro_owner).

-export([new/0, register/5, prepare_declaration/3,
         need_callable/3,
         compile_plan/2,
         cache_expanded/5, commit_compiled/3, finalize/2,
         finalize/3,
         frozen_ids/1, local_macros/1,
         retained_form_ids/1, nonclosure_retain_roots/2,
         expand_final_functions/4,
         source_view/2, env_fingerprint/3,
         reject_locked_mutation/2, safe_load/3, finalize_plan/1,
         module_name/1,
         form_id/1, materialize_forms/2, execute_plan/3,
         with_generation_lock/2, with_local_macro_lifecycle/3]).

-type fa() :: {atom(), non_neg_integer()}.
-type form_id() :: {function | spec, atom(), non_neg_integer()}.
-type macro_environment() ::
        astranaut_macro_registry:macro_environment().
-type local_macro_whitelist_control() ::
        astranaut_macro_expander:local_macro_whitelist_control().
-type expansion_record() ::
        #{canonical_whitelist := disabled | ordsets:ordset(fa()),
          canonical_result := term(),
          results_by_input :=
              #{term() => #{whitelist := disabled | ordsets:ordset(fa()),
                            result := term()}}}.
-type workflow_context() :: #{source_view := [term()],
                              compile_opts := [compile:option()]}.
-type expansion_request() ::
        #{closure_ids := ordsets:ordset(form_id()),
          closure_fas := ordsets:ordset(fa()),
          candidate_local_macros := ordsets:ordset(fa()),
          function_call_analysis := map(),
          referenced_local_macros := ordsets:ordset(fa()),
          macro_environment_snapshot := macro_environment(),
          source_view := [term()],
          forms := #{form_id() => term()}}.
-type compilation_boundary() ::
        #{members := [fa()],
          requests := [expansion_request()]}.
-type state() ::
        #{expansion_records := #{form_id() => expansion_record()},
          atom() => term()}.

-spec new() -> state().
new() ->
    #{local_macros => #{},
      frozen_forms => #{},
      expansion_records => #{},
      canonical_expanded_forms => #{},
      compiled_forms => #{},
      committed_boundaries => #{},
      local_macro_expanded_ids => ordsets:new(),
      retain_roots => ordsets:new(),
      generation => 0}.

-spec register([fa()], map(), [term()], macro_environment(), state()) ->
          {ok, state()} | {error, term()}.
register(FAs, Options, SourceView,
         #{macro_map := MacroMap,
           macro_options := _} = MacroEnvironment,
         State)
  when is_list(FAs), is_map(Options) ->
    CandidateLocalMap = local_macro_entries(MacroMap),
    do_register(FAs, Options, SourceView, MacroEnvironment,
                CandidateLocalMap, State).

-spec do_register([fa()], map(), [term()], macro_environment(),
                  map(), state()) ->
          {ok, state()} | {error, term()}.
do_register(FAs, Options, SourceView, MacroEnvironment,
            CandidateLocalMap, State) ->
    Macros = maps:get(local_macros, State),
    case duplicate_or_existing(FAs, Macros) of
        none ->
            FormMap = forms_id_map(SourceView),
            CandidateFAs = local_macro_fas(CandidateLocalMap),
            case closures(
                   FAs, Options, FormMap,
                   maps:get(macro_map, MacroEnvironment)) of
                {ok, Closures, FunctionCallAnalysis} ->
                    Order = next_order(Macros),
                    Frozen = maps:merge(
                               maps:get(frozen_forms, State),
                               freeze_closures(Closures, FormMap)),
                    NewMacros =
                        lists:foldl(
                          fun(FA, Acc) ->
                                  Closure = maps:get(FA, Closures),
                                  ClosureAnalysis =
                                      closure_function_analysis(
                                        Closure, FunctionCallAnalysis),
                                  Refs = referenced_local_macros(
                                           ClosureAnalysis),
                                  maps:put(
                                    FA,
                                    #{order => Order,
                                      macro_environment_snapshot =>
                                          MacroEnvironment,
                                      closure_ids =>
                                          closure_ids(Closure, FormMap),
                                      closure_fas => Closure,
                                      candidate_local_macros => CandidateFAs,
                                      function_call_analysis =>
                                          ClosureAnalysis,
                                      referenced_local_macros => Refs,
                                      source_view => SourceView,
                                      options => Options,
                                      status => pending},
                                    Acc)
                          end, Macros, FAs),
                    {ok, State#{local_macros => NewMacros,
                               frozen_forms => Frozen}};
                {error, _} = Error -> Error
            end;
        FA ->
            {error, {duplicate_local_macro_declaration, FA}}
    end.

-spec local_macro_entries(map()) -> map().
local_macro_entries(MacroMap) ->
    maps:filter(
      fun(_Key, #{macro_source := local_macro}) -> true;
         (_Key, _Macro) -> false
      end, MacroMap).

-spec local_macro_fas(map()) -> ordsets:ordset(fa()).
local_macro_fas(MacroMap) ->
    ordsets:from_list(
      [{Function, Arity}
       || {_Key, #{macro_source := local_macro,
                   function := Function, arity := Arity}} <-
              maps:to_list(MacroMap)]).

-spec keep_allowed_local_fas(map(), ordsets:ordset(fa())) -> map().
keep_allowed_local_fas(MacroMap, Allowed) ->
    maps:filter(
      fun(_Key, #{macro_source := local_macro,
                  function := Function, arity := Arity}) ->
              ordsets:is_element({Function, Arity}, Allowed);
         (_Key, _Macro) ->
              true
      end, MacroMap).

%% Registration freezes each declaration member with the same order and
%% runtime snapshot. Preparation eagerly validates
%% every form that is currently expandable, compiling only earlier local
%% dependencies that are genuinely needed to perform that expansion.
-spec prepare_declaration([fa()], workflow_context(), state()) ->
          astranaut_return:struct(state()).
prepare_declaration(FAs, WorkflowContext, State) ->
    Macros = maps:get(local_macros, State),
    Dependencies = lists:foldl(
                     fun(FA, Acc) ->
                             Entry = maps:get(FA, Macros),
                             ordsets:union(
                               maps:get(referenced_local_macros, Entry), Acc)
                     end, ordsets:new(), FAs),
    do([ return ||
           State1 <- astranaut_return:foldl_m(
                       fun(Dependency, StateAcc) ->
                               need_callable(
                                 Dependency, WorkflowContext, StateAcc)
                       end, State, Dependencies),
           Requests = requests_for_fas(FAs, State1),
           State2 <- prepare_requests(
                       Requests, WorkflowContext, State1),
           return(State2)
       ]).

-spec need_callable(fa(), workflow_context(), state()) ->
          astranaut_return:struct(state()).
need_callable(FA, WorkflowContext, State) ->
    case compile_plan(FA, State) of
        {ok, Plan} -> execute_plan(Plan, WorkflowContext, State);
        {error, Error} -> astranaut_return:error_fail(Error)
    end.

%% A plan is deliberately pure. execute_plan/3 expands each requested frozen
%% form, validates the cache, then commits it atomically.
-spec compile_plan(fa() | all, state()) ->
          {ok, [compilation_boundary()]} | {error, term()}.
compile_plan(Needed, #{local_macros := Macros} = State) ->
    case needed_entries(Needed, Macros) of
        {error, _} = Error -> Error;
        Entries ->
            Ordered = lists:keysort(1, [{maps:get(order, Entry), FA, Entry}
                                        || {FA, Entry} <- Entries]),
            {ok, compilation_boundaries(
                   Needed, Ordered, maps:get(frozen_forms, State))}
    end.

%% The final generation is intentionally rebuilt from every declaration, not
%% merely pending ones.  Cached expansion results let the caller avoid work.
-spec finalize_plan(state()) ->
          {ok, [compilation_boundary()]} | {error, term()}.
finalize_plan(State) -> compile_plan(all, State).

-spec cache_expanded(form_id(), term(), disabled | ordsets:ordset(fa()),
                     term(), state()) -> {ok, state()} | {error, term()}.
cache_expanded(FormId, Fingerprint, Whitelist, ExpandedForm, State) ->
    Records = maps:get(expansion_records, State, #{}),
    Canonical = maps:get(canonical_expanded_forms, State, #{}),
    Record0 = maps:get(FormId, Records, #{}),
    CanonicalWhitelist = maps:get(canonical_whitelist, Record0, Whitelist),
    CanonicalResult = maps:get(canonical_result, Record0, ExpandedForm),
    case whitelist_matches(CanonicalWhitelist, Whitelist) of
        false ->
            {error, whitelist_conflict(
                      FormId, CanonicalWhitelist, Whitelist)};
        true when CanonicalResult =/= ExpandedForm ->
            {error, {conflicting_local_macro_closure_environment, FormId}};
        true ->
            Results0 = maps:get(results_by_input, Record0, #{}),
            Result = #{whitelist => Whitelist, result => ExpandedForm},
            case maps:find(Fingerprint, Results0) of
                {ok, Existing} when Existing =/= Result ->
                    cache_input_conflict(
                      FormId, CanonicalWhitelist, Existing, Result);
                _ ->
                    Record = #{canonical_whitelist => CanonicalWhitelist,
                               canonical_result => CanonicalResult,
                               results_by_input =>
                                   maps:put(Fingerprint, Result, Results0)},
                    {ok, State#{expansion_records =>
                                   maps:put(FormId, Record, Records),
                               canonical_expanded_forms =>
                                   maps:put(FormId, CanonicalResult,
                                            Canonical)}}
            end
    end.

whitelist_matches(disabled, disabled) -> true;
whitelist_matches(Expected, Observed) -> Expected =:= Observed.

cache_input_conflict(FormId, CanonicalWhitelist,
                     #{whitelist := ExistingWhitelist},
                     #{whitelist := Whitelist})
  when ExistingWhitelist =/= Whitelist ->
    {error, whitelist_conflict(
              FormId, CanonicalWhitelist, Whitelist)};
cache_input_conflict(FormId, _CanonicalWhitelist, _Existing, _Result) ->
    {error, {conflicting_local_macro_closure_environment, FormId}}.

whitelist_conflict(FormId, Expected, Observed) ->
    {conflicting_local_macro_whitelist, FormId,
     #{expected => Expected,
       observed => Observed,
       unexpected => ordsets:subtract(Observed, Expected),
       missing => ordsets:subtract(Expected, Observed)}}.

-spec expanded_form(form_id(), term(), state()) ->
          {ok, #{whitelist := disabled | ordsets:ordset(fa()),
                 result := term()}} | error.
expanded_form(FormId, Fingerprint, State) ->
    case maps:find(FormId, maps:get(expansion_records, State, #{})) of
        {ok, Record} ->
            maps:find(Fingerprint, maps:get(results_by_input, Record, #{}));
        error ->
            error
    end.

-spec local_versions([fa()], state()) -> #{fa() => non_neg_integer()}.
local_versions(FAs, State) ->
    Macros = maps:get(local_macros, State),
    maps:from_list(
      [{FA, Generation}
       || FA <- FAs,
          {ok, #{status := compiled, compiled_generation := Generation}} <-
              [maps:find(FA, Macros)]]).

%% Only a successful compiler/load operation may call this.  It is therefore
%% safe to advance the generation and mark declarations callable here.
-spec commit_compiled([fa()], #{form_id() => term()}, state()) -> state().
commit_compiled(FAs, Forms, #{local_macros := Macros} = State) ->
    Generation = maps:get(generation, State) + 1,
    Macros1 = lists:foldl(
                fun(FA, Acc) ->
                        case maps:find(FA, Acc) of
                            {ok, Entry} ->
                                maps:put(FA, Entry#{status => compiled,
                                                   compiled_generation => Generation}, Acc);
                            error -> Acc
                        end
                end, Macros, FAs),
    ExpandedIds = ordsets:union(maps:keys(Forms), maps:get(local_macro_expanded_ids, State)),
    State#{local_macros => Macros1,
           compiled_forms => maps:merge(maps:get(compiled_forms, State), Forms),
           local_macro_expanded_ids => ExpandedIds,
           generation => Generation}.

%% Retain roots are supplied at the end because export/export_macro/retain can
%% occur after a declaration.  The scanner remains responsible for finding
%% those attributes in its complete output stream.
-spec finalize([fa()], state()) -> {map(), ordsets:ordset(form_id()), state()}.
finalize(RetainRoots, #{local_macros := Macros} = State) ->
    Retained = retained_ids(ordsets:from_list(RetainRoots), Macros),
    Skip = ordsets:subtract(maps:get(local_macro_expanded_ids, State), Retained),
    LocalEnv = maps:from_list([
                  {FA, Entry}
                  || {FA, Entry} <- maps:to_list(Macros), maps:get(status, Entry) =:= compiled]),
    {LocalEnv, Skip, State#{retain_roots => ordsets:from_list(RetainRoots), retained_form_ids => Retained}}.

-spec finalize([fa()], workflow_context(), state()) ->
          astranaut_return:struct({map(), ordsets:ordset(form_id()), state()}).
finalize(RetainRoots, WorkflowContext, State) ->
    case finalize_plan(State) of
        {error, Error} ->
            astranaut_return:error_fail(Error);
        {ok, Plan} ->
            do([ return ||
                   State1 <- execute_plan(Plan, WorkflowContext, State),
                   {FinalLocalEnv, FinalSkipIds, State2} = finalize(RetainRoots, State1),
                   %% Retain is now only a lifecycle/selection policy.  Retain
                   %% and ordinary functions are expanded together later with
                   %% the single final macro environment.
                   return({FinalLocalEnv, FinalSkipIds, State2})
               ])
    end.

-spec frozen_ids(state()) -> ordsets:ordset(form_id()).
frozen_ids(State) -> ordsets:from_list(maps:keys(maps:get(frozen_forms, State))).

-spec local_macros(state()) -> map().
local_macros(State) -> maps:get(local_macros, State).

-spec retained_form_ids(state()) -> ordsets:ordset(form_id()).
retained_form_ids(State) ->
    maps:get(retained_form_ids, State, ordsets:new()).

-spec nonclosure_retain_roots([fa()], state()) -> ordsets:ordset(fa()).
nonclosure_retain_roots(Roots, #{local_macros := Macros}) ->
    ClosureFAs = maps:fold(
                   fun(_FA, Entry, Acc) ->
                           ordsets:union(
                             maps:get(closure_fas, Entry), Acc)
                   end, ordsets:new(), Macros),
    ordsets:subtract(ordsets:from_list(Roots), ClosureFAs).

%% Retain and ordinary Step-2 functions share one source-ordered Forms pass.
%% Every target carries its own frozen source, macro environment and whitelist
%% control, so differing local environments do not require repeated scans.
-spec expand_final_functions([term()], [fa()], macro_environment(),
                             state()) ->
          astranaut_return:struct({[term()], state()}).
expand_final_functions(
  Forms, TargetFAs, MacroEnvironment, State) ->
    OriginalMap = maps:merge(forms_id_map(Forms), maps:get(frozen_forms, State)),
    PreparationContext =
        #{original_forms => OriginalMap,
          macro_environment => MacroEnvironment},
    {Tasks, CompiledForms, TaskMetadata} =
        lists:foldl(
          fun(TargetFA, Acc) ->
                  prepare_final_function_task(
                    TargetFA, PreparationContext, State, Acc)
          end, {#{}, #{}, #{}}, TargetFAs),
    case map_size(Tasks) of
        0 ->
            astranaut_return:return(
              {materialize_forms(Forms, CompiledForms), State});
        _ ->
            do([ return ||
                   #{forms := ExpandedForms,
                     task_results := TaskResults} <-
                       astranaut_macro_expander:expand_functions(
                         Forms, Tasks),
                   {CompiledForms1, State1} <-
                       cache_final_function_tasks(
                         TargetFAs, TaskResults, TaskMetadata,
                         CompiledForms, State),
                   return({materialize_forms(
                             ExpandedForms, CompiledForms1),
                           State1})
               ])
    end.

prepare_final_function_task(
  {Name, Arity},
  #{original_forms := OriginalMap,
    macro_environment :=
        #{macro_map := MacroMap} = MacroEnvironment},
  State, Acc) ->
    FormId = {function, Name, Arity},
    case maps:find(FormId, OriginalMap) of
        error ->
            Acc;
        {ok, OriginalForm} ->
            prepare_final_function_task(
              FormId, OriginalForm, MacroMap,
              MacroEnvironment, State, Acc)
    end.

prepare_final_function_task(
  FormId, OriginalForm, MacroMap, MacroEnvironment, State,
  {Tasks, CompiledForms, TaskMetadata}) ->
    WhitelistControl = final_whitelist_control(FormId, State),
    AllowedLocalFAs = final_allowed_local_fas(
                        WhitelistControl,
                        local_macro_fas(MacroMap)),
    EffectiveMacroMap = keep_allowed_local_fas(
                          MacroMap, AllowedLocalFAs),
    FingerprintLocalFAs = fingerprint_local_fas(
                            WhitelistControl, AllowedLocalFAs),
    LocalVersions = local_versions(FingerprintLocalFAs, State),
    Fingerprint = env_fingerprint(
                    EffectiveMacroMap, LocalVersions,
                    maps:get(macro_options, MacroEnvironment)),
    case cached_final_expansion(
           FormId, Fingerprint, WhitelistControl, State) of
        {ok, ExpandedForm} ->
            {Tasks, maps:put(FormId, ExpandedForm, CompiledForms),
             TaskMetadata};
        error ->
            Task0 = #{form => OriginalForm,
                      macro_map => EffectiveMacroMap,
                      whitelist_control => WhitelistControl},
            Task = add_macro_call_hint(
                     FormId, OriginalForm,
                     maps:get(
                       function_call_analysis,
                       MacroEnvironment, #{}),
                     Task0),
            Metadata = #{fingerprint => Fingerprint,
                          whitelist_control => WhitelistControl,
                          fallback_form => OriginalForm},
            {maps:put(FormId, Task, Tasks), CompiledForms,
             maps:put(FormId, Metadata, TaskMetadata)}
    end.

cache_final_function_tasks(
  TargetFAs, TaskResults, TaskMetadata, CompiledForms, State) ->
    astranaut_return:foldl_m(
      fun({Name, Arity}, {CompiledAcc, StateAcc}) ->
              FormId = {function, Name, Arity},
              case maps:find(FormId, TaskMetadata) of
                  error ->
                      astranaut_return:return({CompiledAcc, StateAcc});
                  {ok, #{fingerprint := Fingerprint,
                         whitelist_control := WhitelistControl,
                         fallback_form := FallbackForm}} ->
                      case maps:find(FormId, TaskResults) of
                          error ->
                              astranaut_return:return(
                                {maps:put(FormId, FallbackForm, CompiledAcc),
                                 StateAcc});
                          {ok, #{form := ExpandedForm,
                                 local_macro_whitelist := Whitelist}} ->
                              do([ return ||
                                     {CachedForm, State1} <-
                                         cache_final_expansion(
                                           FormId, Fingerprint, Whitelist,
                                           ExpandedForm, WhitelistControl,
                                           StateAcc),
                                     return({maps:put(
                                               FormId, CachedForm,
                                               CompiledAcc),
                                             State1})
                                 ])
                      end
              end
      end, {CompiledForms, State}, TargetFAs).

final_whitelist_control(FormId, State) ->
    case maps:is_key(FormId, maps:get(frozen_forms, State)) of
        false -> disabled;
        true -> whitelist_control(FormId, State)
    end.

final_allowed_local_fas(#{mode := verify, expected := Expected}, _Candidates) ->
    Expected;
final_allowed_local_fas(_Control, Candidates) ->
    Candidates.

fingerprint_local_fas(#{mode := verify, expected := Expected}, _Candidates) ->
    Expected;
fingerprint_local_fas(_Control, Candidates) ->
    Candidates.

cached_final_expansion(_FormId, _Fingerprint, disabled, _State) ->
    error;
cached_final_expansion(FormId, Fingerprint, _Control, State) ->
    case expanded_form(FormId, Fingerprint, State) of
        {ok, #{result := ExpandedForm}} -> {ok, ExpandedForm};
        error -> error
    end.

cache_final_expansion(_FormId, _Fingerprint, disabled, ExpandedForm,
                      disabled, State) ->
    astranaut_return:return({ExpandedForm, State});
cache_final_expansion(FormId, Fingerprint, Whitelist, ExpandedForm,
                      _Control, State) ->
    cache_form_result(
      FormId, Fingerprint, Whitelist, ExpandedForm, State).

%% The declaration source view is deliberately a two-part concatenation.  A
%% generated form becomes visible only after it has entered Queue; no future
%% splice result can leak into a declaration's closure analysis.
-spec source_view([term()], [term()]) -> [term()].
source_view(Passed, Queue) -> Passed ++ Queue.

%% The resolved macro map already contains injected attribute values, so all
%% observable environment inputs enter the key without retaining source Forms.
-spec env_fingerprint(map(), term(), term()) -> binary().
env_fingerprint(MacroMap, LocalVersions, Options) ->
    term_to_binary({MacroMap, LocalVersions, Options}).

-spec reject_locked_mutation([term()], state()) -> ok | {error, term()}.
reject_locked_mutation(Forms, State) ->
    Frozen = maps:get(frozen_forms, State),
    case [Form || Form <- Forms,
                  begin Id = form_id(Form), Id =/= undefined andalso maps:is_key(Id, Frozen) end] of
        [] -> ok;
        [Form | _] -> {error, {illegal_locked_form_mutation, Form}}
    end.

%% Generic macro expansion is performed by astranaut_macro before this
%% boundary.  This module owns selection and assembly of the cumulative local
%% macro module, followed by its safe replacement. The caller holds
%% with_generation_lock/2 across expansion, this load, and the state commit.
-spec load_local_macro_forms(ordsets:ordset(fa()), ordsets:ordset(fa()),
                             #{form_id() => term()}, [term()],
                             [compile:option()]) -> astranaut_return:struct(term()).
load_local_macro_forms([], _LocalMacroRelatedFunctions, _CompiledForms,
                       _SourceForms, _CompileOpts) ->
    astranaut_return:return(ok);
load_local_macro_forms(LocalMacroFunctions, LocalMacroRelatedFunctions,
                       CompiledForms, SourceForms, CompileOpts) ->
    MaterializedForms = materialize_forms(SourceForms, CompiledForms),
    Module = astranaut_lib:analyze_forms_module(MaterializedForms),
    case ensure_local_macro_module_available(Module) of
        ok ->
            Forms = select_local_macro_forms(
                      LocalMacroRelatedFunctions, MaterializedForms),
            compile_local_macro_forms(
              LocalMacroFunctions, Forms, CompileOpts);
        {error, Error} ->
            local_macro_module_error(
              Module, MaterializedForms, Error)
    end.

-spec with_generation_lock(module(), fun(() -> Result)) -> Result.
with_generation_lock(Module, Fun) ->
    astranaut_lib:with_module_lock(Module, Fun).

-spec with_local_macro_lifecycle(
        module(), astranaut_error:compile_file(),
        fun(() -> astranaut_return:struct(astranaut:forms()))) ->
          astranaut:parse_transform_return().
with_local_macro_lifecycle(Module, File, Fun) ->
    with_generation_lock(
      Module,
      fun() ->
              try Fun() of
                  Return ->
                      finish_local_macro_lifecycle(
                        Module, File,
                        astranaut_return:to_compiler(Return))
              catch
                  Class:Reason?CAPTURE_STACKTRACE ->
                      _ = cleanup_local_macro_module(Module),
                      erlang:raise(Class, Reason, ?GET_STACKTRACE)
              end
      end).

finish_local_macro_lifecycle(Module, File, Return) ->
    LocalModule = module_name(Module),
    FrozenReturn = freeze_local_macro_diagnostics(LocalModule, Return),
    add_cleanup_error(
      File,
      cleanup_local_macro_module(Module, LocalModule),
      FrozenReturn).

cleanup_local_macro_module(Module) ->
    cleanup_local_macro_module(Module, module_name(Module)).

cleanup_local_macro_module(Module, LocalModule) ->
    case local_macro_module_owner(LocalModule) of
        {ok, Module} -> unload_local_macro_module(LocalModule);
        _ -> ok
    end.

freeze_local_macro_diagnostics(LocalModule, Return) ->
    map_compiler_diagnostics(
      fun(Diagnostic) ->
              freeze_local_macro_diagnostic(LocalModule, Diagnostic)
      end, Return).

freeze_local_macro_diagnostic(
  LocalModule, {Pos, LocalModule, Error}) ->
    Message = LocalModule:format_error(Error),
    {Pos, astranaut_macro,
     {local_macro_diagnostic, LocalModule, Error, Message}};
freeze_local_macro_diagnostic(_LocalModule, Diagnostic) ->
    Diagnostic.

map_compiler_diagnostics(Fun, {warning, Forms, Warnings}) ->
    {warning, Forms, map_file_diagnostics(Fun, Warnings)};
map_compiler_diagnostics(Fun, {error, Errors, Warnings}) ->
    {error,
     map_file_diagnostics(Fun, Errors),
     map_file_diagnostics(Fun, Warnings)};
map_compiler_diagnostics(_Fun, Forms) ->
    Forms.

map_file_diagnostics(Fun, FileDiagnostics) ->
    [{File, lists:map(Fun, Diagnostics)}
     || {File, Diagnostics} <- FileDiagnostics].

add_cleanup_error(_File, ok, Return) ->
    Return;
add_cleanup_error(File, {error, Error}, Return) ->
    astranaut_return:to_compiler(
      astranaut_return:with_error(
        fun(ErrorStruct) ->
                astranaut_error:append_file_errors(
                  [{File, [{0, astranaut_macro, Error}]}],
                  ErrorStruct)
        end,
        astranaut_return:from_compiler(Return))).

%% Execute pure plan data in two explicit stages. ExpansionValidator prepares
%% canonical forms first; GenerationCompiler then consumes only those forms.
-spec execute_plan([compilation_boundary()], workflow_context(), state()) ->
          astranaut_return:struct(state()).
execute_plan([], _WorkflowContext, State) ->
    astranaut_return:return(State);
execute_plan([Boundary | Rest], WorkflowContext, State) ->
    Members = maps:get(members, Boundary),
    case Members of
        [] ->
            execute_plan(Rest, WorkflowContext, State);
        _ ->
            do([ return ||
                   PreparedState <- prepare_requests(
                                      maps:get(requests, Boundary),
                                      WorkflowContext, State),
                   State1 <- compile_boundary(
                               Boundary, WorkflowContext, PreparedState),
                   execute_plan(Rest, WorkflowContext, State1)
               ])
    end.

-spec compile_boundary(compilation_boundary(), workflow_context(), state()) ->
          astranaut_return:struct(state()).
compile_boundary(#{members := Members, requests := Requests} = Boundary,
                 WorkflowContext, State) ->
    BoundaryKey = generation_boundary_key(Boundary),
    case maps:is_key(
           BoundaryKey, maps:get(committed_boundaries, State, #{})) of
        true ->
            astranaut_return:return(State);
        false ->
            SourceView = boundary_source_view(Requests, WorkflowContext),
            Module = astranaut_lib:analyze_forms_module(SourceView),
            with_generation_lock(
              Module,
              fun() ->
              RequiredIds = lists:foldl(
                              fun(Request, Acc) ->
                                      ordsets:union(
                                        maps:get(closure_ids, Request), Acc)
                              end, ordsets:new(), Requests),
              Canonical = maps:get(canonical_expanded_forms, State, #{}),
              case [Id || Id <- RequiredIds, not maps:is_key(Id, Canonical)] of
                  [] ->
                      BoundaryForms = maps:with(RequiredIds, Canonical),
                      CompiledForms = maps:merge(
                                        maps:get(compiled_forms, State),
                                        BoundaryForms),
                      HasFormatter = has_function(
                                       format_error, 1, SourceView),
                      LocalFunctions = maybe_add_formatter(
                                         ordsets:from_list(Members),
                                         HasFormatter),
                      Related0 = lists:foldl(
                                   fun(Request, Acc) ->
                                           ordsets:union(
                                             maps:get(closure_fas, Request), Acc)
                                   end, ordsets:new(), Requests),
                      Related = maybe_add_formatter(
                                  Related0, HasFormatter),
                      do([ return ||
                             load_local_macro_forms(
                               LocalFunctions, Related, CompiledForms,
                               SourceView,
                               maps:get(compile_opts, WorkflowContext)),
                             State1 = commit_compiled(
                                        Members, CompiledForms, State),
                             Committed = maps:put(
                                           BoundaryKey,
                                           maps:get(generation, State1),
                                           maps:get(committed_boundaries,
                                                    State1, #{})),
                             return(State1#{committed_boundaries => Committed})
                         ]);
                  [Missing | _] ->
                      astranaut_return:error_fail(
                        {missing_canonical_local_macro_form, Missing})
              end
              end)
    end.

-spec generation_boundary_key(compilation_boundary()) -> [fa()].
generation_boundary_key(#{members := Members}) ->
    Members.

-spec boundary_source_view([expansion_request()], workflow_context()) -> [term()].
boundary_source_view([], WorkflowContext) ->
    maps:get(source_view, WorkflowContext);
boundary_source_view(Requests, _WorkflowContext) ->
    maps:get(source_view, lists:last(Requests)).

-spec prepare_requests([expansion_request()], workflow_context(), state()) ->
          astranaut_return:struct(state()).
prepare_requests(Requests, WorkflowContext, State) ->
    astranaut_return:foldl_m(
      fun(Request, StateAcc) ->
              prepare_request(Request, WorkflowContext, StateAcc)
      end, State, Requests).

-spec prepare_request(expansion_request(), workflow_context(), state()) ->
          astranaut_return:struct(state()).
prepare_request(#{forms := FrozenForms} = Request, WorkflowContext, State) ->
    RecordForms = record_forms(maps:get(source_view, Request)),
    astranaut_return:foldl_m(
      fun(FormId, StateAcc) ->
              do([ return ||
                     {_ExpandedForm, State1} <-
                         prepare_request_form(
                           FormId, RecordForms, Request,
                           WorkflowContext, StateAcc),
                     return(State1)
                 ])
      end, State, maps:keys(FrozenForms)).

-spec prepare_request_form(form_id(), [term()], expansion_request(),
                           workflow_context(), state()) ->
          astranaut_return:struct({term(), state()}).
prepare_request_form(
  FormId, RecordForms,
  #{candidate_local_macros := Candidates,
    macro_environment_snapshot :=
        #{macro_map := SnapshotMacroMap,
          macro_options := MacroOptions}} = Request,
  WorkflowContext, State) ->
    WhitelistControl = request_whitelist_control(FormId, State),
    LocalVersions = local_versions(
                      fingerprint_local_fas(
                        WhitelistControl, Candidates), State),
    EffectiveMacroMap0 = keep_allowed_local_fas(
                           SnapshotMacroMap, Candidates),
    EffectiveMacroMap = mark_local_macro_callable(
                          EffectiveMacroMap0, State),
    Fingerprint = env_fingerprint(
                    EffectiveMacroMap0, LocalVersions,
                    MacroOptions),
    case expanded_form(FormId, Fingerprint, State) of
        {ok, #{result := Form}} ->
            astranaut_return:return({Form, State});
        error ->
            expand_and_cache_form(
              FormId, EffectiveMacroMap, Fingerprint,
              WhitelistControl, RecordForms, Request, WorkflowContext,
              State)
    end.

request_whitelist_control({spec, _Name, _Arity}, _State) ->
    disabled;
request_whitelist_control(FormId, State) ->
    whitelist_control(FormId, State).

whitelist_control(FormId, State) ->
    case maps:find(FormId, maps:get(expansion_records, State, #{})) of
        {ok, #{canonical_whitelist := Expected}} when Expected =/= disabled ->
            #{mode => verify, form_id => FormId, expected => Expected};
        _ ->
            #{mode => collect, form_id => FormId}
    end.

-spec expand_and_cache_form(form_id(), map(), binary(),
                             local_macro_whitelist_control(),
                             [term()], expansion_request(), workflow_context(),
                             state()) ->
          astranaut_return:struct({term(), state()}).
expand_and_cache_form(FormId, EffectiveMacroMap, Fingerprint,
                      WhitelistControl, RecordForms,
                      #{forms := FrozenForms,
                        function_call_analysis := FunctionCallAnalysis} = Request,
                      WorkflowContext, State) ->
    OriginalForm = maps:get(FormId, FrozenForms),
    case FormId of
        {spec, _Name, _Arity} ->
            cache_form_result(
              FormId, Fingerprint, disabled, OriginalForm, State);
        {function, _Name, _Arity} ->
            Task0 = #{form => OriginalForm,
                      macro_map => EffectiveMacroMap,
                      whitelist_control => WhitelistControl},
            Task = add_macro_call_hint(
                     FormId, OriginalForm,
                     FunctionCallAnalysis, Task0),
            Tasks = #{FormId => Task},
            do([ return ||
                   #{task_results := #{FormId := TaskResult}} <-
                       astranaut_macro_expander:expand_functions(
                         [OriginalForm | RecordForms], Tasks),
                   finish_or_schedule_expansion(
                     TaskResult, FormId, Fingerprint,
                     RecordForms, Request, WorkflowContext, State)
                ])
    end.

add_macro_call_hint(FormId, OriginalForm, FunctionCallAnalysis, Task) ->
    case maps:find(FormId, FunctionCallAnalysis) of
        {ok, #{form := OriginalForm,
               has_macro_call := HasMacroCall}} ->
            Task#{has_macro_call => HasMacroCall};
        _ ->
            Task
    end.

finish_or_schedule_expansion(
  #{needed_local_macros := [_ | _] = Needed}, FormId,
  _Fingerprint, RecordForms, Request, WorkflowContext, State) ->
    do([ return ||
           State1 <- astranaut_return:foldl_m(
                       fun(Dependency, StateAcc) ->
                               need_callable(
                                 Dependency, WorkflowContext,
                                 StateAcc)
                       end, State, Needed),
           prepare_request_form(
             FormId, RecordForms, Request, WorkflowContext, State1)
       ]);
finish_or_schedule_expansion(
  #{form := ExpandedForm,
    local_macro_whitelist := Whitelist}, FormId,
  Fingerprint, _RecordForms, _Request, _WorkflowContext, State) ->
    cache_form_result(
      FormId, Fingerprint, Whitelist, ExpandedForm, State).

record_forms(Forms) ->
    [Form || {attribute, _Pos, record, {_Name, _Fields}} = Form <- Forms].

mark_local_macro_callable(MacroMap, State) ->
    Macros = maps:get(local_macros, State),
    maps:map(
      fun(_Key, #{macro_source := local_macro,
                  function := Function, arity := Arity} = Macro) ->
              Callable = case maps:find({Function, Arity}, Macros) of
                             {ok, #{status := compiled}} -> true;
                             _ -> false
                         end,
              Macro#{local_macro_callable => Callable};
         (_Key, Macro) ->
              Macro
      end, MacroMap).

-spec cache_form_result(form_id(), term(), disabled | ordsets:ordset(fa()),
                        term(), state()) ->
          astranaut_return:struct({term(), state()}).
cache_form_result(FormId, Fingerprint, Whitelist, Form, State) ->
    case cache_expanded(FormId, Fingerprint, Whitelist, Form, State) of
        {ok, State1} -> astranaut_return:return({Form, State1});
        {error, Error} -> astranaut_return:error_fail(Error)
    end.

-spec maybe_add_formatter(ordsets:ordset(fa()), boolean()) ->
          ordsets:ordset(fa()).
maybe_add_formatter([], _HasFormatter) -> [];
maybe_add_formatter(Functions, true) ->
    ordsets:add_element({format_error, 1}, Functions);
maybe_add_formatter(Functions, false) ->
    Functions.

has_function(Name, Arity, Forms) ->
    lists:any(
      fun({function, _Pos, Name0, Arity0, _Clauses}) ->
              Name0 =:= Name andalso Arity0 =:= Arity;
         (_Form) ->
              false
      end, Forms).

-spec materialize_forms([term()], #{form_id() => term()}) -> [term()].
materialize_forms(Forms, CompiledForms) ->
    lists:map(
      fun(Form) ->
              case form_id(Form) of
                  undefined -> Form;
                  Id -> maps:get(Id, CompiledForms, Form)
              end
      end, Forms).

-spec module_name(module()) -> module().
module_name(Module) ->
    list_to_atom(atom_to_list(Module) ++ "__local_macro").

-spec select_local_macro_forms(ordsets:ordset(fa()), [term()]) -> [term()].
select_local_macro_forms(LocalMacroRelatedFunctions, Forms) ->
    lists:reverse(
      lists:foldl(
        fun({attribute, Pos, module, Module}, Acc) ->
                [{attribute, Pos, ?LOCAL_MACRO_OWNER_ATTR, Module},
                 {attribute, Pos, module, module_name(Module)} | Acc];
           ({function, _Pos, Name, Arity, _Clauses} = Node, Acc) ->
                append_if(ordsets:is_element({Name, Arity}, LocalMacroRelatedFunctions), Node, Acc);
           ({attribute, _Pos, spec, {{Name, Arity}, _Body}} = Node, Acc) ->
                append_if(ordsets:is_element({Name, Arity}, LocalMacroRelatedFunctions), Node, Acc);
           ({attribute, _Pos, export, _Exports}, Acc) -> Acc;
           ({attribute, _Pos, local_macro, _Attr}, Acc) -> Acc;
           ({attribute, _Pos, import_macro, _Attr}, Acc) -> Acc;
           ({attribute, _Pos, use_macro, _Attr}, Acc) -> Acc;
           ({attribute, _Pos, macro_options, _Attr}, Acc) -> Acc;
           ({attribute, _Pos, exec_macro, _Attr}, Acc) -> Acc;
           (Node, Acc) -> [Node | Acc]
        end, [], Forms)).

-spec compile_local_macro_forms(ordsets:ordset(fa()), [term()],
                                [compile:option()]) ->
          astranaut_return:struct(term()).
compile_local_macro_forms(LocalMacroFunctions, Forms, CompileOpts) ->
    Forms1 = astranaut_forms:sort_forms(Forms ++ local_macro_exports(LocalMacroFunctions)),
    Module = astranaut_lib:analyze_forms_module(Forms),
    safe_load_locked(Module, Forms1, [without_warnings | CompileOpts]).

-spec local_macro_exports(ordsets:ordset(fa())) -> [term()].
local_macro_exports(LocalMacroFunctions) ->
    lists:foldl(
      fun(Export, Acc) ->
              [astranaut_lib:gen_exports([Export], 0) | Acc]
      end, [], LocalMacroFunctions).

append_if(true, Form, Forms) -> [Form | Forms];
append_if(false, _Form, Forms) -> Forms.

%% Compile then safely replace the single local-macro module generation.  This
%% has no force-purge fallback: callers retain their previous generation when
%% old code is in use.
-spec safe_load(module(), [term()], [compile:option()]) -> astranaut_return:struct({module(), binary()}).
safe_load(Module, Forms, CompileOpts) ->
    astranaut_lib:with_module_lock(
      Module, fun() -> safe_load_locked(Module, Forms, CompileOpts) end).

safe_load_locked(Module, Forms, CompileOpts) ->
    astranaut_return:bind(
      astranaut_lib:compile_forms(Forms, CompileOpts),
      fun({CompiledModule, Binary}) when CompiledModule =:= Module ->
              case astranaut_lib:reload_binary(Module, Binary) of
                  {ok, Result} ->
                      astranaut_return:return(Result);
                  {error, {module_in_use, Module}} ->
                      astranaut_return:error_fail(
                        local_macro_module_in_use);
                  {error, Reason} ->
                      astranaut_return:error_fail(Reason)
              end
      end).

ensure_local_macro_module_available(Module) ->
    LocalModule = module_name(Module),
    case code:is_loaded(LocalModule) of
        false ->
            case code:which(LocalModule) of
                non_existing -> ok;
                _ -> {error, {local_macro_module_name_conflict, LocalModule}}
            end;
        {file, _} ->
            case local_macro_module_owner(LocalModule) of
                {ok, Module} -> ok;
                _ -> {error, {local_macro_module_name_conflict, LocalModule}}
            end
    end.

local_macro_module_owner(Module) ->
    case code:is_loaded(Module) of
        false ->
            not_loaded;
        {file, _} ->
            try proplists:get_value(
                  ?LOCAL_MACRO_OWNER_ATTR,
                  Module:module_info(attributes)) of
                [Owner] when is_atom(Owner) -> {ok, Owner};
                _ -> unknown
            catch
                error:undef -> not_loaded
            end
    end.

unload_local_macro_module(Module) ->
    case code:soft_purge(Module) of
        false ->
            {error, local_macro_module_in_use};
        true ->
            unload_current_local_macro_module(Module)
    end.

unload_current_local_macro_module(Module) ->
    case code:delete(Module) of
        true ->
            case code:soft_purge(Module) of
                true -> ok;
                false -> {error, local_macro_module_in_use}
            end;
        false ->
            case code:is_loaded(Module) of
                false -> ok;
                {file, _} -> {error, local_macro_module_in_use}
            end
    end.

local_macro_module_error(Module, Forms, Error) ->
    File =
        case astranaut_lib:analyze_forms_file(Forms) of
            undefined -> atom_to_list(Module) ++ ".erl";
            SourceFile -> SourceFile
        end,
    astranaut_return:from_compiler(
      {error, [{File, [{0, astranaut_macro, Error}]}], []}).

duplicate_or_existing(FAs, Macros) ->
    case [FA || FA <- FAs, maps:is_key(FA, Macros)] ++ duplicate_fas(FAs) of
        [FA | _] -> FA;
        [] -> none
    end.

duplicate_fas(FAs) ->
    FAs -- ordsets:from_list(FAs).

next_order(Macros) ->
    lists:foldl(fun(#{order := Order}, Acc) -> max(Order, Acc) end, 0, maps:values(Macros)) + 1.

-spec forms_id_map([term()]) -> #{form_id() => term()}.
forms_id_map(Forms) ->
    lists:foldl(fun(Form, Acc) ->
                        case form_id(Form) of undefined -> Acc; Id -> maps:put(Id, Form, Acc) end
                end, #{}, Forms).

form_id({function, _Pos, Name, Arity, _Clauses}) -> {function, Name, Arity};
form_id({attribute, _Pos, spec, {{Name, Arity}, _Body}}) -> {spec, Name, Arity};
form_id(_) -> undefined.

closures(FAs, Options, FormMap, MacroMap) ->
    ClosureRoots = maps:get(closure_roots, Options, []),
    case [FA || FA <- ClosureRoots,
                not maps:is_key(
                      {function, element(1, FA), element(2, FA)}, FormMap)] of
        [] -> closures_1(
                FAs, ClosureRoots, FormMap, MacroMap, #{}, #{});
        Missing -> {error, {invalid_closure_roots, Missing}}
    end.

closures_1([FA | T], Extra, FormMap, MacroMap,
           FunctionCallAnalysis, Acc) ->
    {ok, Closure, FunctionCallAnalysis1} =
        closure(
          ordsets:from_list([FA | Extra]), FormMap,
          MacroMap, FunctionCallAnalysis, ordsets:new()),
    closures_1(
      T, Extra, FormMap, MacroMap, FunctionCallAnalysis1,
      maps:put(FA, Closure, Acc));
closures_1([], _Extra, _FormMap, _MacroMap,
           FunctionCallAnalysis, Acc) ->
    {ok, Acc, FunctionCallAnalysis}.

closure([], _FormMap, _MacroMap, FunctionCallAnalysis, Seen) ->
    {ok, Seen, FunctionCallAnalysis};
closure([FA | T], FormMap, MacroMap, FunctionCallAnalysis, Seen) ->
    case ordsets:is_element(FA, Seen) of
        true ->
            closure(T, FormMap, MacroMap, FunctionCallAnalysis, Seen);
        false ->
            FormId = {function, element(1, FA), element(2, FA)},
            case maps:find(FormId, FormMap) of
                error ->
                    closure(
                      T, FormMap, MacroMap,
                      FunctionCallAnalysis, Seen);
                {ok, Function} ->
                    {Analysis, FunctionCallAnalysis1} =
                        analyze_closure_function(
                          FormId, Function, MacroMap,
                          FunctionCallAnalysis),
                    LocalCalls = maps:get(local_calls, Analysis),
                    closure(
                      ordsets:union(LocalCalls, T), FormMap,
                      MacroMap, FunctionCallAnalysis1,
                      ordsets:add_element(FA, Seen))
            end
    end.

analyze_closure_function(
  FormId, Function, MacroMap, FunctionCallAnalysis) ->
    case maps:find(FormId, FunctionCallAnalysis) of
        {ok, Analysis} ->
            {Analysis, FunctionCallAnalysis};
        error ->
            AnalysisMap =
                astranaut_macro_expander:function_call_analysis(
                  [Function], MacroMap),
            Analysis = maps:get(FormId, AnalysisMap),
            {Analysis,
             maps:put(FormId, Analysis, FunctionCallAnalysis)}
    end.

closure_function_analysis(Closure, FunctionCallAnalysis) ->
    FunctionIds = [{function, Name, Arity}
                   || {Name, Arity} <- Closure],
    maps:with(FunctionIds, FunctionCallAnalysis).

referenced_local_macros(FunctionCallAnalysis) ->
    maps:fold(
      fun(_FormId, Analysis, Acc) ->
              ordsets:union(
                maps:get(local_macro_calls, Analysis), Acc)
      end, ordsets:new(), FunctionCallAnalysis).

freeze_closures(Closures, FormMap) ->
    lists:foldl(fun(Closure, Acc) ->
                        lists:foldl(fun(FA, A) -> freeze_fa(FA, FormMap, A) end, Acc, Closure)
                end, #{}, maps:values(Closures)).

freeze_fa({Name, Arity}, FormMap, Acc) ->
    FunctionId = {function, Name, Arity},
    SpecId = {spec, Name, Arity},
    Acc1 = case maps:find(FunctionId, FormMap) of {ok, FunctionForm} -> maps:put(FunctionId, FunctionForm, Acc); error -> Acc end,
    case maps:find(SpecId, FormMap) of {ok, SpecForm} -> maps:put(SpecId, SpecForm, Acc1); error -> Acc1 end.

closure_ids(Closure, FormMap) ->
    lists:foldl(fun(FA, Acc) ->
                        Ids = [{function, element(1, FA), element(2, FA)}, {spec, element(1, FA), element(2, FA)}],
                        ordsets:union([Id || Id <- Ids, maps:is_key(Id, FormMap)], Acc)
                end, ordsets:new(), Closure).

needed_entries(all, Macros) -> maps:to_list(Macros);
needed_entries(FA, Macros) ->
    case maps:find(FA, Macros) of
        error -> {error, {undefined_local_macro, FA}};
        {ok, #{order := RequestedOrder}} ->
            %% A requested declaration is compiled in the cumulative prefix;
            %% dependency analysis decides only whether that prefix needs
            %% intermediate generations, never which declarations belong to
            %% the final generation.
            [{F, Entry} || {F, #{order := Order} = Entry} <- maps:to_list(Macros),
                           Order =< RequestedOrder]
    end.

-spec compilation_boundaries(fa() | all, [{non_neg_integer(), fa(), map()}],
                             #{form_id() => term()}) ->
          [compilation_boundary()].
compilation_boundaries(Needed, Ordered, Frozen) ->
    Index = compilation_plan_index(Ordered),
    Prefix = [FA || {_Order, FA, _Entry} <- Ordered],
    DependencyRoots = case Needed of
                          all -> Prefix;
                          FA -> [FA]
                      end,
    {Dependencies, _Memo} = dependency_boundaries_for_fas(
                              DependencyRoots, Index, #{}),
    Boundaries = ordsets:to_list(ordsets:from_list(Dependencies ++ [Prefix])),
    %% ordsets sorts lists lexically, not declaration order; restore the plan
    %% order by the final member's declaration index.
    Sorted = lists:sort(
               fun(A, B) ->
                       boundary_order(A, Index) =< boundary_order(B, Index)
               end,
               Boundaries),
    [plan_boundary(Members, Index, Frozen) || Members <- Sorted].

compilation_plan_index(Ordered) ->
    #{entries => maps:from_list(
                    [{FA, Entry} || {_Order, FA, Entry} <- Ordered]),
      orders => maps:from_list(
                  [{FA, Order} || {Order, FA, _Entry} <- Ordered]),
      prefixes => maps:from_list(
                    [{FA, [PrefixFA
                           || {PrefixOrder, PrefixFA, _PrefixEntry} <- Ordered,
                              PrefixOrder =< Order]}
                     || {Order, FA, _OuterEntry} <- Ordered])}.

dependency_boundaries_for_fas(FAs, Index, Memo) ->
    {Reversed, Memo1} =
        lists:foldl(
          fun(FA, {BoundariesAcc, MemoAcc}) ->
                  {Boundaries, MemoNext} = dependency_boundaries(
                                               FA, Index, MemoAcc),
                  {[Boundaries | BoundariesAcc], MemoNext}
          end, {[], Memo}, FAs),
    {lists:append(lists:reverse(Reversed)), Memo1}.

dependency_boundaries(FA, #{entries := Entries,
                            prefixes := Prefixes} = Index, Memo) ->
    case maps:find(FA, Memo) of
        {ok, Boundaries} ->
            {Boundaries, Memo};
        error ->
            Entry = maps:get(FA, Entries),
            Dependencies = maps:get(referenced_local_macros, Entry, []),
            {ReversedChunks, Memo1} =
                lists:foldl(
                  fun(Dep, {ChunksAcc, MemoAcc}) ->
                          {Nested, MemoNext} = dependency_boundaries(
                                                   Dep, Index, MemoAcc),
                          Chunk = Nested ++ [maps:get(Dep, Prefixes)],
                          {[Chunk | ChunksAcc], MemoNext}
                  end, {[], Memo}, Dependencies),
            Boundaries = lists:append(lists:reverse(ReversedChunks)),
            {Boundaries, maps:put(FA, Boundaries, Memo1)}
    end.

boundary_order(Members, #{orders := Orders}) ->
    maps:get(lists:last(Members), Orders).

-spec plan_boundary([fa()], map(),
                    #{form_id() => term()}) -> compilation_boundary().
plan_boundary(Members, #{entries := EntryIndex}, Frozen) ->
    Entries = [{FA, maps:get(FA, EntryIndex)} || FA <- Members],
    #{members => Members,
      requests => [request_for_entry(Entry, Frozen)
                   || {_FA, Entry} <- Entries]}.

-spec requests_for_fas([fa()], state()) -> [expansion_request()].
requests_for_fas(FAs, State) ->
    Macros = maps:get(local_macros, State),
    Frozen = maps:get(frozen_forms, State),
    [request_for_entry(maps:get(FA, Macros), Frozen) || FA <- FAs].

-spec request_for_entry(map(), #{form_id() => term()}) ->
          expansion_request().
request_for_entry(Entry, Frozen) ->
    #{closure_ids => maps:get(closure_ids, Entry),
      closure_fas => maps:get(closure_fas, Entry),
      candidate_local_macros => maps:get(candidate_local_macros, Entry),
      function_call_analysis =>
          maps:get(function_call_analysis, Entry),
      referenced_local_macros => maps:get(referenced_local_macros, Entry),
      macro_environment_snapshot =>
          maps:get(macro_environment_snapshot, Entry),
      source_view => maps:get(source_view, Entry),
      forms => maps:with(maps:get(closure_ids, Entry), Frozen)}.

retained_ids(Roots, Macros) ->
    lists:foldl(fun(Root, Acc) ->
                        lists:foldl(fun({_FA, Entry}, A) ->
                                            Closure = maps:get(closure_fas, Entry),
                                            case ordsets:is_element(Root, Closure) of
                                                true -> ordsets:union(maps:get(closure_ids, Entry), A);
                                                false -> A
                                            end
                                    end, Acc, maps:to_list(Macros))
                end, ordsets:new(), Roots).
