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

-export([new/0, register/6, prepare_declaration/4,
         need_callable/4,
         compile_plan/2,
         cache_expanded/5, commit_compiled/3, finalize/2,
         finalize/4,
         frozen_ids/1, local_macros/1,
         retained_form_ids/1, nonclosure_retain_roots/2,
         expand_final_functions/5,
         source_view/2, env_fingerprint/4,
         reject_locked_mutation/2, safe_load/3, finalize_plan/1,
         module_name/1,
         form_id/1, materialize_forms/2, execute_plan/4]).

-type fa() :: {atom(), non_neg_integer()}.
-type macro_key() :: {atom() | {module(), atom()}, non_neg_integer()}.
-type internal_macro_binding() :: local | {remote, module(), atom()}.
-type internal_macro_bindings() :: #{macro_key() => internal_macro_binding()}.
-type form_id() :: {function | spec, atom(), non_neg_integer()}.
-type macro_runtime_context() :: #{macro_map := map(),
                                   macro_options := map(),
                                   inject_forms := [term()]}.
-type local_macro_whitelist_control() ::
        astranaut_macro_expander:local_macro_whitelist_control().
-type function_expansion() :: astranaut_macro_expander:function_expansion().
-type expansion_record() ::
        #{canonical_whitelist := disabled | ordsets:ordset(fa()),
          canonical_result := term(),
          results_by_input :=
              #{term() => #{whitelist := disabled | ordsets:ordset(fa()),
                            result := term()}}}.
-type workflow_context() :: #{source_view := [term()],
                              compile_opts := [compile:option()]}.
-type macro_ops() ::
        #{resolve_local_references :=
              fun(([{fa(), map()}], [term()]) -> ordsets:ordset(fa())),
          expand_function :=
              fun((map(), [term()], [term()], fa(),
                   local_macro_whitelist_control()) ->
                      astranaut_return:struct(function_expansion()))}.
-type expansion_request() ::
        #{closure_ids := ordsets:ordset(form_id()),
          closure_fas := ordsets:ordset(fa()),
          candidate_local_macros := ordsets:ordset(fa()),
          referenced_local_macros := ordsets:ordset(fa()),
          internal_macro_bindings := internal_macro_bindings(),
          runtime_context_snapshot := macro_runtime_context(),
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

-spec register([fa()], map(), [term()], macro_runtime_context(), macro_ops(),
               state()) ->
          {ok, state()} | {error, term()}.
register(FAs, Options, SourceView,
         #{macro_map := MacroMap,
           macro_options := _, inject_forms := _} = RuntimeContext,
         MacroOps, State)
  when is_list(FAs), is_map(Options), is_map(MacroOps) ->
    Resolve = maps:get(resolve_local_references, MacroOps),
    CandidateLocalMap = local_macro_entries(MacroMap),
    case resolve_internal_macro_bindings(Options, MacroMap) of
        {ok, InternalBindings} ->
            do_register(FAs, Options, SourceView, RuntimeContext,
                        CandidateLocalMap, InternalBindings, Resolve, State);
        {error, _} = Error ->
            Error
    end.

-spec do_register([fa()], map(), [term()], macro_runtime_context(),
                  map(), internal_macro_bindings(),
                  fun(([{fa(), map()}], [term()]) -> [fa()]),
                  state()) ->
          {ok, state()} | {error, term()}.
do_register(FAs, Options, SourceView, RuntimeContext,
            CandidateLocalMap, InternalBindings, Resolve, State) ->
    Macros = maps:get(local_macros, State),
    case duplicate_or_existing(FAs, Macros) of
        none ->
            FormMap = forms_id_map(SourceView),
            case closures(FAs, Options, FormMap) of
                {ok, Closures} ->
                    case validate_internal_policies(
                           Closures, InternalBindings, Macros) of
                        ok ->
                            Order = next_order(Macros),
                            Frozen = maps:merge(maps:get(frozen_forms, State),
                                                freeze_closures(Closures, FormMap)),
                            NewMacros = lists:foldl(
                                          fun(FA, Acc) ->
                                                  Closure = maps:get(FA, Closures),
                                                  CandidateEnv = maps:without(
                                                                   maps:keys(
                                                                     InternalBindings),
                                                                   CandidateLocalMap),
                                                  CandidateFAs = local_macro_fas(
                                                                   CandidateEnv),
                                                  TargetEnvs =
                                                      [{TargetFA, CandidateEnv}
                                                       || TargetFA <- Closure],
                                                  Refs = Resolve(
                                                           TargetEnvs,
                                                           SourceView),
                                                  maps:put(FA, #{order => Order,
                                                                 runtime_context_snapshot =>
                                                                     RuntimeContext,
                                                                 closure_ids => closure_ids(Closure, FormMap),
                                                                 closure_fas => Closure,
                                                                 candidate_local_macros =>
                                                                     CandidateFAs,
                                                                 referenced_local_macros => Refs,
                                                                 internal_macro_bindings =>
                                                                     InternalBindings,
                                                                 source_view => SourceView,
                                                                 options => Options,
                                                                 status => pending}, Acc)
                                          end, Macros, FAs),
                            {ok, State#{local_macros => NewMacros,
                                       frozen_forms => Frozen}};
                        {error, _} = Error -> Error
                    end;
                {error, _} = Error -> Error
            end;
        FA ->
            {error, {duplicate_local_macro_declaration, FA}}
    end.

-spec resolve_internal_macro_bindings(map(), map()) ->
          {ok, internal_macro_bindings()} | {error, term()}.
resolve_internal_macro_bindings(#{internal_function := true}, MacroMap) ->
    {ok, maps:fold(fun add_internal_macro_binding/3, #{}, MacroMap)};
resolve_internal_macro_bindings(#{internal_function := Refs}, MacroMap)
  when is_list(Refs) ->
    {Bindings, Missing} =
        lists:foldl(
          fun(Ref, {BindingsAcc, MissingAcc}) ->
                  Key = internal_macro_key(Ref),
                  case maps:find(Key, MacroMap) of
                      {ok, Macro} ->
                          {add_internal_macro_binding(
                             Key, Macro, BindingsAcc),
                           MissingAcc};
                      error ->
                          {BindingsAcc, [Ref | MissingAcc]}
                  end
          end, {#{}, []}, Refs),
    case Missing of
        [] -> {ok, Bindings};
        _ -> {error, {undefined_internal_functions, lists:reverse(Missing)}}
    end;
resolve_internal_macro_bindings(_Options, _MacroMap) ->
    {ok, #{}}.

internal_macro_key({Function, Arity}) ->
    {Function, Arity};
internal_macro_key({Module, Function, Arity}) ->
    {{Module, Function}, Arity}.

add_internal_macro_binding(Key, Macro, Bindings) ->
    Binding = internal_macro_binding(Key, Macro),
    Bindings1 = maps:put(Key, Binding, Bindings),
    case {Key, Binding} of
        {{Alias, Arity}, {remote, Module, Function}}
          when is_atom(Alias) ->
            maps:put({{Module, Function}, Arity}, Binding, Bindings1);
        _ ->
            Bindings1
    end.

internal_macro_binding(_Key, #{macro_source := local_macro}) ->
    local;
internal_macro_binding(_Key,
                       #{macro_module := Module, function := Function}) ->
    {remote, Module, Function};
internal_macro_binding({{Module, Function}, _Arity}, _Macro) ->
    {remote, Module, Function};
internal_macro_binding(_Key, _Macro) ->
    local.

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
-spec prepare_declaration([fa()], workflow_context(), macro_ops(), state()) ->
          astranaut_return:struct(state()).
prepare_declaration(FAs, WorkflowContext, MacroOps, State) ->
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
                                 Dependency, WorkflowContext, MacroOps, StateAcc)
                       end, State, Dependencies),
           Requests = requests_for_fas(FAs, State1),
           State2 <- prepare_requests(
                       Requests, WorkflowContext, MacroOps, State1),
           return(State2)
       ]).

-spec need_callable(fa(), workflow_context(), macro_ops(), state()) ->
          astranaut_return:struct(state()).
need_callable(FA, WorkflowContext, MacroOps, State) ->
    case compile_plan(FA, State) of
        {ok, Plan} -> execute_plan(Plan, WorkflowContext, MacroOps, State);
        {error, Error} -> astranaut_return:error_fail(Error)
    end.

%% A plan is deliberately pure. execute_plan/4 expands each requested frozen
%% form through MacroOps, validates the cache, then commits it atomically.
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
-spec finalize_plan(state()) -> {ok, [compilation_boundary()]}.
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

-spec finalize([fa()], workflow_context(), macro_ops(), state()) ->
          astranaut_return:struct({map(), ordsets:ordset(form_id()), state()}).
finalize(RetainRoots, WorkflowContext, MacroOps, State) ->
    case finalize_plan(State) of
        {error, Error} ->
            astranaut_return:error_fail(Error);
        {ok, Plan} ->
            do([ return ||
                   State1 <- execute_plan(Plan, WorkflowContext, MacroOps, State),
                   {FinalLocalEnv, FinalSkipIds, State2} = finalize(RetainRoots, State1),
                   %% Retain is now only a lifecycle/selection policy.  Retain
                   %% and ordinary functions are expanded together later with
                   %% the single FinalMacroRuntimeContext.
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

%% Retain and ordinary Step-2 functions share this final-context path. Frozen
%% functions verify their local expansion records; ordinary functions use the
%% same expander with whitelist control disabled and do not touch those records.
-spec expand_final_functions([term()], [fa()], macro_runtime_context(),
                             macro_ops(), state()) ->
          astranaut_return:struct({[term()], state()}).
expand_final_functions(Forms, TargetFAs, RuntimeContext, MacroOps, State) ->
    OriginalMap = maps:merge(forms_id_map(Forms), maps:get(frozen_forms, State)),
    astranaut_return:foldl_m(
      fun(TargetFA, {FormsAcc, StateAcc}) ->
              FormId = {function, element(1, TargetFA), element(2, TargetFA)},
              case maps:find(FormId, OriginalMap) of
                  error ->
                      astranaut_return:return({FormsAcc, StateAcc});
                  {ok, OriginalForm} ->
                      do([ return ||
                             {ExpandedForm, State1} <-
                                 expand_final_function(
                                   FormId, TargetFA, OriginalForm, Forms,
                                   RuntimeContext, MacroOps, StateAcc),
                             return({materialize_forms(
                                       FormsAcc, #{FormId => ExpandedForm}),
                                     State1})
                         ])
              end
      end, {Forms, State}, TargetFAs).

-spec expand_final_function(form_id(), fa(), term(), [term()],
                            macro_runtime_context(), macro_ops(), state()) ->
          astranaut_return:struct({term(), state()}).
expand_final_function(FormId, TargetFA, OriginalForm, SourceForms,
                      #{macro_map := MacroMap} = RuntimeContext,
                      MacroOps, State) ->
    RuntimeInjectForms = maps:get(inject_forms, RuntimeContext),
    InternalBindings = internal_bindings_for_form(FormId, State),
    MacroMapWithoutInternal = maps:without(
                                maps:keys(InternalBindings), MacroMap),
    WhitelistControl = final_whitelist_control(FormId, State),
    AllowedLocalFAs = final_allowed_local_fas(
                        WhitelistControl,
                        local_macro_fas(MacroMapWithoutInternal)),
    EffectiveMacroMap = keep_allowed_local_fas(
                          MacroMapWithoutInternal, AllowedLocalFAs),
    FingerprintLocalFAs = fingerprint_local_fas(
                            WhitelistControl, AllowedLocalFAs),
    LocalVersions = local_versions(FingerprintLocalFAs, State),
    Fingerprint = env_fingerprint(
                    EffectiveMacroMap, LocalVersions,
                    fingerprint_options(
                      maps:get(macro_options, RuntimeContext),
                      InternalBindings),
                    RuntimeInjectForms),
    case cached_final_expansion(
           FormId, Fingerprint, WhitelistControl, State) of
        {ok, ExpandedForm} ->
            astranaut_return:return({ExpandedForm, State});
        error ->
            Expand = maps:get(expand_function, MacroOps),
            RewrittenForm = rewrite_internal_macro_calls(
                              OriginalForm, InternalBindings),
            ExpansionSource = materialize_forms(
                                SourceForms, #{FormId => RewrittenForm}),
            do([ return ||
                   #{forms := ExpandedSource,
                     local_macro_whitelist := Whitelist} <-
                       Expand(EffectiveMacroMap, RuntimeInjectForms,
                              ExpansionSource, TargetFA,
                              WhitelistControl),
                   ExpandedMap = forms_id_map(ExpandedSource),
                   ExpandedForm = maps:get(FormId, ExpandedMap, RewrittenForm),
                   cache_final_expansion(
                     FormId, Fingerprint, Whitelist, ExpandedForm,
                     WhitelistControl, State)
               ])
    end.

internal_bindings_for_form(FormId, State) ->
    Macros = maps:get(local_macros, State),
    case maps:find(form_fa(FormId), Macros) of
        {ok, RootEntry} ->
            maps:get(internal_macro_bindings, RootEntry, #{});
        error ->
            case [maps:get(internal_macro_bindings, Entry, #{})
                  || {_FA, Entry} <- maps:to_list(Macros),
                     ordsets:is_element(
                       FormId, maps:get(closure_ids, Entry))] of
                [Bindings | _] -> Bindings;
                [] -> #{}
            end
    end.

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

%% Keep all observable expansion inputs in the key.  `term_to_binary' gives a
%% stable value suitable for map keys without assuming a particular Env shape.
-spec env_fingerprint(map(), term(), term(), [term()]) -> binary().
env_fingerprint(MacroMap, LocalVersions, Options, InjectForms) ->
    term_to_binary({MacroMap, LocalVersions, Options, InjectForms}).

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
    Forms = select_local_macro_forms(LocalMacroRelatedFunctions, MaterializedForms),
    compile_local_macro_forms(LocalMacroFunctions, Forms, CompileOpts).

-spec with_generation_lock(module(), fun(() -> Result)) -> Result.
with_generation_lock(Module, Fun) ->
    global:trans({?MODULE, module_name(Module)}, Fun).

%% Execute pure plan data in two explicit stages. ExpansionValidator prepares
%% canonical forms first; GenerationCompiler then consumes only those forms.
-spec execute_plan([compilation_boundary()], workflow_context(), macro_ops(),
                   state()) -> astranaut_return:struct(state()).
execute_plan([], _WorkflowContext, _MacroOps, State) ->
    astranaut_return:return(State);
execute_plan([Boundary | Rest], WorkflowContext, MacroOps, State) ->
    Members = maps:get(members, Boundary),
    case Members of
        [] ->
            execute_plan(Rest, WorkflowContext, MacroOps, State);
        _ ->
            do([ return ||
                   PreparedState <- prepare_requests(
                                      maps:get(requests, Boundary),
                                      WorkflowContext, MacroOps, State),
                   State1 <- compile_boundary(
                               Boundary, WorkflowContext, PreparedState),
                   execute_plan(Rest, WorkflowContext, MacroOps, State1)
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
                      LocalFunctions = maybe_add_formatter(
                                         ordsets:from_list(Members), SourceView),
                      Related0 = lists:foldl(
                                   fun(Request, Acc) ->
                                           ordsets:union(
                                             maps:get(closure_fas, Request), Acc)
                                   end, ordsets:new(), Requests),
                      Related = maybe_add_formatter(Related0, SourceView),
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

-spec prepare_requests([expansion_request()], workflow_context(), macro_ops(),
                       state()) ->
          astranaut_return:struct(state()).
prepare_requests(Requests, WorkflowContext, MacroOps, State) ->
    astranaut_return:foldl_m(
      fun(Request, StateAcc) ->
              prepare_request(Request, WorkflowContext, MacroOps, StateAcc)
      end, State, Requests).

-spec prepare_request(expansion_request(), workflow_context(), macro_ops(),
                      state()) ->
          astranaut_return:struct(state()).
prepare_request(#{forms := FrozenForms} = Request, WorkflowContext,
                MacroOps, State) ->
    astranaut_return:foldl_m(
      fun(FormId, StateAcc) ->
              do([ return ||
                     {_ExpandedForm, State1} <-
                         prepare_request_form(
                           FormId, Request, WorkflowContext,
                           MacroOps, StateAcc),
                     return(State1)
                 ])
      end, State, maps:keys(FrozenForms)).

-spec prepare_request_form(form_id(), expansion_request(), workflow_context(),
                           macro_ops(), state()) ->
          astranaut_return:struct({term(), state()}).
prepare_request_form(
  FormId,
  #{candidate_local_macros := Candidates,
    internal_macro_bindings := InternalBindings,
    runtime_context_snapshot :=
        #{macro_map := SnapshotMacroMap,
          macro_options := MacroOptions,
          inject_forms := InjectFormsSnapshot}} = Request,
  WorkflowContext, MacroOps, State) ->
    TargetFA = form_fa(FormId),
    WhitelistControl = request_whitelist_control(FormId, State),
    LocalVersions = local_versions(
                      fingerprint_local_fas(
                        WhitelistControl, Candidates), State),
    MacroMapWithoutInternal = maps:without(
                                maps:keys(InternalBindings),
                                SnapshotMacroMap),
    EffectiveMacroMap0 = keep_allowed_local_fas(
                           MacroMapWithoutInternal, Candidates),
    EffectiveMacroMap = mark_local_macro_callable(
                          EffectiveMacroMap0, State),
    Fingerprint = env_fingerprint(
                    EffectiveMacroMap0, LocalVersions,
                    fingerprint_options(MacroOptions, InternalBindings),
                    InjectFormsSnapshot),
    case expanded_form(FormId, Fingerprint, State) of
        {ok, #{result := Form}} ->
            astranaut_return:return({Form, State});
        error ->
            expand_and_cache_form(
              FormId, TargetFA, EffectiveMacroMap, Fingerprint,
              WhitelistControl, Request, WorkflowContext,
              MacroOps, State)
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

-spec expand_and_cache_form(form_id(), fa(), map(), binary(),
                             local_macro_whitelist_control(),
                             expansion_request(), workflow_context(),
                             macro_ops(), state()) ->
          astranaut_return:struct({term(), state()}).
expand_and_cache_form(FormId, TargetFA, EffectiveMacroMap, Fingerprint,
                      WhitelistControl,
                      #{forms := FrozenForms, source_view := SourceView,
                        internal_macro_bindings := InternalBindings,
                        runtime_context_snapshot :=
                            #{inject_forms := InjectFormsSnapshot}} = Request,
                      WorkflowContext, MacroOps, State) ->
    OriginalForm0 = maps:get(FormId, FrozenForms),
    OriginalForm = rewrite_internal_macro_calls(
                     OriginalForm0, InternalBindings),
    case FormId of
        {spec, _Name, _Arity} ->
            cache_form_result(
              FormId, Fingerprint, disabled, OriginalForm, State);
        {function, _Name, _Arity} ->
            SnapshotForms0 = materialize_forms(SourceView, FrozenForms),
            SnapshotForms = materialize_forms(
                              SnapshotForms0, #{FormId => OriginalForm}),
            Expand = maps:get(expand_function, MacroOps),
            do([ return ||
                   Expansion <-
                       Expand(EffectiveMacroMap, InjectFormsSnapshot,
                              SnapshotForms, TargetFA,
                               WhitelistControl),
                   finish_or_schedule_expansion(
                     Expansion, FormId, TargetFA, OriginalForm, Fingerprint,
                     Request, WorkflowContext, MacroOps, State)
                ])
    end.

fingerprint_options(MacroOptions, InternalBindings) ->
    {MacroOptions, maps:to_list(InternalBindings)}.

rewrite_internal_macro_calls(Form, InternalBindings) ->
    astranaut:smap(
      fun({call, CallPos, {atom, FunctionPos, Function}, Args} = Node) ->
              Key = {Function, length(Args)},
              case maps:find(Key, InternalBindings) of
                  {ok, {remote, Module, OriginalFunction}} ->
                      {call, CallPos,
                       {remote, FunctionPos,
                        {atom, FunctionPos, Module},
                        {atom, FunctionPos, OriginalFunction}},
                       Args};
                  _ ->
                      Node
              end;
         (Node) ->
              Node
      end, Form, #{traverse => post, normalize => false}).

finish_or_schedule_expansion(
  #{needed_local_macros := [_ | _] = Needed}, FormId, _TargetFA,
  _OriginalForm, _Fingerprint, Request, WorkflowContext, MacroOps, State) ->
    do([ return ||
           State1 <- astranaut_return:foldl_m(
                       fun(Dependency, StateAcc) ->
                               need_callable(
                                 Dependency, WorkflowContext,
                                 MacroOps, StateAcc)
                       end, State, Needed),
           prepare_request_form(
             FormId, Request, WorkflowContext, MacroOps, State1)
       ]);
finish_or_schedule_expansion(
  #{forms := ExpandedSource,
    local_macro_whitelist := Whitelist}, FormId, _TargetFA, OriginalForm,
  Fingerprint, _Request, _WorkflowContext, _MacroOps, State) ->
    ExpandedMap = forms_id_map(ExpandedSource),
    ExpandedForm = maps:get(FormId, ExpandedMap, OriginalForm),
    cache_form_result(
      FormId, Fingerprint, Whitelist, ExpandedForm, State).

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

-spec form_fa(form_id()) -> fa().
form_fa({function, Name, Arity}) -> {Name, Arity};
form_fa({spec, Name, Arity}) -> {Name, Arity}.

-spec maybe_add_formatter(ordsets:ordset(fa()), [term()]) ->
          ordsets:ordset(fa()).
maybe_add_formatter([], _Forms) -> [];
maybe_add_formatter(Functions, Forms) ->
    case maps:is_key({function, format_error, 1}, forms_id_map(Forms)) of
        true -> ordsets:add_element({format_error, 1}, Functions);
        false -> Functions
    end.

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
                [{attribute, Pos, module, module_name(Module)} | Acc];
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
    global:trans({?MODULE, Module}, fun() -> safe_load_locked(Module, Forms, CompileOpts) end).

safe_load_locked(Module, Forms, CompileOpts) ->
    astranaut_return:bind(
      astranaut_lib:compile_forms(Forms, CompileOpts),
      fun({CompiledModule, Binary}) when CompiledModule =:= Module ->
              case code:is_loaded(CompiledModule) of
                  false -> load_binary(Module, Binary);
                  {file, _} ->
                      case code:soft_purge(CompiledModule) of
                          true -> load_binary(Module, Binary);
                          false -> astranaut_return:error_fail(local_macro_module_in_use)
                      end
              end
      end).

load_binary(Module, Binary) ->
    case code:load_binary(Module, [], Binary) of
        {module, Module} -> astranaut_return:return({Module, Binary});
        {error, Reason} -> astranaut_return:error_fail(Reason)
    end.

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

closures(FAs, Options, FormMap) ->
    Extra = maps:get(extra_functions, Options, []),
    case [FA || FA <- Extra, not maps:is_key({function, element(1, FA), element(2, FA)}, FormMap)] of
        [] -> closures_1(FAs, Extra, FormMap, #{});
        Missing -> {error, {invalid_extra_functions, Missing}}
    end.

closures_1([FA | T], Extra, FormMap, Acc) ->
    case closure(ordsets:from_list([FA | Extra]), FormMap, ordsets:new()) of
        {ok, Closure} -> closures_1(T, Extra, FormMap, maps:put(FA, Closure, Acc));
        Error -> Error
    end;
closures_1([], _Extra, _FormMap, Acc) -> {ok, Acc}.

closure([], _FormMap, Seen) -> {ok, Seen};
closure([FA | T], FormMap, Seen) ->
    case ordsets:is_element(FA, Seen) of
        true -> closure(T, FormMap, Seen);
        false ->
            case maps:find({function, element(1, FA), element(2, FA)}, FormMap) of
                error -> closure(T, FormMap, Seen);
                {ok, {function, _Pos, _Name, _Arity, Clauses}} ->
                    closure(ordsets:union(local_calls(Clauses), T), FormMap, ordsets:add_element(FA, Seen))
            end
    end.

local_calls(Clauses) ->
    astranaut:sreduce(
      fun({call, _Pos, {atom, _P, Name}, Args}, Acc) -> ordsets:add_element({Name, length(Args)}, Acc);
         (_, Acc) -> Acc
      end, ordsets:new(), Clauses, #{traverse => pre}).

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

validate_internal_policies(Closures, InternalBindings, Existing) ->
    NewPolicies = policy_map(
                    maps:to_list(Closures), InternalBindings, #{}),
    OldPolicies = maps:fold(fun(FA, Entry, Acc) ->
                                    policy_map(
                                      [{FA,
                                        maps:get(closure_fas, Entry)}],
                                      maps:get(internal_macro_bindings,
                                               Entry, #{}),
                                      Acc)
                            end, #{}, Existing),
    case [{FA, Ps} || {FA, Ps} <- maps:to_list(maps:merge_with(fun(_K, A, B) -> ordsets:union(A, B) end, OldPolicies, NewPolicies)),
                      length(Ps) > 1] of
        [] -> ok;
        [{FA, Ps} | _] -> {error, {conflicting_internal_function_policy, FA, Ps}}
    end.

policy_map([{Root, Closure} | T], InternalBindings, Acc) ->
    Policy = maps:to_list(InternalBindings),
    Acc1 = lists:foldl(fun(FA, A) ->
                               case FA =:= Root of
                                   true ->
                                       A;
                                   false ->
                                       Ps = maps:get(
                                              FA, A, ordsets:new()),
                                       maps:put(
                                         FA,
                                         ordsets:add_element(Policy, Ps),
                                         A)
                               end
                       end, Acc, Closure),
    policy_map(T, InternalBindings, Acc1);
policy_map([], _InternalBindings, Acc) -> Acc.

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
compilation_boundaries(all, Ordered, Frozen) ->
    Prefix = [FA || {_Order, FA, _Entry} <- Ordered],
    Dependencies = lists:append(
                     [dependency_boundaries(FA, Ordered) || FA <- Prefix]),
    Boundaries = ordsets:to_list(ordsets:from_list(Dependencies ++ [Prefix])),
    Sorted = lists:sort(
               fun(A, B) -> boundary_order(A, Ordered) =< boundary_order(B, Ordered) end,
               Boundaries),
    [plan_boundary(Members, Ordered, Frozen) || Members <- Sorted];
compilation_boundaries(Needed, Ordered, Frozen) ->
    Prefix = [FA || {_Order, FA, _Entry} <- Ordered],
    Dependencies = dependency_boundaries(Needed, Ordered),
    Boundaries = ordsets:to_list(ordsets:from_list(Dependencies ++ [Prefix])),
    %% ordsets sorts lists lexically, not declaration order; restore the plan
    %% order by the final member's declaration index.
    Sorted = lists:sort(fun(A, B) -> boundary_order(A, Ordered) =< boundary_order(B, Ordered) end, Boundaries),
    [plan_boundary(Members, Ordered, Frozen) || Members <- Sorted].

dependency_boundaries(FA, Ordered) ->
    Entry = entry_for(FA, Ordered),
    lists:append([dependency_boundaries(Dep, Ordered) ++ [prefix_to(Dep, Ordered)]
                  || Dep <- maps:get(referenced_local_macros, Entry, [])]).

entry_for(FA, [{_Order, FA, Entry} | _]) -> Entry;
entry_for(FA, [_ | T]) -> entry_for(FA, T).

prefix_to(FA, Ordered) ->
    DependencyOrder = element(1, lists:keyfind(FA, 2, Ordered)),
    [F || {Order, F, _Entry} <- Ordered, Order =< DependencyOrder].

boundary_order(Members, Ordered) ->
    FA = lists:last(Members),
    element(1, lists:keyfind(FA, 2, Ordered)).

-spec plan_boundary([fa()], [{non_neg_integer(), fa(), map()}],
                    #{form_id() => term()}) -> compilation_boundary().
plan_boundary(Members, Ordered, Frozen) ->
    Entries = [{FA, entry_for(FA, Ordered)} || FA <- Members],
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
      referenced_local_macros => maps:get(referenced_local_macros, Entry),
      internal_macro_bindings => maps:get(internal_macro_bindings, Entry),
      runtime_context_snapshot => maps:get(runtime_context_snapshot, Entry),
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
