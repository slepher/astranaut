%%% Own BEAM module names, cumulative source assembly, loading and generation
%%% state. Inputs contain validated forms and compilation-only snapshots;
%%% macro expansion and declaration environments stay with astranaut_macro_local.
-module(astranaut_macro_local_compile).

-export([
    new/1,
    module_name/1,
    module/1,
    is_callable/2,
    versions/2,
    compiled_ids/1,
    ensure/5,
    remove_forms/2
]).

-type fa() :: {atom(), non_neg_integer()}.
-type form_id() :: {function | spec, atom(), non_neg_integer()}.

-type state() :: #{
    module := module(),
    compiled_forms := #{form_id() => term()},
    committed_boundaries := #{[fa()] => non_neg_integer()},
    versions := #{fa() => non_neg_integer()},
    generation := non_neg_integer()
}.
-type snapshot() :: #{
    closure_ids := ordsets:ordset(form_id()),
    closure_fas := ordsets:ordset(fa()),
    source_view := [term()]
}.
-export_type([state/0]).

-spec new(module()) -> state().
new(Module) ->
    #{
        module => Module,
        compiled_forms => #{},
        committed_boundaries => #{},
        versions => #{},
        generation => 0
    }.

-spec module(state()) -> module().
module(#{module := Module}) -> Module.

-spec is_callable(fa(), state()) -> boolean().
is_callable(FA, #{versions := Versions}) -> maps:is_key(FA, Versions).

-spec versions([fa()], state()) -> #{fa() => non_neg_integer()}.
versions(FAs, #{versions := Versions}) -> maps:with(FAs, Versions).

-spec compiled_ids(state()) -> ordsets:ordset(form_id()).
compiled_ids(#{compiled_forms := Forms}) -> ordsets:from_list(maps:keys(Forms)).

-spec module_name(module()) -> module().
module_name(Module) ->
    Candidate = list_to_atom(
        atom_to_list(Module) ++ "__local_macro__" ++
            integer_to_list(erlang:unique_integer([positive]))
    ),
    case {code:is_loaded(Candidate), code:which(Candidate)} of
        {false, non_existing} -> Candidate;
        _ -> module_name(Module)
    end.

-spec ensure([fa()], [snapshot()], #{form_id() => term()}, map(), state()) ->
    astranaut_return:struct(state()).
ensure(Members, Snapshots, Canonical, Context, #{committed_boundaries := Committed} = State) ->
    case maps:is_key(Members, Committed) of
        true -> astranaut_return:return(State);
        false -> compile_boundary(Members, Snapshots, Canonical, Context, State)
    end.

compile_boundary(
    Members,
    Snapshots,
    Canonical,
    #{compile_opts := CompileOpts, formatter_info := Formatter},
    State
) ->
    RequiredIds = ordsets:union([maps:get(closure_ids, Snapshot) || Snapshot <- Snapshots]),
    case [Id || Id <- RequiredIds, not maps:is_key(Id, Canonical)] of
        [Missing | _] ->
            astranaut_return:error_fail({missing_canonical_local_macro_form, Missing});
        [] ->
            Forms = maps:merge(maps:get(compiled_forms, State), maps:with(RequiredIds, Canonical)),
            Related = ordsets:union([maps:get(closure_fas, Snapshot) || Snapshot <- Snapshots]),
            Functions = ordsets:union([
                ordsets:from_list(Members),
                Related,
                maps:get(related_fas, Formatter)
            ]),
            Exports = ordsets:union(ordsets:from_list(Members), maps:get(exports, Formatter)),
            Source = maps:get(source_view, lists:last(Snapshots)),
            astranaut_return:lift_m(
                fun(_Binary) -> commit(Members, Forms, State) end,
                compile(module(State), Source, Forms, Functions, Exports, CompileOpts)
            )
    end.

%% A generation becomes callable only after both compilation and loading succeed.
commit(
    Members,
    Forms,
    #{
        versions := Versions,
        committed_boundaries := Committed,
        generation := Previous
    } = State
) ->
    Generation = Previous + 1,
    State#{
        compiled_forms => Forms,
        committed_boundaries => maps:put(Members, Generation, Committed),
        versions => lists:foldl(
            fun(FA, Acc) -> maps:put(FA, Generation, Acc) end, Versions, Members
        ),
        generation => Generation
    }.

-spec compile(
    module(),
    [term()],
    #{form_id() => term()},
    ordsets:ordset(fa()),
    ordsets:ordset(fa()),
    [compile:option()]
) -> astranaut_return:struct({module(), binary()}).
compile(Module, SourceForms, CompiledForms, Functions, Exports, CompileOpts) ->
    Materialized = materialize_forms(SourceForms, CompiledForms),
    Selected = select_forms(Module, Functions, Materialized),
    Forms = astranaut_forms:sort_forms(Selected ++ [astranaut_lib:gen_exports(Exports, 0)]),
    astranaut_return:bind(
        astranaut_lib:compile_forms(Forms, [without_warnings | CompileOpts]),
        fun({CompiledModule, Binary}) when CompiledModule =:= Module ->
            case astranaut_lib:reload_binary(Module, Binary) of
                {ok, Result} -> astranaut_return:return(Result);
                {error, Reason} -> astranaut_return:error_fail(Reason)
            end
        end
    ).

materialize_forms(Forms, CompiledForms) ->
    lists:map(
        fun
            ({function, _, Name, Arity, _} = Form) ->
                maps:get({function, Name, Arity}, CompiledForms, Form);
            ({attribute, _, spec, {{Name, Arity}, _}} = Form) ->
                maps:get({spec, Name, Arity}, CompiledForms, Form);
            (Form) ->
                Form
        end,
        Forms
    ).

select_forms(Module, Functions, Forms) ->
    lists:reverse(
        lists:foldl(
            fun
                ({attribute, Pos, module, _}, Acc) ->
                    [{attribute, Pos, module, Module} | Acc];
                ({function, _, Name, Arity, _} = Form, Acc) ->
                    append_if(ordsets:is_element({Name, Arity}, Functions), Form, Acc);
                ({attribute, _, spec, {{Name, Arity}, _}} = Form, Acc) ->
                    append_if(ordsets:is_element({Name, Arity}, Functions), Form, Acc);
                ({attribute, _, export, _}, Acc) ->
                    Acc;
                ({attribute, _, local_macro, _}, Acc) ->
                    Acc;
                ({attribute, _, import_macro, _}, Acc) ->
                    Acc;
                ({attribute, _, use_macro, _}, Acc) ->
                    Acc;
                ({attribute, _, macro_options, _}, Acc) ->
                    Acc;
                ({attribute, _, exec_macro, _}, Acc) ->
                    Acc;
                (Form, Acc) ->
                    [Form | Acc]
            end,
            [],
            Forms
        )
    ).

append_if(true, Form, Forms) -> [Form | Forms];
append_if(false, _Form, Forms) -> Forms.

-spec remove_forms([term()], ordsets:ordset(form_id())) -> [term()].
remove_forms(Forms, FinalSkipIds) ->
    Skip = ordsets:from_list(FinalSkipIds),
    lists:flatmap(
        fun(Form) -> remove_form(Form, Skip) end,
        Forms
    ).

remove_form(
    {attribute, Pos, compile, {nowarn_unused_function, FAs}}, Skip
) ->
    RemainingFAs =
        [
            FA
         || FA = {Name, Arity} <- FAs,
            not ordsets:is_element(
                {function, Name, Arity}, Skip
            )
        ],
    case RemainingFAs of
        [] -> [];
        _ -> [{attribute, Pos, compile, {nowarn_unused_function, RemainingFAs}}]
    end;
remove_form(Form, Skip) ->
    case ordsets:is_element(form_id(Form), Skip) of
        true -> [];
        false -> [Form]
    end.

form_id({function, _, Name, Arity, _}) -> {function, Name, Arity};
form_id({attribute, _, spec, {{Name, Arity}, _}}) -> {spec, Name, Arity};
form_id(_) -> undefined.
