#!/usr/bin/env escript
%% -*- erlang -*-

%% Compare src/syntax.term with erl_syntax from every supported OTP branch.
%%
%% Sources are fetched from erlang/otp maint-N and cached without modification:
%%   .cache/syntax/erl_syntax_R21.erl
%%
%% Usage:
%%   escript scripts/check_syntax_schema.escript
%%   escript scripts/check_syntax_schema.escript --offline
%%   escript scripts/check_syntax_schema.escript --refresh
%%
%% The audit intentionally does not inspect or constrain erl_syntax's private
%% #tree{} representation. It checks public type/subtree/update behavior for
%% raw abstract formats and make_tree/is_leaf branches for syntax_tools-only
%% node types.

-mode(compile).

-define(BASE_URL,
        "https://raw.githubusercontent.com/erlang/otp/maint-~B/"
        "lib/syntax_tools/src/erl_syntax.erl").

main(Args) ->
    try
        Opts = parse_args(Args),
        Root = project_root(),
        SchemaFile = filename:join([Root, "src", "syntax.term"]),
        CacheDir = filename:join([Root, ".cache", "syntax"]),
        Schema = read_schema(SchemaFile),
        Versions = schema_versions(Schema),
        ok = filelib:ensure_dir(filename:join(CacheDir, "placeholder")),
        Results = [audit_version(Vsn, Schema, CacheDir, Opts) || Vsn <- Versions],
        Errors = lists:append([E || {_Vsn, E} <- Results]),
        report(Results, Errors),
        case Errors of
            [] -> ok;
            _ -> halt(1)
        end
    catch
        throw:{usage, Message} ->
            io:format(standard_error, "~s~n", [Message]),
            halt(2);
        Class:Reason:Stacktrace ->
            io:format(standard_error, "syntax schema audit crashed: ~p:~tp~n~tp~n",
                      [Class, Reason, Stacktrace]),
            halt(2)
    end.

parse_args([]) -> #{};
parse_args(["--offline"]) -> #{offline => true};
parse_args(["--refresh"]) -> #{refresh => true};
parse_args(["-h"]) -> throw({usage, usage()});
parse_args(["--help"]) -> throw({usage, usage()});
parse_args(_) -> throw({usage, usage()}).

usage() ->
    "usage: escript scripts/check_syntax_schema.escript "
    "[--offline | --refresh]".

project_root() ->
    Script = filename:absname(escript:script_name()),
    filename:dirname(filename:dirname(Script)).

read_schema(File) ->
    case file:consult(File) of
        {ok, [Schema]} when is_map(Schema) -> Schema;
        {ok, Terms} -> fail("~s must contain exactly one map, got ~tp", [File, Terms]);
        {error, Reason} -> fail("cannot read ~s: ~tp", [File, Reason])
    end.

schema_versions(#{otp_versions := #{min := Min, max := Max}})
  when is_integer(Min), is_integer(Max), Min =< Max ->
    lists:seq(Min, Max);
schema_versions(_) ->
    fail("syntax.term has no valid otp_versions range", []).

audit_version(Vsn, Schema, CacheDir, Opts) ->
    io:format("OTP ~B: ", [Vsn]),
    Source = ensure_source(Vsn, CacheDir, Opts),
    Forms = parse_source(Source),
    Supported = supported_types(Forms),
    Mod = load_version_module(Vsn, Source, Forms),
    try
        Errors0 = audit_nodes(Vsn, Schema, Supported, Mod),
        Errors = [{Vsn, Detail} || Detail <- Errors0],
        io:format("~s (~B supported node types)~n",
                  [case Errors of [] -> "ok"; _ -> "FAILED" end,
                   sets:size(Supported)]),
        {Vsn, Errors}
    after
        code:purge(Mod),
        code:delete(Mod)
    end.

ensure_source(Vsn, CacheDir, Opts) ->
    File = filename:join(CacheDir,
                         lists:flatten(io_lib:format("erl_syntax_R~B.erl", [Vsn]))),
    case {maps:get(refresh, Opts, false), filelib:is_regular(File)} of
        {false, true} -> File;
        _ ->
            case maps:get(offline, Opts, false) of
                true -> fail("OTP ~B cache is missing in offline mode: ~s", [Vsn, File]);
                false -> download_source(Vsn, File)
            end
    end.

download_source(Vsn, File) ->
    URL = lists:flatten(io_lib:format(?BASE_URL, [Vsn])),
    io:format("fetch ", []),
    Tmp = File ++ ".tmp",
    case os:find_executable("curl") of
        false -> download_with_httpc(URL, Tmp);
        Curl -> download_with_curl(Curl, URL, Tmp)
    end,
    case file:read_file(Tmp) of
        {ok, Body} -> ensure_erl_syntax_source(URL, Body);
        {error, Reason0} -> fail("cannot read downloaded file ~s: ~tp", [Tmp, Reason0])
    end,
    ok = replace_file(Tmp, File),
    File.

download_with_curl(Curl, URL, Tmp) ->
    Args = ["-fL", "--connect-timeout", "10", "--max-time", "60",
            "--retry", "2", "--user-agent", "astranaut-syntax-schema-audit",
            "-o", Tmp, URL],
    Port = open_port({spawn_executable, Curl},
                     [binary, exit_status, stderr_to_stdout, {args, Args}]),
    case collect_port(Port, <<>>) of
        {0, _Output} -> ok;
        {Status, Output} ->
            fail("curl download ~s failed with status ~B: ~ts",
                 [URL, Status, Output])
    end.

collect_port(Port, Output) ->
    receive
        {Port, {data, Data}} -> collect_port(Port, <<Output/binary, Data/binary>>);
        {Port, {exit_status, Status}} -> {Status, Output}
    after 70000 ->
        port_close(Port),
        fail("curl download timed out", [])
    end.

download_with_httpc(URL, Tmp) ->
    ok = ensure_http_started(),
    HTTPOpts = [{timeout, 60000}, {connect_timeout, 10000}|ssl_options()],
    Request = {URL, [{"user-agent", "astranaut-syntax-schema-audit"}]},
    case httpc:request(get, Request, HTTPOpts, [{body_format, binary}]) of
        {ok, {{_Version, 200, _Reason}, _Headers, Body}} ->
            ensure_erl_syntax_source(URL, Body),
            ok = write_file(Tmp, Body),
            ok;
        {ok, {{_Version, Status, Reason}, _Headers, Body}} ->
            fail("download ~s failed: HTTP ~B ~s (~tp)",
                 [URL, Status, Reason, Body]);
        {error, Reason} ->
            fail("download ~s failed: ~tp", [URL, Reason])
    end.

ensure_http_started() ->
    case application:ensure_all_started(inets) of
        {ok, _} -> ok;
        {error, Reason} -> fail("cannot start inets: ~tp", [Reason])
    end,
    case application:ensure_all_started(ssl) of
        {ok, _} -> ok;
        {error, Reason2} -> fail("cannot start ssl: ~tp", [Reason2])
    end.

ssl_options() ->
    case erlang:function_exported(public_key, cacerts_get, 0) of
        true ->
            [{ssl, [{verify, verify_peer},
                    {cacerts, public_key:cacerts_get()},
                    {customize_hostname_check,
                     [{match_fun, public_key:pkix_verify_hostname_match_fun(https)}]}]}];
        false ->
            %% Old OTP releases do not expose the OS CA store through
            %% public_key. Keep the downloader usable there; cached audits do
            %% not need TLS or network access.
            [{ssl, [{verify, verify_none}]}]
    end.

ensure_erl_syntax_source(URL, Body) ->
    case binary:match(Body, <<"-module(erl_syntax).">>) of
        nomatch -> fail("downloaded file is not erl_syntax source: ~s", [URL]);
        _ -> ok
    end.

write_file(File, Body) ->
    case file:write_file(File, Body) of
        ok -> ok;
        {error, Reason} -> fail("cannot write ~s: ~tp", [File, Reason])
    end.

replace_file(Tmp, File) ->
    _ = file:delete(File),
    case file:rename(Tmp, File) of
        ok -> ok;
        {error, Reason} -> fail("cannot move ~s to ~s: ~tp", [Tmp, File, Reason])
    end.

parse_source(File) ->
    case epp:parse_file(File, [], []) of
        {ok, Forms} -> Forms;
        {error, Reason} -> fail("cannot parse cached source ~s: ~tp", [File, Reason])
    end.

load_version_module(Vsn, Source, Forms0) ->
    Mod = list_to_atom("astranaut_erl_syntax_r" ++ integer_to_list(Vsn)),
    Forms = [rename_module(Form, Mod) || Form <- Forms0],
    case compile:forms(Forms, [binary, return_errors, return_warnings,
                               nowarn_deprecated_catch]) of
        {ok, Mod, Beam} -> load_binary(Mod, Source, Beam);
        {ok, Mod, Beam, _Warnings} -> load_binary(Mod, Source, Beam);
        {error, Errors, Warnings} ->
            fail("cannot compile cached OTP ~B erl_syntax:~n~tp~n~tp",
                 [Vsn, Errors, Warnings])
    end.

rename_module({attribute, Anno, module, erl_syntax}, Mod) ->
    {attribute, Anno, module, Mod};
rename_module(Form, _Mod) ->
    Form.

load_binary(Mod, Source, Beam) ->
    case code:load_binary(Mod, Source, Beam) of
        {module, Mod} -> Mod;
        {error, Reason} -> fail("cannot load ~p: ~tp", [Mod, Reason])
    end.

supported_types(Forms) ->
    MakeTree = [Type || {function, _, make_tree, 2, Clauses} <- Forms,
                        {clause, _, [{atom, _, Type}, _], _, _} <- Clauses],
    Leaf = lists:append([leaf_types(Clauses)
                         || {function, _, is_leaf, 1, Clauses} <- Forms]),
    sets:from_list(MakeTree ++ Leaf).

leaf_types(Term) when is_tuple(Term) ->
    case Term of
        {clause, _, [{atom, _, Type}], _, [{atom, _, true}]} -> [Type];
        _ -> lists:append([leaf_types(E) || E <- tuple_to_list(Term)])
    end;
leaf_types([H|T]) -> leaf_types(H) ++ leaf_types(T);
leaf_types([]) -> [];
leaf_types(_) -> [].

audit_nodes(Vsn, Schema, Supported, Mod) ->
    Nodes = maps:get(nodes, Schema, []),
    Active = maps:from_list([{maps:get(type, N), N}
                            || N <- Nodes, active(Vsn, N)]),
    CompletenessErrors = audit_completeness(Vsn, Schema, Active, Supported),
    CompletenessErrors ++
        lists:append([audit_node(Vsn, Node, Active, Supported, Schema, Mod)
                      || Node <- maps:values(Active)]).

audit_completeness(Vsn, Schema, Active, Supported) ->
    Modeled = sets:from_list(
                [Type || {Type, Node} <- maps:to_list(Active),
                         not maps:is_key(alias_of, Node)]),
    Excluded = sets:from_list(
                 [maps:get(type, Entry)
                  || Entry <- maps:get(excluded_nodes, Schema, []),
                     active(Vsn, Entry)]),
    Missing = sets:subtract(sets:subtract(Supported, Modeled), Excluded),
    Conflicts = sets:intersection(Modeled, Excluded),
    Stale = sets:subtract(Excluded, Supported),
    [{Type, missing_from_syntax_term} || Type <- sets:to_list(Missing)] ++
        [{Type, modeled_and_excluded} || Type <- sets:to_list(Conflicts)] ++
        [{Type, excluded_but_missing_from_erl_syntax} || Type <- sets:to_list(Stale)].

audit_node(Vsn, #{type := Type} = Node, Active, Supported, Schema, Mod) ->
    AvailabilityErrors =
        case maps:find(alias_of, Node) of
            {ok, Target} ->
                case maps:is_key(Target, Active) of
                    true -> [];
                    false -> [{Type, {inactive_or_missing_alias_target, Target}}]
                end;
            error ->
                case sets:is_element(Type, Supported) of
                    true -> [];
                    false -> [{Type, missing_from_erl_syntax}]
                end
        end,
    Formats = [F || F <- maps:get(formats, Node, []), active(Vsn, F)],
    FormatErrors = lists:append(
                     [audit_format(Type, Format, Node, Schema, Mod)
                      || Format <- Formats]),
    LayoutBranchErrors =
        case maps:is_key(alias_of, Node) of
            false -> audit_layout_branches(Vsn, Type, Node, Schema, Mod);
            true -> []
        end,
    AvailabilityErrors ++ FormatErrors ++ LayoutBranchErrors.

audit_format(Type, #{shape := Shape} = Format, Node, Schema, Mod) ->
    Id = maps:get(id, Format, anonymous),
    Sample = sample(Type, Shape),
    case public_node(Type, Sample, Mod) of
        {ok, PublicNode} ->
            audit_subtrees(Type, Id, PublicNode, Node, Schema, Mod);
        {ok, Actual, _PublicNode} ->
            [{Type, {format_type_mismatch, Id, Actual, Shape}}];
        {error, Reason} ->
            [{Type, {format_rejected, Id, Shape, Reason}}]
    end.

%% Record field tuples are deliberately ambiguous in erl_parse: outside a
%% record expression, the four-element form denotes record_access and the
%% three-element form has no standalone erl_syntax:type/1 branch.  Validate
%% them through the public record_expr/subtrees projection that disambiguates
%% them.  This does not inspect or depend on erl_syntax's private #tree form.
public_node(Type, Sample, Mod)
  when Type =:= record_field; Type =:= typed_record_field ->
    Parent = {record, 1, sample_record, [Sample]},
    case safe_apply(Mod, subtrees, [Parent]) of
        {ok, [_RecordType, [PublicNode]]} ->
            case safe_apply(Mod, type, [PublicNode]) of
                {ok, Type} -> {ok, PublicNode};
                {ok, Actual} -> {ok, Actual, PublicNode};
                {error, Reason} -> {error, {contextual_type_rejected, Reason}}
            end;
        {ok, Other} -> {error, {invalid_record_projection, Other}};
        {error, Reason} -> {error, {record_projection_rejected, Reason}}
    end;
public_node(Type, Sample, Mod) ->
    case safe_apply(Mod, type, [Sample]) of
        {ok, Type} ->
            {ok, Sample};
        {ok, Actual} ->
            {ok, Actual, Sample};
        {error, Reason} ->
            {error, Reason}
    end.

audit_layout_branches(_Vsn, _Type, #{layout := attribute}, _Schema, _Mod) ->
    [];
audit_layout_branches(Vsn, Type, Node, Schema, Mod) ->
    Layouts = [L || L <- resolve_layouts(Node, Schema), active(Vsn, L)],
    lists:append([audit_layout_branch(Type, Index, Layout, Mod)
                  || {Index, Layout0} <- lists:zip(lists:seq(1, length(Layouts)), Layouts),
                     Layout <- expand_layout_context(Layout0)]).

audit_layout_branch(Type, Index, Layout, Mod) ->
    case groups_for_layout(Layout) of
        {any, SeedGroups} ->
            audit_any_layout(Type, Index, Layout, SeedGroups, Mod);
        {fixed, Groups} ->
            audit_constructed_layout(Type, Index, Layout, Groups, Mod)
    end.

audit_any_layout(Type, Index, Layout, SeedGroups, Mod) ->
    %% `groups => any' is a deliberate wildcard: the source API has node
    %% types whose number of subtree groups varies by release or by node
    %% variant.  Probe the public constructor with increasing group counts,
    %% using only public syntax-tree terms for the children.  Do not infer a
    %% layout from the private #tree{} representation.
    Counts = lists:seq(1, 8),
    case first_constructible_layout(Type, Counts, SeedGroups, Mod) of
        {ok, _Groups, Sample, ActualGroups} ->
            case layout_matches(ActualGroups, Layout) of
                true -> check_update(Type, {layout, Index}, Sample, ActualGroups, Mod);
                false ->
                    [{Type, {layout_projection_mismatch, Index,
                             group_lengths(ActualGroups), Layout}}]
            end;
        {error, Reason} ->
            [{Type, {layout_make_tree_rejected, Index, any, Reason}}]
    end.

first_constructible_layout(Type, Counts, SeedGroups, Mod) ->
    first_constructible_layout(Type, Counts, SeedGroups, Mod, no_candidate).

first_constructible_layout(Type, [Count|Rest], SeedGroups, Mod, _LastReason) ->
    Groups = repeated_groups(Count, SeedGroups),
    case safe_apply(Mod, make_tree, [Type, Groups]) of
        {ok, Sample} ->
            case safe_apply(Mod, type, [Sample]) of
                {ok, Type} ->
                    case safe_apply(Mod, subtrees, [Sample]) of
                        {ok, ActualGroups} when is_list(ActualGroups) ->
                            {ok, Groups, Sample, ActualGroups};
                        {ok, Other} ->
                            first_constructible_layout(Type, Rest, SeedGroups, Mod,
                                                       {subtrees, Other});
                        {error, Reason} ->
                            first_constructible_layout(Type, Rest, SeedGroups, Mod,
                                                       {subtrees, Reason})
                    end;
                {ok, Actual} ->
                    first_constructible_layout(Type, Rest, SeedGroups, Mod,
                                               {type, Actual});
                {error, Reason} ->
                    first_constructible_layout(Type, Rest, SeedGroups, Mod,
                                               {type, Reason})
            end;
        {error, Reason} ->
            first_constructible_layout(Type, Rest, SeedGroups, Mod, Reason)
    end;
first_constructible_layout(_Type, [], _SeedGroups, _Mod, LastReason) ->
    {error, LastReason}.

repeated_groups(Count, [Seed|_]) -> lists:duplicate(Count, Seed);
repeated_groups(Count, []) -> lists:duplicate(Count, [sample_node(inherit)]).

audit_constructed_layout(Type, Index, Layout, Groups, Mod) ->
    case safe_apply(Mod, make_tree, [Type, Groups]) of
        {ok, Sample} ->
            case safe_apply(Mod, type, [Sample]) of
                {ok, Type} ->
                    case safe_apply(Mod, subtrees, [Sample]) of
                        {ok, ActualGroups} when is_list(ActualGroups) ->
                            BranchErrors =
                                case layout_matches(ActualGroups, Layout) of
                                    true -> [];
                                    false ->
                                        [{Type,
                                          {layout_projection_mismatch,
                                           Index, group_lengths(ActualGroups),
                                           Layout}}]
                                end,
                            BranchErrors ++
                                check_update(Type, {layout, Index}, Sample,
                                             ActualGroups, Mod);
                        {ok, Other} ->
                            [{Type, {layout_subtrees_rejected, Index,
                                     Groups, {invalid_result, Other}}}];
                        {error, Reason} ->
                            [{Type, {layout_subtrees_rejected, Index,
                                     Groups, Reason}}]
                    end;
                {ok, Actual} ->
                    [{Type, {layout_constructor_type_mismatch, Index,
                             Actual, Groups}}];
                {error, Reason} ->
                    [{Type, {layout_constructed_tree_rejected, Index,
                             Groups, Reason}}]
            end;
        {error, Reason} ->
            [{Type, {layout_make_tree_rejected, Index, Groups, Reason}}]
    end.

groups_for_layout(#{groups := any, children := Children} = Layout) ->
    {any, [group_for_child(Child, Layout) || Child <- layout_children(Children)]};
groups_for_layout(#{children := Children} = Layout) ->
    {fixed, [group_for_child(Child, Layout) || Child <- layout_children(Children)]}.

group_for_child(#{role := Role, cardinality := one}, _Layout) ->
    [sample_layout_child(Role)];
group_for_child(#{role := Role, cardinality := one_or_many}, _Layout) ->
    [sample_layout_child(Role)];
group_for_child(#{role := Role, cardinality := many},
                #{when_format := multiple_templates}) ->
    [sample_layout_child(Role), sample_layout_child(Role)];
group_for_child(#{role := Role, cardinality := many}, _Layout) ->
    [sample_layout_child(Role)];
group_for_child(#{role := Role, cardinality := deep_many}, _Layout) ->
    [sample_layout_child(Role)].

sample_layout_child(clause) -> sample_clause();
sample_layout_child(pattern) -> {var, 1, 'X'};
sample_layout_child(type) -> {type, 1, integer, []};
sample_layout_child(type_param) -> {var, 1, 'T'};
sample_layout_child(binary_field) ->
    {bin_element, 1, {integer, 1, 0}, default, default};
sample_layout_child(map_field) ->
    {map_field_assoc, 1, {atom, 1, key}, {atom, 1, value}};
sample_layout_child(_Role) -> {atom, 1, sample}.

audit_subtrees(Type, Id, Sample, Node, Schema, Mod) ->
    case safe_apply(Mod, subtrees, [Sample]) of
        {ok, Groups} when is_list(Groups) ->
            LayoutErrors = check_layout(Type, Id, Groups, Node, Schema),
            UpdateErrors = check_update(Type, Id, Sample, Groups, Mod),
            LayoutErrors ++ UpdateErrors;
        {ok, Other} -> [{Type, {invalid_subtrees_result, Id, Other}}];
        {error, Reason} -> [{Type, {subtrees_rejected, Id, Reason}}]
    end.

check_layout(_Type, _Id, _Groups, #{layout := attribute}, _Schema) ->
    %% Astranaut has deliberate attribute projections in Erlang code; the raw
    %% erl_syntax attribute projection is not the schema contract for them.
    [];
check_layout(Type, Id, Groups, Node, Schema) ->
    Layouts = resolve_layouts(Node, Schema),
    case Layouts of
        [] -> [];
        _ ->
            Candidates = lists:append([expand_layout_context(L)
                                       || L <- Layouts, layout_applies(Id, L)]),
            case lists:any(fun(L) -> layout_matches(Groups, L) end, Candidates) of
                true -> [];
                false ->
                    [{Type, {no_matching_layout, Id, group_lengths(Groups),
                             Candidates}}]
            end
    end.

resolve_layouts(#{layouts := Layouts}, _Schema) -> Layouts;
resolve_layouts(#{layout := Name}, #{layouts := Named}) when is_atom(Name) ->
    maps:get(Name, Named, []);
resolve_layouts(_Node, _Schema) ->
    [].

expand_layout_context(#{context := Context} = Layout) ->
    [(maps:remove(context, Layout))#{children => maps:get(children, Variant)}
     || Variant <- Context];
expand_layout_context(Layout) -> [Layout].

layout_applies(Id, Layout) ->
    case maps:find(when_format, Layout) of
        error -> true;
        {ok, Id} -> true;
        {ok, Ids} when is_list(Ids) -> lists:member(Id, Ids);
        _ -> false
    end.

layout_matches(Groups, #{groups := any}) ->
    is_list(Groups);
layout_matches(Groups, #{groups := Count, children := Children})
  when is_integer(Count), length(Groups) =:= Count ->
    cardinalities_match(Groups, layout_children(Children));
layout_matches(_Groups, _Layout) -> false.

layout_children(Children) -> Children.

cardinalities_match(Groups, Children) when length(Groups) =:= length(Children) ->
    lists:all(fun({Group, Child}) -> cardinality_matches(Group, maps:get(cardinality, Child)) end,
              lists:zip(Groups, Children));
cardinalities_match(_Groups, _Children) -> false.

cardinality_matches(Group, one) -> is_list(Group) andalso length(Group) =:= 1;
cardinality_matches(Group, one_or_many) -> is_list(Group) andalso Group =/= [];
cardinality_matches(Group, many) -> is_list(Group);
cardinality_matches(Group, deep_many) -> is_list(Group);
cardinality_matches(_Group, _Cardinality) -> false.

group_lengths(Groups) -> [length(G) || G <- Groups].

check_update(Type, Id, Sample, Groups, Mod) ->
    %% update_tree/2 is specified only when subtrees/1 is nonempty.  This also
    %% covers conditional leaves such as tuple_type(any_size).
    case Groups of
        [] -> [];
        _ -> check_nonleaf_update(Type, Id, Sample, Groups, Mod)
    end.

check_nonleaf_update(Type, Id, Sample, Groups, Mod) ->
    case safe_apply(Mod, update_tree, [Sample, Groups]) of
        {ok, Updated} ->
            case safe_apply(Mod, type, [Updated]) of
                {ok, Type} -> check_roundtrip(Type, Id, Sample, Updated, Mod);
                {ok, Actual} -> [{Type, {update_type_mismatch, Id, Actual}}];
                {error, Reason} -> [{Type, {updated_tree_rejected, Id, Reason}}]
            end;
        {error, Reason} -> [{Type, {update_tree_rejected, Id, Reason}}]
    end.

%% Layout probes deliberately use the smallest public syntax tree that
%% exercises a subtree grouping. Some are not complete legal Erlang forms and
%% cannot be reverted out of their parent context. Concrete format samples,
%% on the other hand, must survive projection and reconstruction semantically.
check_roundtrip(_Type, {layout, _Index}, _Sample, _Updated, _Mod) ->
    [];
check_roundtrip(Type, Id, Sample, Updated, Mod) ->
    case {safe_apply(Mod, revert, [Sample]),
          safe_apply(Mod, revert, [Updated])} of
        {{ok, OriginalForm}, {ok, OriginalForm}} ->
            [];
        {{ok, OriginalForm}, {ok, UpdatedForm}} ->
            [{Type, {update_roundtrip_mismatch, Id,
                     OriginalForm, UpdatedForm}}];
        {{error, Reason}, _} ->
            [{Type, {original_revert_rejected, Id, Reason}}];
        {_, {error, Reason}} ->
            [{Type, {updated_revert_rejected, Id, Reason}}]
    end.

safe_apply(Mod, Function, Args) ->
    try apply(Mod, Function, Args) of
        Value -> {ok, Value}
    catch
        Class:Reason -> {error, {Class, Reason}}
    end.

active(Vsn, Map) ->
    Since = maps:get(since, Map, Vsn),
    Until = maps:get(until, Map, Vsn),
    Vsn >= Since andalso Vsn =< Until.

sample(Type, Shape) -> sample_shape(Type, Shape).

sample_shape(_Type, anno) -> 1;
sample_shape(_Type, {value, Name}) -> sample_value(Name);
sample_shape(_Type, {values, Name}) -> sample_values(Name);
sample_shape(Type, {node, Name}) -> sample_node(Type, Name);
sample_shape(Type, {nodes, Name}) -> sample_nodes(Type, Name);
sample_shape(Type, {optional, Shape}) -> sample_shape(Type, Shape);
sample_shape(Type, Map) when is_map(Map) ->
    maps:map(fun(_K, V) -> sample_shape(Type, V) end, Map);
sample_shape(Type, [H|T]) ->
    [sample_shape(Type, H)|sample_shape(Type, T)];
sample_shape(_Type, []) -> [];
sample_shape(Type, Tuple) when is_tuple(Tuple) ->
    list_to_tuple([sample_shape(Type, E) || E <- tuple_to_list(Tuple)]);
sample_shape(_Type, Value) -> Value.

sample_value(arity) -> 0;
sample_value(characters) -> "sample";
sample_value(location) -> 1;
sample_value(operator) -> '+';
sample_value(error_info) -> {1, erl_parse, sample_error};
sample_value(warning_info) -> {1, erl_parse, sample_warning};
sample_value(local_record_name) -> sample_record;
sample_value(record_name) -> sample_record;
sample_value(native_record_name) -> {sample_module, sample_record};
sample_value(anonymous_record_name) -> [];
sample_value(_Name) -> sample.

sample_values(type_specifiers) -> [integer];
sample_values(record_names) -> [sample];
sample_values(_Name) -> [sample].

sample_nodes(clause, guards) -> [[{atom, 1, sample_guard}]];
sample_nodes(list_comp, templates) ->
    [{atom, 1, template1}, {atom, 1, template2}];
sample_nodes(map_comp, templates) ->
    [{map_field_assoc, 1, {atom, 1, key1}, {atom, 1, value1}},
     {map_field_assoc, 1, {atom, 1, key2}, {atom, 1, value2}}];
sample_nodes(try_expr, handlers) -> [sample_try_clause()];
sample_nodes(Type, Name) -> [sample_node(Type, Name)].

sample_node(record_expr, fields) ->
    {record_field, 1, {atom, 1, field}, {atom, 1, value}};
sample_node(record_type, fields) ->
    {type, 1, field_type, [{atom, 1, field}, {type, 1, integer, []}]};
sample_node(map_expr, fields) ->
    {map_field_assoc, 1, {atom, 1, key}, {atom, 1, value}};
sample_node(map_type, fields) ->
    {type, 1, map_field_assoc,
     [{type, 1, atom, []}, {type, 1, integer, []}]};
sample_node(typed_record_field, field) ->
    {record_field, 1, {atom, 1, field}};
sample_node(_Type, patterns) -> {var, 1, 'X'};
sample_node(_Type, pattern) -> {var, 1, 'X'};
sample_node(_Type, clauses) -> sample_clause();
sample_node(_Type, clause) -> sample_clause();
sample_node(_Type, handlers) -> sample_try_clause();
sample_node(_Type, else_clauses) -> sample_clause();
sample_node(map_comp, template) ->
    {map_field_assoc, 1, {atom, 1, key}, {atom, 1, value}};
sample_node(_Type, template) -> {atom, 1, template};
sample_node(_Type, templates) -> {atom, 1, template};
sample_node(_Type, size) -> {integer, 1, 8};
sample_node(_Type, _Name) -> {atom, 1, sample}.

sample_node(Name) -> sample_node(undefined, Name).

sample_clause() -> {clause, 1, [], [], [{atom, 1, ok}]}.

sample_try_clause() ->
    {clause, 1,
     [{tuple, 1, [{atom, 1, error}, {var, 1, 'Reason'}, {var, 1, '_'}]}],
     [], [{atom, 1, handled}]}.

report(Results, []) ->
    io:format("syntax schema audit passed for ~B OTP releases.~n", [length(Results)]);
report(_Results, Errors) ->
    io:format(standard_error, "~nsyntax schema audit found ~B error(s):~n", [length(Errors)]),
    lists:foreach(fun({Vsn, {Type, Detail}}) ->
                          io:format(standard_error, "  OTP ~B ~p: ~tp~n",
                                    [Vsn, Type, Detail])
                  end, Errors).

fail(Format, Args) ->
    throw({usage, lists:flatten(io_lib:format(Format, Args))}).
