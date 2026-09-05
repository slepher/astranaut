-module(astranaut_macro_expander_tests).
-include_lib("eunit/include/eunit.hrl").

-export([capture/2, capture_form/2]).
-export([provider_isolation_check/1]).

provider_isolation_test_() ->
    {timeout, 30, fun() ->
        TestBeam = filename:rootname(code:which(?MODULE)),
        Ebin = filename:join(code:lib_dir(astranaut), "ebin"),
        Eval = lists:flatten(
            io_lib:format(
                "{module, ~p} = code:load_abs(~tp), ~p:provider_isolation_check(~tp), halt().",
                [?MODULE, TestBeam, ?MODULE, Ebin]
            )
        ),
        Port = open_port({spawn_executable, os:find_executable("erl")}, [
            binary,
            exit_status,
            stderr_to_stdout,
            {args, ["+S", "2", "-noshell", "-eval", Eval]}
        ]),
        try
            ?assertEqual({0, <<>>}, isolation_result(Port, <<>>))
        after
            case erlang:port_info(Port) of
                undefined -> ok;
                _ -> port_close(Port)
            end
        end
    end}.

isolation_result(Port, Output) ->
    receive
        {Port, {data, Data}} -> isolation_result(Port, <<Output/binary, Data/binary>>);
        {Port, {exit_status, Status}} -> {Status, Output}
    after 20000 ->
        error({provider_isolation_timeout, Output})
    end.

provider_isolation_check(Ebin) ->
    %% Load the core without putting its directory on the code path. The local
    %% provider is genuinely unavailable, not merely unloaded.
    [
        begin
            {module, _} = code:load_abs(filename:rootname(Beam))
        end
     || Beam <- filelib:wildcard(filename:join(Ebin, "*.beam")),
        filename:basename(Beam) =/= "astranaut_macro_local.beam"
    ],
    non_existing = code:which(astranaut_macro_local),
    Forms = [
        {attribute, 1, file, {"macro_provider_isolation.erl", 1}},
        {attribute, 1, module, macro_provider_isolation},
        {attribute, 2, import_macro, astranaut_macros},
        {attribute, 3, exec_macro,
            {astranaut_macros, literal, [
                [{attribute, 3, export, [{value, 0}]}]
            ]}},
        {function, 4, value, 0, [
            {clause, 4, [], [], [
                {call, 4, {remote, 4, {atom, 4, astranaut_macros}, {atom, 4, literal}}, [
                    {atom, 4, ok}
                ]}
            ]}
        ]}
    ],
    %% astranaut_macros has no formatter; compiler output can carry its warning.
    Expanded =
        case astranaut_macro:parse_transform(Forms ++ [{eof, 6}], []) of
            {warning, ExpandedForms, _} -> ExpandedForms;
            ExpandedForms when is_list(ExpandedForms) -> ExpandedForms
        end,
    true = is_list(Expanded),
    true = lists:member({attribute, 3, export, [{value, 0}]}, Expanded),
    true = lists:member(
        {function, 4, value, 0, [{clause, 4, [], [], [{atom, 4, ok}]}]}, Expanded
    ),
    {error, Errors, _Warnings} = astranaut_macro:parse_transform(
        Forms ++ [{attribute, 5, local_macro, [{value, 0}]}, {eof, 6}], []
    ),
    true = lists:any(
        fun({_File, Diagnostics}) ->
            lists:member(
                {5, astranaut_lib, {
                    astranaut_macro, {macro_capability_unavailable, astranaut_macro_local}
                }},
                Diagnostics
            )
        end,
        Errors
    ),
    false = code:is_loaded(astranaut_macro_local),
    ok.

%% Both attribute and function invocations must group arguments before adding
%% injected attributes; exact arity must still win over grouped fallback.
shared_invocation_test() ->
    Macro = #{
        module => ?MODULE,
        function => capture,
        macro => {?MODULE, capture},
        arity => 2,
        call_arity => 1,
        formatter => astranaut_macro,
        as_attr => capture,
        group_args => true,
        attributes => #{file => "capture.erl"}
    },
    MacroMap = #{{capture, 1} => Macro},
    AttributeMacro = Macro#{function => capture_form, macro => {?MODULE, capture_form}},
    AttributeMap = #{{capture, 1} => AttributeMacro},
    Arguments = [{atom, 5, a}, {atom, 5, b}],
    Attribute = {attribute, 5, capture, Arguments},
    {ok, Target} = astranaut_macro_expander:resolve_attribute_target(
        Attribute, AttributeMap, astranaut_macro_expander:attribute_macro_index(AttributeMap)
    ),
    {just, [CapturedAttribute]} = astranaut_return:run(
        astranaut_traverse:eval(
            astranaut_macro_expander:expand_attribute_target(Target),
            astranaut_macro,
            #{node => form},
            ok
        )
    ),
    Call = {call, 5, {atom, 5, capture}, Arguments},
    Function = {function, 5, value, 0, [{clause, 5, [], [], [Call]}]},
    Task = #{form => Function, macro_map => MacroMap, observation_control => disabled},
    {just, #{forms := [{function, 5, value, 0, [{clause, 5, [], [], [Result]}]}]}} =
        astranaut_return:run(
            astranaut_macro_expander:expand_functions([Function], #{{function, value, 0} => Task})
        ),
    ?assertEqual(
        {Arguments, #{file => "capture.erl", pos => 5}},
        erl_parse:normalise(Result)
    ),
    ?assertEqual({attribute, 5, captured, erl_parse:normalise(Result)}, CapturedAttribute),
    Exact = AttributeMacro#{group_args => false, call_arity => 2},
    ExactMap = AttributeMap#{{capture, 2} => Exact},
    ?assertMatch(
        {ok, #{macro := Exact}},
        astranaut_macro_expander:resolve_attribute_target(
            Attribute, ExactMap, astranaut_macro_expander:attribute_macro_index(ExactMap)
        )
    ).

capture(Arguments, Attributes) ->
    erl_parse:abstract({Arguments, Attributes}, 5).

capture_form(Arguments, Attributes) ->
    [{attribute, 5, captured, {Arguments, Attributes}}].
