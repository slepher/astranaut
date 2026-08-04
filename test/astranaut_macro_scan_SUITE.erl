%%%-------------------------------------------------------------------
%%% Source-ordered scan-and-splice primitives.
%%%-------------------------------------------------------------------
-module(astranaut_macro_scan_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile({parse_transform, astranaut_do}).

-export([all/0]).
-export([basic/1, splice/1, splice_empty/1, expand/1, expand_with_state/1,
         run_without_local_capability/1,
         preserve_generated_function_position/1, generated_function_original_merge/1,
         generated_merge_preserves_original_spec/1,
         generated_merge_prefers_generated_spec/1,
         generated_merge_keeps_generated_spec_without_original/1,
         lift_m_bridge/1]).

all() -> [basic, splice, splice_empty, expand, expand_with_state,
          run_without_local_capability,
          preserve_generated_function_position, generated_function_original_merge,
          generated_merge_preserves_original_spec,
          generated_merge_prefers_generated_spec,
          generated_merge_keeps_generated_spec_without_original,
          lift_m_bridge].

run_without_local_capability(_Config) ->
    Forms = [{attribute, 1, module, ordinary_macro_scan_test},
             {function, 2, value, 0,
              [{clause, 2, [], [], [{atom, 2, ok}]}]}],
    {just, {_ScannedForms,
            #{capability := disabled,
              registry := Registry}}} =
        astranaut_return:run(
          astranaut_macro_scan:run(
            ordinary_macro_scan_test, "ordinary_macro_scan_test.erl",
            #{max_depth => 100}, Forms, [])),
    false = maps:is_key(local_macro_module, Registry),
    ok.

lift_m_bridge(_Config) ->
    TraverseMA = astranaut_traverse:return(42),
    EvalResult = astranaut_traverse:eval(TraverseMA, ?MODULE, #{}, ok),
    {just, V} = astranaut_return:run(EvalResult),
    42 = V,
    ok.

basic(_Config) ->
    Forms = [{attribute, {1,1}, module, test_module},
             {function, {2,1}, foo, 0, [{clause, {2,1}, [], [], [{atom, {2,5}, ok}]}]}],
    Handler = fun(Form) -> astranaut_traverse:return(Form) end,
    {just, {Result, _State}} = run_splice(Handler, Forms, #{}),
    [_, _] = Result,
    ok.

splice(_Config) ->
    Forms = [{attribute, {1,1}, x, ok},
             {attribute, {2,1}, keep, ok}],
    Handler = fun
        ({attribute, _Pos, x, _}) ->
            astranaut_traverse:return(
              {splice, [{attribute, {10,1}, y, ok},
                        {function, {11,1}, bar, 0, [{clause, {11,1}, [], [], [{atom, {11,5}, ok}]}]}]});
        (Form) -> astranaut_traverse:return(Form)
    end,
    {just, {Result, _State}} = run_splice(Handler, Forms, #{}),
    true = lists:any(fun({attribute, _, y, _}) -> true; (_) -> false end, Result),
    true = lists:any(fun({function, _, bar, 0, _}) -> true; (_) -> false end, Result),
    true = lists:any(fun({attribute, _, keep, _}) -> true; (_) -> false end, Result),
    false = lists:any(fun({attribute, _, x, _}) -> true; (_) -> false end, Result),
    ok.

splice_empty(_Config) ->
    Forms = [{attribute, {1,1}, import_macro, foo},
             {attribute, {2,1}, keep, ok}],
    Handler = fun
        ({attribute, _Pos, import_macro, _}) ->
            do([ traverse ||
                   astranaut_traverse:modify(fun(S) -> maps:put(count, 1, S) end),
                   astranaut_traverse:return({splice, []})
               ]);
        (Form) ->
            astranaut_traverse:return(Form)
    end,
    {just, {Result, State}} = run_splice(Handler, Forms, #{}),
    1 = maps:get(count, State),
    false = lists:any(fun({attribute, _, import_macro, _}) -> true; (_) -> false end, Result),
    true = lists:any(fun({attribute, _, keep, _}) -> true; (_) -> false end, Result),
    ok.

expand(_Config) ->
    Forms = [{attribute, {1,1}, module, test},
             {attribute, {2,1}, my_macro, ok}],
    Handler = fun
        ({attribute, _Pos, my_macro, _}) ->
            Expanded = [{attribute, {10,1}, local_macro, [{gen, 0}]},
                        {function, {11,1}, gen, 0, [{clause, {11,1}, [], [], [{atom, {11,5}, ok}]}]}],
            astranaut_traverse:return({splice, Expanded});
        (Form) -> astranaut_traverse:return(Form)
    end,
    {just, {Result, _State}} = run_splice(Handler, Forms, #{}),
    true = lists:any(fun({attribute, _, local_macro, _}) -> true; (_) -> false end, Result),
    true = lists:any(fun({function, _, gen, 0, _}) -> true; (_) -> false end, Result),
    true = lists:any(fun({attribute, _, module, _}) -> true; (_) -> false end, Result),
    false = lists:any(fun({attribute, _, my_macro, _}) -> true; (_) -> false end, Result),
    ok.

expand_with_state(_Config) ->
    Forms = [{attribute, {1,1}, my_macro, ok}],
    Handler = fun
        ({attribute, _Pos, my_macro, _}) ->
            do([ traverse ||
                   astranaut_traverse:modify(fun(S) -> maps:put(expanded, true, S) end),
                   astranaut_traverse:return(
                     {splice, [{function, {10,1}, bar, 0, [{clause, {10,1}, [], [], [{atom, {10,5}, ok}]}]}]})
               ]);
        (Form) ->
            astranaut_traverse:return(Form)
    end,
    Init = #{count => 42},
    {just, {Result, State}} = run_splice(Handler, Forms, Init),
    42 = maps:get(count, State),
    true = maps:get(expanded, State),
    true = lists:any(fun({function, _, bar, 0, _}) -> true; (_) -> false end, Result),
    ok.

preserve_generated_function_position(_Config) ->
    Before = {function, {2,1}, before, 0, [{clause, {2,1}, [], [], [{atom, {2,5}, ok}]}]},
    After = {function, {4,1}, 'after', 0, [{clause, {4,1}, [], [], [{atom, {4,5}, ok}]}]},
    Generated = {function, {10,1}, generated, 0, [{clause, {10,1}, [], [], [{atom, {10,5}, ok}]}]},
    Forms = [{attribute, {1,1}, module, test},
             Before,
             {attribute, {3,1}, my_macro, ok},
             After],
    Handler = fun
        ({attribute, _Pos, my_macro, _}) ->
            astranaut_traverse:return({splice, [Generated]});
        (Form) ->
            astranaut_traverse:return(Form)
    end,
    {just, {Result, _State}} = run_splice(Handler, Forms, #{}),
    FunctionNames = [Name || {function, _, Name, 0, _} <- Result],
    [before, generated, 'after'] = FunctionNames,
    ok.

generated_function_original_merge(_Config) ->
    Original = {function, {2,1}, foo, 0,
                [{clause, {2,1}, [], [], [{atom, {2,5}, original}]}]},
    Generated = {function, {10,1}, foo, 0,
                 [{clause, {10,1}, [], [],
                   [{call, {10,5}, {atom, {10,5}, '__original__'}, []}]}]},
    Forms = [{attribute, {1,1}, module, test},
             Original,
             {attribute, {3,1}, my_macro, ok}],
    Handler = fun
        ({attribute, _Pos, my_macro, _}) ->
            astranaut_traverse:return({splice, [Generated]});
        (Form) ->
            astranaut_traverse:return(Form)
    end,
    {just, {Result, _State}} = run_splice(Handler, Forms, #{}),
    FunctionNames = [Name || {function, _, Name, 0, _} <- Result],
    [foo_1, foo] = FunctionNames,
    false = astranaut:search(
              fun({call, _, {atom, _, '__original__'}, []}) -> true;
                 (_) -> false
              end, Result, #{traverse => pre}),
    true = astranaut:search(
             fun({call, _, {atom, _, foo_1}, []}) -> true;
                (_) -> false
             end, Result, #{traverse => pre}),
    ok.

generated_merge_preserves_original_spec(_Config) ->
    OriginalSpec = spec_form({2,1}, foo),
    {Original, Generated} = original_and_wrapper(),
    Result = run_original_merge([OriginalSpec, Original], [Generated]),
    [OriginalSpec] = [Spec || Spec = {attribute, _, spec, _} <- Result],
    ok.

generated_merge_prefers_generated_spec(_Config) ->
    OriginalSpec = spec_form({2,1}, foo),
    GeneratedSpec = spec_form({10,1}, foo),
    {Original, Generated} = original_and_wrapper(),
    Result = run_original_merge([OriginalSpec, Original], [GeneratedSpec, Generated]),
    [GeneratedSpec] = [Spec || Spec = {attribute, _, spec, _} <- Result],
    ok.

generated_merge_keeps_generated_spec_without_original(_Config) ->
    GeneratedSpec = spec_form({10,1}, foo),
    {Original, Generated} = original_and_wrapper(),
    Result = run_original_merge([Original], [GeneratedSpec, Generated]),
    [GeneratedSpec] = [Spec || Spec = {attribute, _, spec, _} <- Result],
    ok.

original_and_wrapper() ->
    Original = {function, {2,1}, foo, 0,
                [{clause, {2,1}, [], [], [{atom, {2,5}, original}]}]},
    Generated = {function, {10,1}, foo, 0,
                 [{clause, {10,1}, [], [],
                   [{call, {10,5}, {atom, {10,5}, '__original__'}, []}]}]},
    {Original, Generated}.

run_original_merge(Prefix, GeneratedForms) ->
    Forms = [{attribute, {1,1}, module, test}] ++ Prefix ++
            [{attribute, {3,1}, my_macro, ok}],
    Handler = fun
        ({attribute, _Pos, my_macro, _}) ->
            astranaut_traverse:return({splice, GeneratedForms});
        (Form) ->
            astranaut_traverse:return(Form)
    end,
    {just, {Result, _State}} = run_splice(Handler, Forms, #{}),
    Result.

spec_form(Pos, Name) ->
    {attribute, Pos, spec,
     {{Name, 0},
      [{type, Pos, 'fun',
        [{type, Pos, product, []}, {type, Pos, atom, []}]}]}}.

run_splice(Handler, Forms, InitState) ->
    astranaut_return:run(
      astranaut_traverse:run(
        astranaut_macro_scan:map_forms_splice(
          Handler, Forms, #{traverse => none}),
        ?MODULE, #{}, InitState)).
