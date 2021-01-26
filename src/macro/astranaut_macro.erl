%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2018, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 18 Nov 2018 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_macro).

-include("stacktrace.hrl").
-include("quote.hrl").
-include("astranaut_do.hrl").

%% API
-export([transform_macro/5, transform_macros/2]).
-export([parse_transform/2, format_error/1]).

%%%===================================================================
%%% API
%%%===================================================================
transform_macro(M, F, A, Opts, Forms) ->
    transform_macros([{M, F, A, Opts}], Forms).

transform_macros(MFAOpts, Forms) when is_list(MFAOpts) ->
    File = astranaut:file(Forms),
    [{Line, Module}] = astranaut:attributes_with_line(module, Forms),
    {Macros, _MacroOptions, Warnings} =
        lists:foldl(
          fun({M, F, A, Opts}, Acc) -> 
                  astranaut_macro_options:add({M, F, A}, Opts, Module, File, Line, Forms, Acc)
          end, {[], maps:new(), []}, lists:reverse(MFAOpts)),
    transform_macros_1(Macros, Forms, File, Warnings).

parse_transform(Forms, Options) ->
    File = astranaut:file(Forms),
    [Module] = astranaut:attributes(module, Forms),
    {Macros, Warnings} = macros(File, Forms, Module),
    case astranaut_macro_local:compile(Macros, Forms, Options) of
        {ok, LWarnings} ->
            TraverseReturn = transform_macros_1(Macros, Forms, File, Warnings ++ LWarnings),
            astranaut_return_m:to_compiler(TraverseReturn);
        {error, Errors, NWarnings} ->
            {error, Errors, Warnings ++ NWarnings}
    end.

format_error({unexported_macro, Module, Function, Arity}) ->
    io_lib:format("unexported macro ~p:~p/~p.", [Module, Function, Arity]);
format_error({undefined_macro, Function, Arity}) ->
    io_lib:format("undefined macro ~p/~p.", [Function, Arity]);
format_error({unloaded_module, Module}) ->
    io_lib:format("module ~p could not be loaded, add to erl_first_files in rebar.config to make it compile first.", [Module]);
format_error({invalid_use_macro, Opts}) ->
    io_lib:format("invalid use macro ~p.", [Opts]);
format_error({invalid_option_value, Key, Value}) ->
    io_lib:format("invalid option value ~p ~p.", [Key, Value]);
format_error({invalid_option_value, Value}) ->
    io_lib:format("invalid option value ~p.", [Value]);
format_error({invalid_option, Options}) ->
    io_lib:format("invalid option ~p.", [Options]);
format_error({non_exported_formatter, Module}) ->
    io_lib:format("format_error/1 is not exported from module ~p.", [Module]);
format_error({unloaded_formatter_module, Module}) ->
    io_lib:format("formatter module ~p could not be loaded.", [Module]);
format_error(Message) ->
    case io_lib:deep_char_list(Message) of
        true -> Message;
        _    -> io_lib:write(Message)
    end.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
macros(File, Forms, LocalModule) ->
    Macros = lists:flatten(astranaut:attributes_with_line(use_macro, Forms)),
    {AllMacros, _MacroOptions, Warnings} = 
        lists:foldl(
          fun({Line, {Module, {Function, Arity}}}, Acc) ->
                  astranaut_macro_options:add(
                    {Module, Function, Arity}, [], LocalModule, File, Line, Forms, Acc);
             ({Line, {Module, {Function, Arity}, Opts}}, Acc) ->
                  astranaut_macro_options:add(
                    {Module, Function, Arity}, Opts, LocalModule, File, Line, Forms, Acc);
             ({Line, {{Function, Arity}}}, Acc) ->
                  astranaut_macro_options:add(
                    {Function, Arity}, [], LocalModule, File, Line, Forms, Acc);
             ({Line, {{Function, Arity}, Opts}}, Acc) ->
                  astranaut_macro_options:add(
                    {Function, Arity}, Opts, LocalModule, File, Line, Forms, Acc);
             ({Line, Other}, {LocalMacrosAcc, AllMacrosAcc, WarningsAcc}) ->
                  {LocalMacrosAcc, AllMacrosAcc, [{Line, ?MODULE, {invalid_use_macro, Other}}|WarningsAcc]}
          end, {[], maps:new(), []}, Macros),
    {lists:reverse(AllMacros), lists:reverse(Warnings)}.

transform_macros_1(Macros, Forms, _File, Warnings) ->
    Monad = transform_macros_2(Macros, Warnings),
    astranaut_traverse_m:exec(Monad, ?MODULE, #{forms => Forms, counter => 1}).

transform_macros_2(Macros, Warnings) ->
    do([astranaut_traverse_m || 
           astranaut_traverse_m:warnings(Warnings),
           astranaut_monad:map_m(
             fun(#{} = MacroOpts) ->
                     do([ astranaut_traverse_m ||
                            %% transform -exec_macro({MacroModule, MacroFun, Arguments}).
                            transform_macro_attr(MacroOpts),
                            %% transform macro MacroModule:MacroFun(Arguments).
                            transform_macro_call(MacroOpts)
                        ])
             end, Macros, astranaut_traverse_m),
           astranaut_traverse_m:modify(
             fun(#{forms := Forms}) ->
                     format_forms(astranaut:reorder_exports(Forms))
             end)
       ]).

%%%===================================================================
%%% transform_macro_attr and it's help functions.
%%%===================================================================
transform_macro_attr(MacroOpts) ->
    do([astranaut_traverse_m || 
           #{forms := Forms, counter := Counter} <- astranaut_traverse_m:get(),
           astranaut_traverse_m:put(#{offset => 1, counter => Counter, forms => Forms}),
           astranaut_traverse:map_m(
             fun(Form, #{}) ->
                     do([astranaut_traverse_m || 
                            #{counter := CounterAcc, forms := Forms1, offset := Offset} <-
                                astranaut_traverse_m:get(),
                            Return = walk_macro_attr(Form, MacroOpts#{counter => CounterAcc}),
                            NForm <- astranaut_walk_return:to_traverse_m(Return, Form, #{with_state => true}),
                            CounterAcc1 <- astranaut_traverse_m:get(),
                            FormsWithCounter1 = #{offset => Offset, 
                                                  forms => Forms1, 
                                                  counter => CounterAcc1},
                            FormsWithCounter2 = 
                                append_form(Form, NForm, MacroOpts, FormsWithCounter1),
                            astranaut_traverse_m:put(FormsWithCounter2)
                        ])
             end, Forms, #{formatter => ?MODULE, traverse => list})
       ]).

walk_macro_attr({attribute, Line, exec_macro, {Function, Arguments}} = NodeA, #{macro := Function} = Opts) ->
    apply_macro(NodeA, Opts#{arguments => to_list(Arguments), line => Line});
walk_macro_attr({attribute, Line, exec_macro, {Module, Function, Arguments}} = NodeA,
                #{macro := {Module, Function}} = Opts) ->
    apply_macro(NodeA, Opts#{arguments => to_list(Arguments), line => Line});
walk_macro_attr({attribute, Line, Attribute, Arguments} = NodeA, #{as_attr := Attribute} = Opts) ->
    apply_macro(NodeA, Opts#{arguments => to_list(Arguments), line => Line});
walk_macro_attr(Node, #{counter := Counter}) ->
    {Node, Counter}.

to_list(Arguments) when is_list(Arguments) ->
    Arguments;
to_list(Arguments) ->
    [Arguments].

append_form(Form, Form, _MacroOpts, #{offset := Offset} = FormsWithCounter) ->
    FormsWithCounter#{offset => Offset + 1};
append_form(Form, NForm, MacroOpts, #{offset := Offset, forms := Forms} = FormsWithCounter) ->
    Line = erl_syntax:get_pos(Form),
    {Nodes, Forms1} = update_forms(to_list(NForm), Forms, MacroOpts#{line => Line}),
    Forms2 = astranaut:replace_from_nth(Nodes, Offset, Forms1),
    FormsWithCounter#{offset => Offset + length(Nodes), forms => Forms2}.

update_forms(Nodes, Acc, #{auto_export := true, line := Line} = Opts) ->
    Exports = exports(Nodes, Line),
    update_forms(Exports ++ Nodes, Acc, maps:remove(auto_export, Opts));
update_forms(Nodes, Acc, #{merge_function := false}) ->
    {Nodes, Acc};
update_forms(Nodes, Forms, #{merge_function := MergeFunction}) ->
    {NRests, NForms} = 
        lists:foldl(
          fun({function, _Line, _FName, _Arity, _Clauses} = Function, {Rests, Acc}) ->
                  case insert_function(Function, Acc, MergeFunction, []) of
                      {ok, NAcc} ->
                          {Rests, NAcc};
                      error ->
                          {[Function|Rests], Acc}
                  end;
             ({attribute, Line, export, FAs}, {Rests, Acc}) ->
                  case lists:filter(
                         fun({Name, Arity}) ->
                                 not already_exported(Name, Arity, Acc)
                         end, FAs) of
                      [] ->
                          {Rests, Acc};
                      NFAs ->
                          NExport = {attribute, Line, export, NFAs},
                          {[NExport|Rests], Acc}
                  end;
             (Form, {Rests, Acc}) ->
                  {[Form|Rests], Acc}
          end, {[], Forms}, Nodes),
    {lists:reverse(NRests), NForms};
update_forms(Nodes, Acc, _MOpts) ->
    {Nodes, Acc}.

insert_function({function, _Line, FName, Arity, Clauses}, 
                [{function, Line, FName, Arity, FClauses}|T], Merge, Heads) ->
    {ok, lists:reverse(Heads) ++ [{function, Line, FName, Arity, merge_clauses(Clauses, FClauses, Merge)}|T]};
insert_function(_Function, [], _Merge, _Heads) ->
    error;
insert_function(Function, [H|T], Merge, Heads) ->
    insert_function(Function, T, Merge, [H|Heads]).

merge_clauses(Clauses1, Clauses2, head) ->
    Clauses1 ++ Clauses2;
merge_clauses(Clauses1, Clauses2, true) ->
    Clauses1 ++ Clauses2;
merge_clauses(Clauses1, Clauses2, tail) ->
    Clauses2 ++ Clauses1.

exports(Nodes, Line) ->
    case lists:foldl(
           fun({function, _Line, FName, Arity, _Clauses}, Acc) ->
                   [{FName, Arity}|Acc];
              (_, Acc) ->
                   Acc
           end, [], Nodes) of
        [] ->
            [];
        FAs ->
            [astranaut:exports(lists:reverse(FAs), Line)]
    end.

already_exported(Name, Arity, [{attribute, _Line, export, FAs}|T]) ->
    case lists:member({Name, Arity}, FAs) of
        true ->
            true;
        false ->
            already_exported(Name, Arity, T)
    end;
already_exported(Name, Arity, [_H|T]) ->
    already_exported(Name, Arity, T);
already_exported(_Name, _Arity, []) ->
    false.

%%%===================================================================
%%% transform_macro_call and it's help functions.
%%%===================================================================
transform_macro_call(MacroOpts) ->
    Traverse = maps:get(order, MacroOpts, post),
    Opts = #{traverse => Traverse, formatter => ?MODULE},
    do([astranaut_traverse_m ||
           #{forms := Forms0, counter := Counter0} <- astranaut_traverse_m:get(),
           astranaut_traverse_m:put(Counter0),
           Forms1 <- 
               astranaut_traverse:map_m(
                 fun(Node, _Attr) ->
                         do([astranaut_traverse_m ||
                                CounterAcc <- astranaut_traverse_m:get(),
                                Return = walk_macro_call(Node, MacroOpts#{counter => CounterAcc}),
                                astranaut_walk_return:to_traverse_m(Return, Node, #{with_state => true})
                            ])
                 end, Forms0, Opts),
           astranaut_traverse_m:modify(fun(Counter1) -> #{forms => Forms1, counter => Counter1} end)
       ]).

walk_macro_call({call, Line, {atom, _Line2, Function}, Arguments} = Node, #{macro := Function} = Opts) ->
    apply_macro(Node, Opts#{arguments => Arguments, line => Line});
walk_macro_call({call, Line, {remote, Line2, {atom, Line2, Module}, {atom, Line2, Function}}, Arguments} = Node, 
                #{macro := {Module, Function}} = Opts) ->
    apply_macro(Node, Opts#{arguments => Arguments, line => Line});
walk_macro_call(Node, #{counter := Counter}) ->
    {Node, Counter}.

%%%===================================================================
%%% apply_macro and it's help functions.
%%%===================================================================
apply_macro(NodeA, #{module := Module, arity := Arity,
                     arguments := Arguments, counter := Counter} = Opts) ->
    Arguments1 = group_arguments(Arguments, Opts),
    Arguments2 = append_attrs(Arguments1, Opts),
    Line = erl_syntax:get_pos(NodeA),
    if
        length(Arguments2) == Arity ->
            astranaut_traverse_m:with_formatter(
              Module,
              astranaut_traverse_m:update_line(
                Line,
              astranaut_traverse_m:astranaut_traverse_m(
                do_apply_macro(NodeA, Opts#{arguments => Arguments2}))));
        true ->
            astranaut_walk_return:new(#{node => NodeA, state => Counter})
    end.

do_apply_macro(NodeA, #{module := Module, function := Function,
                        arguments := Arguments, line := Line, counter := Counter} = Opts) ->
    MacroReturn = apply_mfa(Module, Function, Arguments),
    case astranaut_walk_return:new(MacroReturn) of
        #{return := NodeB} = MacroReturnStruct ->
            MacroNameStr = macro_name_str(Opts),
            NodeC = update_with_counter(NodeB, MacroNameStr, integer_to_list(Counter)),
            NodeD = astranaut:replace_line_zero(NodeC, Line),
            format_node(NodeD, Opts),
            MacroReturnStruct1 = maps:without([return, node, nodes], MacroReturnStruct),
            astranaut_walk_return:new(MacroReturnStruct1#{node => NodeD, state => Counter + 1});
        #{} = MacroReturnStruct ->
            MacroReturnStruct#{node => NodeA, state => Counter}
    end.

macro_name_str(#{module := Module, function := _Function, arity := _Arity}) ->
    atom_to_list(Module).

group_arguments(Arguments, #{group_args := true}) ->
    [Arguments];
group_arguments(Arguments, #{}) ->
    Arguments.

append_attrs(Arguments, #{attributes := Attrs, line := Line}) ->
    Arguments ++ [Attrs#{line => Line}];
append_attrs(Arguments, #{}) ->
    Arguments.

apply_mfa(Module, Function, Arguments) ->
    try
        erlang:apply(Module, Function, Arguments)
    catch
        _:Exception?CAPTURE_STACKTRACE ->
            StackTrace = ?GET_STACKTRACE,
            {error, {exception, Exception, Module, Function, StackTrace}}
    end.

update_with_counter(Tree, Module, Counter) ->
    Opts = #{traverse => post, formatter => ?MODULE},
    astranaut_traverse:map(
      fun(Node, _Attr) ->
              walk_var_counter(Node, Module, Counter)
      end, Tree, Opts).

walk_var_counter({var, Line, VarName} = Var, MacronameStr, Counter) ->
    case split_varname(atom_to_list(VarName)) of
        [Head, MacronameStr] ->
            VarName1 = list_to_atom(Head ++ "@" ++ MacronameStr ++ "@_" ++ Counter),
            {var, Line, VarName1};
        _ ->
            Var
    end;
walk_var_counter(Node, _ModuleStr, _Counter) ->
    Node.
%%%===================================================================
%%% format functions.
%%%===================================================================
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

format_forms(Forms) ->
    case astranaut:attributes(debug_macro, Forms) of
        [] ->
            ok;
        [true] ->
            io:format("~s~n", [astranaut:safe_to_string(Forms)]);
        [ast] ->
            io:format("~p~n", [Forms])
    end,
    Forms.

format_node(Node, #{file := File, line := Line} = Opts) ->
    case maps:get(debug, Opts, false) of
        true ->
            io:format("from ~s:~p ~s~n", [filename:basename(File), Line, format_mfa(Opts)]),
            io:format("~s~n", [astranaut:safe_to_string(Node)]);
        false ->
            ok
    end,
    case maps:get(debug_ast, Opts, false) of
        true ->
            io:format("from ~s:~p ~s~n", [filename:basename(File), Line, format_mfa(Opts)]),
            io:format("~p~n", [Node]);
        false ->
            ok
    end.
    
format_mfa(#{function := Function, arity := Arity, local := true}) ->
    io_lib:format("~p/~p", [Function, Arity]);
format_mfa(#{module := Module, function := Function, arity := Arity}) ->
    io_lib:format("~p:~p/~p", [Module, Function, Arity]).


