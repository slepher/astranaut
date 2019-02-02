%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2018, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 18 Nov 2018 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_macro).

-include("quote.hrl").

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
    {Macros, Warnings} = macros(Forms, Module, File),
    case astranaut_macro_local:compile(Macros, Forms, Options) of
        {ok, LWarnings} ->
            transform_macros_1(Macros, Forms, File, Warnings ++ LWarnings);
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
macros(Forms, LocalModule, File) ->
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

transform_macros_1(Macros, Forms, File, Warnings) ->
    Monad = transform_macros_2(Macros, Warnings),
    TraverseReturn = astranaut_traverse_monad:exec(Monad, Forms),
    astranaut_traverse:parse_transform_return(TraverseReturn, File).

transform_macros_2(Macros, Warnings) ->
    astranaut_traverse_monad:then(
      astranaut_traverse_monad:warnings(Warnings),
      astranaut_traverse_monad:then(
        astranaut_traverse_monad:map_m(
          fun(MacroOpts) ->
                  astranaut_traverse_monad:bind_state(
                    fun(Forms) ->
                            astranaut_traverse_monad:bind(
                              %% transform macro attributes
                              transform_macro_attr(MacroOpts, Forms),
                              fun(NForms) ->
                                      %% transform macro call
                                      transform_macro_call(MacroOpts, NForms)
                              end)
                    end)
          end, Macros),
        astranaut_traverse_monad:modify(
          fun(Forms) ->
                  format_forms(astranaut:reorder_exports(Forms))
          end))).

%%%===================================================================
%%% transform_macro_attr and it's help functions.
%%%===================================================================
transform_macro_attr(MacroOpts, Forms) ->
    astranaut_traverse_monad:then(
      astranaut_traverse_monad:put({1, Forms}),
      astranaut_traverse_monad:then(
        astranaut_traverse_monad:map_m(
          fun(Form) ->
                  Return = walk_macro_attr(Form, MacroOpts),
                  astranaut_traverse_monad:bind(
                    astranaut_traverse:fun_return_to_monad(Return, Form, MacroOpts),
                    fun(NForm) ->
                            %% modify the from list by transformed form.
                            astranaut_traverse_monad:modify(
                              fun({N, Forms1}) ->
                                      append_form(Form, NForm, MacroOpts, N, Forms1)
                              end)
                    end)
          end, Forms),
        % return the forms modified and set state ok(useless).
        astranaut_traverse_monad:state(
          fun({_N, Forms1}) ->
                  {Forms1, ok}
          end))).

walk_macro_attr({attribute, Line, exec_macro, {Function, Arguments}} = NodeA, #{macro := Function} = Opts) ->
    apply_macro(NodeA, Opts#{arguments => to_list(Arguments), line => Line});
walk_macro_attr({attribute, Line, exec_macro, {Module, Function, Arguments}} = NodeA,
                #{macro := {Module, Function}} = Opts) ->
    apply_macro(NodeA, Opts#{arguments => to_list(Arguments), line => Line});
walk_macro_attr({attribute, Line, Attribute, Arguments} = NodeA, #{as_attr := Attribute} = Opts) ->
    apply_macro(NodeA, Opts#{arguments => to_list(Arguments), line => Line});
walk_macro_attr(Node, _MacroOpts) ->
    Node.

to_list(Arguments) when is_list(Arguments) ->
    Arguments;
to_list(Arguments) ->
    [Arguments].

append_form(Form, Form, _MacroOpts, N, Acc) ->
    {N + 1, Acc};
append_form(Form, NForm, MacroOpts, N, Acc) ->
    Line = erl_syntax:get_pos(Form),
    {Nodes, NAcc} = update_forms(to_list(NForm), Acc, MacroOpts#{line => Line}),
    NNAcc = astranaut:replace_from_nth(Nodes, N, NAcc),
    {N + length(Nodes), NNAcc}.

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
transform_macro_call(MacroOpts, Forms) ->
    Traverse = maps:get(order, MacroOpts, post),
    Opts = #{traverse => Traverse, formatter => ?MODULE},
    astranaut_traverse:map_m(
      fun(Node, _Attr) ->
              Return = walk_macro_call(Node, MacroOpts),
              astranaut_traverse:fun_return_to_monad(Return, Node, MacroOpts)
      end, Forms, Opts).

walk_macro_call({call, Line, {atom, _Line2, Function}, Arguments} = Node, #{macro := Function} = Opts) ->
    apply_macro(Node, Opts#{arguments => Arguments, line => Line});
walk_macro_call({call, Line, {remote, Line2, {atom, Line2, Module}, {atom, Line2, Function}}, Arguments} = Node, 
                #{macro := {Module, Function}} = Opts) ->
    apply_macro(Node, Opts#{arguments => Arguments, line => Line});
walk_macro_call(Node, __MacroOpts) ->
    Node.

%%%===================================================================
%%% apply_macro and it's help functions.
%%%===================================================================
apply_macro(NodeA, #{module := Module, function := Function, arity := Arity, 
                     arguments := Arguments, line := Line} = Opts) ->
    Arguments1 = group_arguments(Arguments, Opts),
    Arguments2 = append_attrs(Arguments1, Opts),
    if
        length(Arguments2) == Arity ->
            MacroReturn = apply_mfa(Module, Function, Arguments2),
            case astranaut_traverse:traverse_fun_return_struct(MacroReturn) of
                #{node := NodeB} = MacroReturnStruct ->
                    NodeC = astranaut:replace_line_zero(NodeB, Line),
                    format_node(NodeC, Opts),
                    MacroReturnStruct#{node => NodeC};
                #{} = MacroReturnStruct ->
                    MacroReturnStruct#{node => NodeA}
            end;
        true ->
            astranaut_traverse:traverse_fun_return(#{node => NodeA})
    end.

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
        _:Exception:StackTrace ->
            {error, {exception, Exception, Module, Function, StackTrace}}
    end.

%%%===================================================================
%%% format functions.
%%%===================================================================
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


