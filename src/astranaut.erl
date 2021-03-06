%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 17 Oct 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut).

%% API exports
-export([attributes/2, attributes_with_line/2, attribute_nodes/2,  module_attributes/2, read/1]).
-export([is_mfa/1, is_opts/1]).
-export([attribute_node/3]).
-export([abstract/1, abstract/2]).
-export([file/1, module/1]).
-export([exports/1, exports/2, exported_function/2, function/2, function_fa/1, merge_clauses/1]).
-export([replace_line/2, replace_line_zero/2, safe_to_string/1, to_string/1]).
-export([replace_from_nth/3]).
-export([reorder_exports/1, reorder_attributes/2]).
-export([update_option_warnings/2, validate_options/2]).
-export([ast_to_options/1, ast_to_options/2]).
-export([relative_path/1]).
%%====================================================================
%% API functions
%%====================================================================

%% this method is from https://github.com/efcasado/forms/blob/master/src/forms.erl
-spec read(atom() | iolist()) -> [erl_parse:abstract_form()].
read(Module) when is_atom(Module) ->
    case beam_lib:chunks(code:which(Module), [abstract_code]) of
        {ok, {Module, [{abstract_code, {raw_abstract_v1, Forms}}]}} ->
            Forms;
        {ok, {no_debug_info, _}} ->
            throw({forms_not_found, Module});
        {error, beam_lib, {file_error, _, enoent}} ->
            throw({module_not_found, Module})
    end;
read(File) ->
    case epp:parse_file(File, []) of
        {ok, Forms} ->
            Forms;
        {ok, Forms, _Extra} ->
            Forms;
        {error, enoent} ->
            throw({file_not_found, File})
    end.

abstract(Term) ->
    erl_syntax:revert(erl_syntax:abstract(Term)).

abstract(Term, Line) ->
    replace_line(abstract(Term), Line).

-spec attributes(atom(), _Forms) -> [_Attribute].

module_attributes(Attribute, Module) ->
    Attributes = Module:module_info(attributes),
    lists:reverse(
      lists:foldl(
        fun({Attr, Value}, Acc) when Attr == Attribute ->
                [Value|Acc];
           (_Other, Acc) ->
                Acc
        end, [], Attributes)).

file(Forms) ->
    [{File, _}|_] = attributes(file, Forms),
    File.

module(Forms) ->
    [Module|_] = attributes(module, Forms),
    Module.

attribute_nodes(Attribute, Forms) ->
    lists:reverse(
      lists:foldl(
        fun({attribute, _Line, Attr, _Values} = Node, Acc) when Attr == Attribute ->
                [Node|Acc];
           (_Other, Acc) ->
                Acc
        end, [], Forms)).

attributes(Attribute, Forms) ->
    lists:reverse(
      lists:foldl(
        fun({attribute, _Line, Attr, Values}, Acc) when Attr == Attribute ->
                [Values|Acc];
           (_Other, Acc) ->
                Acc
        end, [], Forms)).

attributes_with_line(Attribute, Forms) ->
    lists:reverse(
      lists:foldl(
        fun({attribute, Line, Attr, Values}, Acc) when Attr == Attribute ->
                [{Line, Values}|Acc];
           (_Other, Acc) ->
                Acc
        end, [], Forms)).

attribute_node(Name, Line, Value) when is_atom(Name), is_integer(Line) ->
    {attribute, Line, Name, Value}.

is_mfa({Module, {Function, Arity}}) when is_atom(Module), is_atom(Function), is_integer(Arity) ->
    true;
is_mfa({Function, Arity}) when is_atom(Function), is_integer(Arity) ->
    true;
is_mfa(Function) when is_atom(Function) ->
    true;
is_mfa(_) ->
    false.

is_opts(Opts) when is_list(Opts) ->
    lists:all(fun is_opts_element/1, Opts);
is_opts(Opts) when is_map(Opts) ->
    true;
is_opts(Opts) when is_atom(Opts) ->
    true;
is_opts(_Opts) ->
    false.

is_opts_element(OptsElement) when is_atom(OptsElement) ->
    true;
is_opts_element({Key, _Value}) when is_atom(Key) ->
    true;
is_opts_element(_OptsElement) ->
    false.

replace_line(Ast, Line) ->
    replace_line_cond(fun(_) -> true end, Ast, Line).

replace_line_zero(Ast, 0) ->
    Ast;
replace_line_zero(Ast, Line) ->
    replace_line_cond(
      fun(0) -> true;
         (_) -> false
      end, Ast, Line).

replace_line_cond(Cond, Ast, Line) when is_integer(Line) ->
    astranaut_traverse:map(
      fun(Node, #{node := attribute}) ->
              Node;
         (Tuple, _Attr) when is_tuple(Tuple) ->
              case tuple_to_list(Tuple) of
                  [_Action, TupleLine|_Rest] when is_integer(TupleLine) ->
                      case Cond(TupleLine) of
                          true ->
                              setelement(2, Tuple, Line);
                          false ->
                              Tuple
                      end;
                  _ ->
                      Tuple
              end;
         (Node, _Attr) ->
              Node
         end, Ast, #{traverse => pre}).

exported_function(Name, {'fun', Line, {clauses, _Clauses}} = Fun) ->
    Function = function(Name, Fun),
    FunctionFa = function_fa(Function),
    [exports([FunctionFa], Line), Function].

function(Name, {'fun', Line, {clauses, Clauses}}) ->
    Arity = clause_arity(Clauses),
    {function, Line, Name, Arity, Clauses}.

function_fa({function, _Line, Name, Arity, _Clauses}) ->
    {Name, Arity}.

exports(Exports) ->
    exports(Exports, 0).

exports(Exports, Line) ->
    {attribute, Line, export, Exports}.

reorder_exports(Forms) ->
    reorder_attributes(Forms, #{docks => [export], spec_as_fun => true, dock_spec => true}).

reorder_attributes(Forms, Options) ->
    {_, GroupForms} =
        lists:foldl(
          fun(Form, {AccN, Acc}) ->
                  {AccN + 1, [{AccN, Form}|Acc]}
          end, {1, []}, Forms),
    lists:foldl(
      fun({N, {attribute, _, _, _} = Attribute}, Acc) ->
              case astranaut_erl_syntax:is_file(Attribute) of
                  {file, _File} ->
                      Acc;
                  _ ->
                      replace_from_nth(Attribute, N, Acc, Options)
              end;
         (_, Acc) ->
              Acc
      end, Forms, lists:reverse(GroupForms)).

safe_to_string(Form) ->
    try 
        to_string(Form)
    catch
        _:Exception ->
            io_lib:format("ast could not format ~p~n~p", [Exception, Form])
    end.

to_string(Forms) when is_list(Forms) ->
    erl_prettypr:format(erl_syntax:form_list(Forms));
to_string(Form) ->
    erl_prettypr:format(erl_syntax:form_list([Form])).

merge_clauses([{'fun', Line, {clauses, _}}|_T] = Nodes) -> 
    NClauses =
        lists:flatten(
          lists:map(
            fun({'fun', _, {clauses, FClauses}}) ->
                    FClauses
            end, Nodes)),
    {'fun', Line, {clauses, NClauses}}.

replace_from_nth(Nodes, N, Forms) ->
    replace_from_nth(Nodes, N, Forms, #{docks => [export]}).

replace_from_nth(Nodes, N, Forms, #{docks := DockAttributesGroups} = Options) when is_list(Nodes) ->
    DockMap =
        lists:foldl(
          fun(AttrributePos, Acc) ->
                  case lists:nth(AttrributePos, DockAttributesGroups) of
                      Attributes when is_list(Attributes) ->
                          lists:foldl(
                            fun(Attribute, Acc1) ->
                                    maps:put(Attribute, AttrributePos, Acc1)
                            end, Acc, Attributes);
                      Attribute when is_atom(Attribute) ->
                          maps:put(Attribute, AttrributePos, Acc)
                  end
          end, maps:new(), lists:seq(1, length(DockAttributesGroups))),
    replace_from_nth(Nodes, N, Forms, [], Options#{dock_map => DockMap});
replace_from_nth(Node, N, Forms, Options) when N > 0 ->
    replace_from_nth([Node], N, Forms, Options).
    
replace_from_nth(Nodes, 1, [_|Forms], Heads, _Options) ->
    lists:reverse(Heads) ++ Nodes ++ Forms;
replace_from_nth(Nodes, N, [Form|Forms], Heads, #{dock_map := DockAttributes} = Options) ->
    PartitionFun =
        case Form of
            {function, _Line0, NameF, ArityF, _Clauses} ->
                fun({attribute, _Line1, spec, {{NameS, ArityS}, _SpecValue}}) ->
                        case maps:get(dock_spec, Options, true) of
                            true ->
                                (NameS == NameF) and (ArityS == ArityF);
                            false ->
                                maps:is_key(spec, DockAttributes)
                        end;
                   ({attribute, _Line1, Attribute, _AttributeValue}) ->
                        case maps:get(force, Options, false) of
                            true ->
                                true;
                            false ->
                                maps:is_key(Attribute, DockAttributes)
                        end;
                   (_) ->
                        false
                end;
            {attribute, _line0, Attribute1, _AttributeValue1} ->
                SpecAsFun = maps:get(spec_as_fun, Options, false),
                if
                   SpecAsFun and (Attribute1 == spec) ->
                        fun({attribute, _Line1, Attribute, _AttributeValue}) ->
                                case maps:get(force, Options, false) of
                                    true ->
                                        true;
                                    false ->
                                        maps:is_key(Attribute, DockAttributes)
                                end;
                           (_) ->
                                false
                        end;
                    true ->
                        case maps:find(Attribute1, DockAttributes) of
                            {ok, AttributePos1} ->
                                fun({attribute, _Line1, Attribute2, _AttributeValue2}) ->
                                        case maps:find(Attribute2, DockAttributes) of
                                            {ok, AttributePos2} ->
                                                AttributePos1 > AttributePos2;
                                            error ->
                                                false
                                        end;
                                   (_) ->
                                        false
                                end;
                            error ->
                                fun(_) -> false end
                        end
                end;
            {eof, _Line} ->
                fun(_) -> true end;
            _ ->
                fun(_) -> false end
        end,
    {Docks, Rests} = lists:partition(PartitionFun, Nodes),
    replace_from_nth(Rests, N - 1, Forms, [Form|lists:reverse(Docks) ++ Heads], Options);

replace_from_nth(Nodes, N, [Head|Forms], Heads, WithExports) ->
    replace_from_nth(Nodes, N - 1, Forms, [Head|Heads], WithExports).

ast_to_options(Ast) ->
    ast_to_options(Ast, []).

ast_to_options(Ast, ExcepKeys) ->
    Writer = astranaut_monad_writer_t:writer_t(astranaut_monad_identity:new()),
    MonadWriter = ast_to_options(Ast, ExcepKeys, Writer),
    astranaut_monad_identity:run(astranaut_monad_writer_t:run(MonadWriter)).

ast_to_options({cons, _Line, Head, Tail}, Keys, Writer) ->
    astranaut_monad:bind(
      ast_to_value(Head, Writer),
      fun(Head1) ->
              astranaut_monad:bind(
                ast_to_options(Tail, Keys, Writer),
                fun(Tail1) ->
                        astranaut_monad:return([Head1|Tail1], Writer)
                end, Writer)
      end, Writer);
ast_to_options({nil, _Line}, _Keys, Writer) ->
    astranaut_monad:return([], Writer);
ast_to_options({map, _Line, MapAssocs}, Keys, Writer) ->
    astranaut_monad:foldl_m(
      fun({map_field_assoc, _, {atom, _LineA, Key}, Value}, Acc) ->
              case lists:member(Key, Keys) of
                  true ->
                      astranaut_monad:return(maps:put(Key, Value, Acc), Writer);
                  false ->
                      astranaut_monad:bind(
                        ast_to_value(Value, Writer),
                        fun(Value1) ->
                                astranaut_monad:return(maps:put(Key, Value1, Acc), Writer)
                        end, Writer)
              end;
         ({map_field_assoc, _, Key, _Value}, Acc) ->
              astranaut_monad:then(
                astranaut_monad:tell([{invalid_option_key, Key}], Writer),
                astranaut_monad:return(Acc, Writer),
                Writer)
      end, maps:new(), MapAssocs, Writer);
ast_to_options(Value, _Keys, Warnings) ->
    ast_to_value(Value, Warnings).

ast_to_value({Type, _Line, Value}, Writer) when Type == atom ; Type == integer; Type == float; Type == string ->
    astranaut_monad:return(Value, Writer);
ast_to_value(Value, Writer) ->
    astranaut_monad:return(Value, Writer).

update_option_warnings(OptionName, Warnings) ->
    lists:map(
      fun({invalid_option, Value}) ->
              {invalid_option_value, OptionName, Value};
         ({invalid_option_value, Key, Value})  ->
              {invalid_option_value, OptionName, Key, Value}
      end, Warnings).

validate_options(F, Options) ->
    validate_options(F, Options, []).

validate_options(F, Options, Warnings) when is_map(Options) ->
    maps:fold(
        fun(Key, Value, {OptionsAcc, WarningAcc} = Acc) ->
                case apply_f(F, Key, Value, Options) of
                    ok ->
                        Acc;
                    error ->
                        NWarningsAcc = [{invalid_option_value, Key, Value}|WarningAcc],
                        NOptsAcc = maps:remove(Key, OptionsAcc),
                        {NOptsAcc, NWarningsAcc};
                    {error, Reason} ->
                        NWarningsAcc = [Reason|WarningAcc],
                        NOptsAcc = maps:remove(Key, OptionsAcc),
                        {NOptsAcc, NWarningsAcc}
                end
        end, {Options, Warnings}, Options);
validate_options(F, Options, Warnings) when is_list(Options) ->
    {Options1, Warnings1} = 
        lists:foldl(
          fun({Key, Value}, {OptionsAcc, WarningsAcc}) when is_atom(Key) ->
                  {maps:put(Key, Value, OptionsAcc), WarningsAcc};
             (Key, {OptionsAcc, WarningsAcc}) when is_atom(Key) ->
                  {maps:put(Key, true, OptionsAcc), WarningsAcc};
             (Value, {OptionsAcc, WarningsAcc}) ->
                  {OptionsAcc, [{invalid_option_value, Value}|WarningsAcc]}
          end, {maps:new(), Warnings}, Options),
    validate_options(F, Options1, Warnings1);
validate_options(F, Attr, Warings) when is_atom(Attr) ->
    validate_options(F, #{Attr => true}, Warings);
validate_options(F, {Key, Value}, Warings) when is_atom(Key) ->
    validate_options(F, #{Key => Value}, Warings);
validate_options(_F, Options, Warnings) ->
    {maps:new(), [{invalid_option, Options}|Warnings]}.

relative_path(Path) ->
    case file:get_cwd() of
        {ok, BasePath} ->
            string:replace(Path, BasePath ++ "/", "");
        {error, _Reason} ->
            Path
    end.

%%====================================================================
%% Internal functions
%%====================================================================
clause_arity([{clause, _Line, Patterns, _Guards, _Body}|_T]) ->
    length(Patterns).

apply_f(F, Key, Value, _Options) when is_function(F, 2) ->
    F(Key, Value);
apply_f(F, Key, Value, Options) when is_function(F, 3) ->
    F(Key, Value, Options).
