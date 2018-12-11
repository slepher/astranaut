-module(astranaut).

%% API exports
-export([attributes/2, attributes_with_line/2, module_attributes/2, read/1]).
-export([file/1]).
-export([function/2, merge_clauses/1]).
-export([replace_line/2, to_string/1]).
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

replace_line(Ast, Line) ->
    astranaut_traverse:map(
      fun(Tuple) when is_tuple(Tuple) ->
              TupleList = tuple_to_list(Tuple),
              case TupleList of
                  [_Action, TupleLine|_Rest] when is_integer(TupleLine) ->
                      setelement(2, Tuple, Line);
                  _ ->
                      Tuple
              end;
         (Node) ->
              Node
         end, Ast, pre).

function(Name, {'fun', Line, {clauses, Clauses}}) ->
    Arity = clause_arity(Clauses),
    {function, Line, Name, Arity, Clauses}.

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

%%====================================================================
%% Internal functions
%%====================================================================
clause_arity([{clause, _Line, Patterns, _Guards, _Body}|_T]) ->
    length(Patterns).
