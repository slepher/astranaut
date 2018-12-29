%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2018, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 19 Oct 2018 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_quote).

%% API
-export([parse_transform/2, format_error/1]).
-export([quote/1, quote/2]).
-export([uncons/1]).

%%%===================================================================
%%% API
%%%===================================================================
parse_transform(Forms, _Options) ->
    astranaut_traverse:map(fun walk/2, Forms, #{traverse => pre, module => ?MODULE}).

format_error(Message) ->
    case io_lib:deep_char_list(Message) of
        true -> Message;
        _    -> io_lib:write(Message)
    end.

quote(Value) ->
    quote(Value, #{line => 0, quote_type => expression}).

quote(Value, Line) when is_integer(Line) ->
    quote_1(Value, #{line => Line, quote_type => expression});
quote(Value, Opts) when is_map(Opts) ->
    quote_1(Value, Opts).

uncons({cons, _Line, Head, Tail}) ->
    [Head|uncons(Tail)];
uncons({nil, _Line}) ->
    [];
uncons(Value) ->
    Value.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
walk({call, Line1, {atom, _Line2, quote}, [Form]}, Attr) ->
    %% transform quote(Form)
    QuoteType = quote_type(Attr),
    Opts = #{line => Line1, quote_type => QuoteType},
    quote(Form, Opts);
walk({call, Line1, {atom, _Line2, quote}, [Form, Line]}, #{node := expression}) ->
    %% transform quote(Form, Line) in expression
    Opts = #{line => Line1, quote_type => expression, replaced_line => true},
    call_remote(astranaut, replace_line_zero, [quote(Form, Opts), Line], Line1);
walk({call, Line1, {atom, _Line2, quote_code}, Codes} = Node, #{node := NodeType} = Attr) ->
    %% transform quote_code(Codes)
    case split_codes(Codes, NodeType) of
        {ok, NCodes, Line} ->
            Form = astranaut_code:quote_codes(NCodes),
            Opts = #{line => Line1, quote_type => expression, replaced_line => true},
            call_remote(astranaut, replace_line_zero, [quote(Form, Opts), Line], Line1);
        {ok, NCodes} ->
            Form = astranaut_code:quote_codes(NCodes),
            QuoteType = quote_type(Attr),
            Opts = #{line => Line1, quote_type => QuoteType},
            quote(Form, Opts);
        {error, invalid_quote_code} ->
            #{error => io_lib:format("invalid quote ~s", [astranaut:to_string(Node)])}
    end;
walk({match, Line1, {atom, _Line2, quote}, Form}, #{node := pattern}) ->
    %% transform quote = Form in pattern match
    Opts = #{line => Line1, quote_type => pattern},
    quote(Form, Opts);
walk({match, Line1, {atom, _Line2, quote_code}, Code}, #{node := pattern}) ->
    %% transform quote_code = Code in pattern match
    Form = astranaut_code:quote_codes([Code]),
    Opts = #{line => Line1, quote_type => pattern},
    quote(Form, Opts);
walk(Node, _Attr) ->
    Node.

quote_1({unquote, Unquote}, _Opts) ->
    Unquote;
quote_1({call, _Line1, {atom, _Line2, unquote}, [Unquote]}, _Opts) ->
    Unquote;
quote_1([{call, _Line1, {atom, _Line2, unquote_splicing}, [Unquotes]}|T], Opts) ->
    unquote_splicing_form(Unquotes, T, Opts);
quote_1({match, _, {atom, _, unquote}, Unquote}, _Opts) ->
    Unquote;
quote_1([{match, _, {atom, _, unquote_splicing}, Unquotes}|T], Opts) ->
    unquote_splicing_form(Unquotes, T, Opts);
quote_1({match, _Line1, Pattern, Value}, #{line := Line} = Opts) ->
    {match, Line, quote_1(Pattern, Opts#{quote_type => pattern}), Value};

quote_1([{var, __Line1, Var} = VarForm|T], Opts) when is_atom(Var) ->
    metavariable_list(VarForm, T, Opts);
quote_1({var, _Line1, Var} = VarForm, Opts) when is_atom(Var) ->
    metavariable(VarForm, Opts);
quote_1({atom, _Line1, Atom} = VarForm, Opts) when is_atom(Atom) ->
    metavariable(VarForm, Opts);

quote_1(Tuple, Opts) when is_tuple(Tuple) ->
    quote_tuple(Tuple, Opts);
quote_1([H|T], #{line := Line} = Opts) ->
    {cons, Line, quote_1(H, Opts), quote_1(T, Opts)};
quote_1([], #{line := Line}) ->
    {nil, Line};
quote_1(Float, #{line := Line}) when is_float(Float) ->
    {float, Line, Float};
quote_1(Integer, #{line := Line}) when is_integer(Integer) ->
    {integer, Line, Integer};
quote_1(Atom, #{line := Line}) when is_atom(Atom) ->
    {atom, Line, Atom}.

quote_tuple(Tuple, #{line := Line} = Opts) ->
    TupleList = tuple_to_list(Tuple),
    case TupleList of
        [_Action, TupleLine|_Rest] when is_integer(TupleLine) ->
            NOpts = Opts#{line => TupleLine},
            {tuple, TupleLine, quote_tuple_list(TupleList, NOpts)};
        _ ->
            {tuple, Line, quote_tuple_list_1(TupleList, Opts)}
    end.

quote_tuple_list([Action, _TupleLine|Rest], #{quote_type := expression, replaced_line := true} = Opts) ->
    quote_tuple_list_1([Action, 0|Rest], Opts);
quote_tuple_list(TupleList, #{quote_type := expression} = Opts) ->
    quote_tuple_list_1(TupleList, Opts);
quote_tuple_list([Action, TupleLine|Rest], #{quote_type := pattern} = Opts) ->
    [quote_1(Action, Opts), {var, TupleLine, '_'}|quote_tuple_list_1(Rest, Opts)].

quote_tuple_list_1(List, Opts) ->
    lists:map(fun(Item) -> quote_1(Item, Opts) end, List).

call_remote(Module, Function, Arguments, Line) ->
    {call, Line, {remote, Line, {atom, Line, Module}, {atom, Line, Function}}, Arguments}.

quote_type(#{node := pattern}) ->
    pattern;
quote_type(_) ->
    expression.

split_codes(Codes, NodeType) ->
    case lists:partition(
           fun({string, _, _}) ->
                   true;
              (_) ->
                   false
           end, Codes) of
        {NCodes, [Line]} ->
            case NodeType of
                expression ->
                    {ok, NCodes, Line};
                _ ->
                    {error, invalid_quote_code}
            end;
        {NCodes, []} ->
            {ok, NCodes};
        {_NCodes, _NonCodes} ->
            {error, invalid_quote_code}
    end.

unquote_splicing_form(Unquotes, Rest, #{line := Line, quote_type := expression} = Opts) ->
    {op, Line, '++', call_remote(?MODULE, uncons, [Unquotes], Line), quote_1(Rest, Opts)};
unquote_splicing_form(Unquotes, _Rest, #{quote_type := pattern}) ->
    Unquotes.

metavariable_list({FType, _, Atom} = Form, T, #{line := Line} = Opts) ->
    case parse_metavariable(Atom, FType, Line) of
        {value_list, VarList} ->
            unquote_splicing_form(VarList, T, Opts);
        _ ->
            {cons, Line, metavariable(Form, Opts), quote_1(T, Opts)}
    end.

metavariable({FType, _, Atom} = Form, #{line := Line, quote_type := Type} = Opts) ->
    case parse_metavariable(Atom, FType, Line) of
        {value, Var} ->
            Var;
        {VarType, Var} ->
            {tuple, Line, [quote_1(VarType, Opts), line_variable(Line, Line, Type), Var]};
        default ->
            quote_tuple(Form, Opts)
    end.

line_variable(Line0, Line, expression) ->
    {integer, Line, Line0};
line_variable(_Line0, Line, pattern) ->
    {var, Line, '_'}.

parse_metavariable(Atom, var, Line) ->
    case atom_to_list(Atom) of
        [$_|T] ->
            parse_metavariable(T, Line);
        _ ->
            default
    end;
parse_metavariable(Atom, atom, Line) ->
    parse_metavariable(atom_to_list(Atom), Line).

parse_metavariable([$A,$@|T], Line) ->
    {atom, {var, Line, list_to_atom(T)}};
parse_metavariable([$V,$@|T], Line) ->
    {var, {var, Line, list_to_atom(T)}};
parse_metavariable([$I,$@|T], Line) ->
    {integer, {var, Line, list_to_atom(T)}};
parse_metavariable([$F,$@|T], Line) ->
    {float, {var, Line, list_to_atom(T)}};
parse_metavariable([$S,$@|T], Line) ->
    {string, {var, Line, list_to_atom(T)}};
parse_metavariable([$L,$@|T], Line) ->
    {value_list, {var, Line, list_to_atom(T)}};
parse_metavariable([$@|T], Line) ->
    {value, {var, Line, list_to_atom(T)}};
parse_metavariable(_, _Line) ->
    default.
