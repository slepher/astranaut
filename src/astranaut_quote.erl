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
-export([uncons/1, cons/2]).

%%%===================================================================
%%% API
%%%===================================================================
parse_transform(Forms, _Options) ->
    Opts = #{traverse => pre, formatter => ?MODULE, parse_transform => true},
    astranaut_traverse:map(fun walk/2, Forms, Opts).

format_error({invalid_quote, Node}) ->
    io_lib:format("invalid quote ~s", [astranaut:to_string(Node)]);
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

cons([H|T], Rest) ->
    {cons, 0, H, cons(T, Rest)};
cons([], Rest) ->
    Rest.

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
    case Line of
        {atom, _, code_line} ->
            Opts = #{line => Line1, quote_type => expression, code_line => true},
            quote(Form, Opts);
        _ ->
            Opts = #{line => Line1, quote_type => expression},
            call_remote(astranaut, replace_line_zero, [quote(Form, Opts), Line], Line1)
    end;
walk({call, Line1, {atom, _Line2, quote_code}, Codes} = Node, #{node := NodeType} = Attr) ->
    %% transform quote_code(Codes)
    case split_codes(Codes, NodeType) of
        {ok, NCodes, Line} ->
            Form = astranaut_code:quote_codes(NCodes),
            case Line of
                {atom, _, code_line} ->
                    Opts = #{line => Line1, quote_type => expression, code_line => true},
                    quote(Form, Opts);
                _ ->
                    Opts = #{line => Line1, quote_type => expression},
                    call_remote(astranaut, replace_line_zero, [quote(Form, Opts), Line], Line1)
            end;
        {ok, NCodes} ->
            Form = astranaut_code:quote_codes(NCodes),
            QuoteType = quote_type(Attr),
            Opts = #{line => Line1, quote_type => QuoteType},
            quote(Form, Opts);
        {error, invalid_quote_code} ->
            astranaut_traverse:traverse_fun_return(#{error => {invalid_quote, Node}})
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

quote_1({call, _Line1, {atom, _Line2, unquote}, [Unquote]}, Opts) ->
    unquote(Unquote, Opts#{type => value});
quote_1({cons, Line, {call, _Line1, {atom, _Line2, unquote_splicing}, [Unquotes]}, Rest}, Opts) ->
    %quote([a, b, unquote_splicing(V), c, d]),
    RestQuote = quote_1(Rest, Opts),
    call_remote(?MODULE, cons, [Unquotes, RestQuote], Line);
quote_1([{call, _Line1, {atom, _Line2, unquote_splicing}, [Unquotes]}|T], Opts) ->
    %quote({a, b, unquote_splicing(V), c, d}),
    unquote_splicing(Unquotes, T, Opts);
quote_1({match, _, {atom, _, unquote}, Unquote}, Opts) ->
    unquote(Unquote, Opts#{type => value});
quote_1([{match, _, {atom, _, unquote_splicing}, Unquotes}|T], Opts) ->
    unquote_splicing(Unquotes, T, Opts);
quote_1({match, _Line1, Pattern, Value}, #{line := Line, quote_type := pattern} = Opts) ->
    % _A@World = World2 => {atom, _, World} = World2
    {match, Line, quote_1(Pattern, Opts#{quote_type => pattern}), Value};
quote_1([{var, _Line1, Var} = VarForm|T], Opts) when is_atom(Var) ->
    unquote_if_binding_list(VarForm, T, Opts);
quote_1({var, _Line1, Var} = VarForm, Opts) when is_atom(Var) ->
    unquote_if_binding(VarForm, Opts);
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
            {tuple, TupleLine, quote_tuple_list(TupleList, Opts)};
        _ ->
            {tuple, Line, quote_tuple_list_1(TupleList, Opts)}
    end.

quote_tuple_list([MA, Spec], #{attribute := spec} = Opts) ->
    %% special form of {attribute, Line, spec, {{F, A}, Spec}}.
    %% there is no line in {F, A}.
    NOpts = maps:remove(attribute, Opts),
    [quote_1(MA, Opts#{attribute => attr}), quote_1(Spec, NOpts)];
quote_tuple_list(TupleList, #{attribute := attr} = Opts) ->
    %% special form of {attribute, Line, export, [{F, A}...]}.
    %% special form of {attribute, Line, Attribute, T}.
    %% there is no line in {F, A} and T.
    quote_tuple_list_1(TupleList, Opts);
quote_tuple_list([Action, TupleLine|Rest] = TupleList, Opts) ->
    NOpts = update_attribute_opt(TupleList, Opts),
    [quote_1(Action, Opts), quote_line(Opts#{line => TupleLine})|quote_tuple_list_1(Rest, NOpts)].

quote_tuple_list_1(List, Opts) ->
    lists:map(fun(Item) -> quote_1(Item, Opts) end, List).

quote_line(#{line := Line, quote_type := pattern}) ->
    {var, Line, '_'};
quote_line(#{line := Line, quote_type := expression, code_line := true} = Opts) ->
    quote_1(Line, Opts);
quote_line(#{quote_type := expression} = Opts) ->
    quote_1(0, Opts).

update_attribute_opt([attribute, _Line, spec|_T], Opts) ->
    Opts#{attribute => spec};
update_attribute_opt([attribute, _Line, type|_T], Opts) ->
    Opts;
update_attribute_opt([attribute|_T], Opts) ->
    Opts#{attribute => attr};
update_attribute_opt(_, Opts) ->
    Opts.

quote_type(#{node := pattern}) ->
    pattern;
quote_type(_) ->
    expression.

%% check ast in quote_code valid.
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

call_remote(Module, Function, Arguments, Line) ->
    {call, Line, {remote, Line, {atom, Line, Module}, {atom, Line, Function}}, Arguments}.

unquote_splicing(Unquotes, Rest, #{line := Line, quote_type := expression} = Opts) ->
    {op, Line, '++', call_remote(?MODULE, uncons, [Unquotes], Line), quote_1(Rest, Opts)};
unquote_splicing(Unquotes, _Rest, #{quote_type := pattern}) ->
    Unquotes.

unquote(Exp, #{type := value}) ->
    Exp;
unquote(Exp, #{type := Type, line := Line} = Opts) ->
    {tuple, Line, [quote_1(Type, Opts), quote_line(Opts), Exp]}.

unquote_if_binding_list({var, Line, Atom} = Form, T, #{} = Opts) ->
    case parse_binding(Atom, Line) of
        {value_list, VarList} ->
            unquote_splicing(VarList, T, Opts#{line => Line});
        _ ->
            {cons, Line, unquote_if_binding(Form, Opts#{line => Line}), quote_1(T, Opts#{line => Line})}
    end.

unquote_if_binding({var, Line, Atom} = Form, Opts) ->
    case parse_binding(Atom, Line) of
        {value, Var} ->
            unquote(Var, Opts#{type => value});
        {VarType, Var} ->
            unquote(Var, Opts#{type => VarType, line => Line});
        default ->
            quote_tuple(Form, Opts)
    end.

parse_binding(Atom, Line) ->
    case parse_binding_1(atom_to_list(Atom)) of
        {VarType, VarName} ->
            {VarType, {var, Line, list_to_atom(VarName)}};
        default ->
            default
    end.

parse_binding_1([$_,$A,$@|T]) ->
    {atom, T};
parse_binding_1([$_,$V,$@|T]) ->
    {var, T};
parse_binding_1([$_,$I,$@|T]) ->
    {integer, T};
parse_binding_1([$_,$F,$@|T]) ->
    {float, T};
parse_binding_1([$_,$S,$@|T]) ->
    {string, T};
parse_binding_1([$_,$L,$@|T]) ->
    {value_list, T};
parse_binding_1([$_,$@|T]) ->
    {value, T};
parse_binding_1(_) ->
    default.
