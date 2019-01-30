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
-export([uncons/2, cons/2]).

%%%===================================================================
%%% API
%%%===================================================================
parse_transform(Forms, _Options) ->
    Opts = #{traverse => pre, formatter => ?MODULE},
    File = astranaut:file(Forms),
    NodeM = astranaut_traverse:map_m(fun(Node, Attr) -> walk(Node, Attr, File) end, Forms, Opts),
    Reply = astranaut_traverse_monad:run(NodeM, ok),
    NReply = astranaut_traverse:map_traverse_return(
               fun({NNode, _State}) ->
                       NNode
               end, Reply),
    astranaut_traverse:parse_transform_return(NReply, File).


format_error({invalid_quote, Node}) ->
    io_lib:format("invalid quote ~s", [astranaut:to_string(Node)]);
format_error(Message) ->
    case io_lib:deep_char_list(Message) of
        true -> Message;
        _    -> io_lib:write(Message)
    end.

quote(Value) ->
    quote(Value, #{}).

quote(Value, Options) ->
    Options1 = maps:merge(#{quote_line => 0, quote_type => expression}, Options),
    quote_0(Value, Options1).

uncons(Cons, []) ->
    uncons_1(Cons);
uncons(Cons, Rest) ->
    uncons_1(Cons) ++ Rest.

uncons_1({cons, _Line, Head, Tail}) ->
    [Head|uncons_1(Tail)];
uncons_1({nil, _Line}) ->
    [];
uncons_1(Value) ->
    Value.

cons([H|T], Rest) ->
    {cons, 0, H, cons(T, Rest)};
cons([], Rest) ->
    Rest;
cons({cons, Line, Head, Tail}, Rest) ->
    {cons, Line, Head, cons(Tail, Rest)};
cons({nil, _Line}, Rest) ->
    Rest.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
walk({call, _Line1, {atom, _Line2, quote}, [Form]} = Node, Attr, File) ->
    %% transform quote(Form)
    quote(Form, #{}, Node, Attr, File);
walk({call, _Line1, {atom, _Line2, quote}, [Form, Options]} = Node, Attr, File) ->
    Options1 = to_options(Options),
    quote(Form, Options1, Node, Attr, File);
walk({call, _Line1, {atom, _Line2, quote_code}, Codes} = Node, #{node := NodeType} = Attr, File) ->
    %% transform quote_code(Codes)
    case split_codes(Codes, NodeType) of
        {ok, NCodes, Options} ->
            Options1 = to_options(Options),
            Form = astranaut_code:quote_codes(NCodes),
            quote(Form, Options1, Node, Attr, File);
        {error, invalid_quote_code} ->
            astranaut_traverse:traverse_fun_return(#{error => {invalid_quote, Node}})
    end;
walk({match, _Line1, {atom, _Line2, quote}, Form} = Node, #{node := pattern} = Attr, File) ->
    %% transform quote = Form in pattern match
    quote(Form, #{}, Node, Attr, File);
walk({match, _Line1, {atom, _Line2, quote_code}, Code} = Node, #{node := pattern} = Attr, File) ->
    %% transform quote_code = Code in pattern match
    Form = astranaut_code:quote_codes([Code]),
    quote(Form, #{}, Node, Attr, File);
walk(Node, _Attr, _File) ->
    astranaut_traverse_monad:return(Node).

quote(Value, Options, Node, Attr, File) ->
    QuoteLine = erl_syntax:get_pos(Node),
    QuoteType = quote_type(Attr),
    Options1 = maps:merge(#{quote_line => QuoteLine, quote_type => QuoteType, file => File}, Options),
    quote(Value, Options1).

quote_0(Value, #{debug := true, quote_line := QuoteLine, file := File} = Options) ->
    astranaut_traverse_monad:lift_m(
      fun(QuotedAst) ->
              QuotedCode = astranaut:to_string(QuotedAst),
              RelaPath = astranaut:relative_path(File),
              io:format("~ts:~p~n~s~n", [RelaPath, QuoteLine, QuotedCode]),
              QuotedAst
      end, quote_0(Value, Options#{debug => false}));
quote_0(Value, #{line := Line, quote_line := QuoteLine} = Options) ->
    Options1 = maps:remove(line, Options),
    astranaut_traverse_monad:lift_m(
      fun(Quoted) ->
              call_remote(astranaut, replace_line_zero, [Quoted, Line], QuoteLine)
      end, quote_0(Value, Options1));
quote_0(Value, #{warnings := Warnings} = Options) ->
    Options1 = maps:remove(warnings, Options),
    astranaut_traverse_monad:then(
      astranaut_traverse_monad:warnings(Warnings),
      quote_0(Value, Options1));
quote_0(Value, #{} = Options) ->
    quote_m(Value, Options).

quote_1({call, _Line1, {atom, _Line2, unquote}, [Unquote]}, Opts) ->
    unquote(Unquote, Opts#{type => value});
quote_1({cons, _Line1, {call, _Line2, {atom, _Line3, unquote_splicing}, [Unquotes]}, T}, Opts) ->
    %quote([a, b, unquote_splicing(V), c, d]),
    unquote_splicing(Unquotes, T, Opts#{join => cons});
quote_1([{call, _Line1, {atom, _Line2, unquote_splicing}, [Unquotes]}|T], Opts) ->
    %quote({a, b, unquote_splicing(V), c, d}),
    unquote_splicing(Unquotes, T, Opts#{join => list});
quote_1({match, _, {atom, _, unquote}, Unquote}, Opts) ->
    unquote(Unquote, Opts#{type => value});
quote_1([{match, _, {atom, _, unquote_splicing}, Unquotes}|T], Opts) ->
    unquote_splicing(Unquotes, T, Opts#{join => list});
quote_1({match, _Line1, Pattern, Value}, #{quote_line := Line, quote_type := pattern} = Opts) ->
    % _A@World = World2 => {atom, _, World} = World2
    astranaut_traverse_monad:bind(
      quote_m(Pattern, Opts#{quote_type => pattern}),
      fun(Pattern1) ->
              astranaut_traverse_monad:return({match, Line, Pattern1, Value})
      end);
quote_1({cons, _Line1, {var, Line, VarName}, T} = Tuple, Opts) when is_atom(VarName) ->
    case parse_binding(VarName, Line) of
        {value_list, Binding} ->
            unquote_splicing(Binding, T, Opts#{join => cons});
        default ->
            quote_tuple(Tuple, Opts)
    end;
quote_1([{var, Line, VarName} = Var|T], Opts) when is_atom(VarName) ->
    case parse_binding(VarName, Line) of
        {value_list, Binding} ->
            unquote_splicing(Binding, T, Opts#{quote_line => Line, join => list});
        _ ->
            quote_list([Var|T], Opts)
    end;
quote_1({var, Line, VarName} = Var, Opts) when is_atom(VarName) ->
    case parse_binding(VarName, Line) of
        {value_list, Binding} ->
            astranaut_traverse_monad:then(
              astranaut_traverse_monad:warning({Line, ?MODULE, {invalid_binding, Binding}}),
              quote_tuple(Var, Opts));
        {BindingType, Binding} ->
            unquote(Binding, Opts#{type => BindingType, quote_line => Line});
        default ->
            quote_tuple(Var, Opts)
    end;
quote_1(Tuple, Opts) when is_tuple(Tuple) ->
    quote_tuple(Tuple, Opts);
quote_1(List, Opts) when is_list(List) ->
    quote_list(List, Opts);
quote_1(Float, #{quote_line := Line}) when is_float(Float) ->
    {float, Line, Float};
quote_1(Integer, #{quote_line := Line}) when is_integer(Integer) ->
    {integer, Line, Integer};
quote_1(Atom, #{quote_line := Line}) when is_atom(Atom) ->
    {atom, Line, Atom}.

quote_m(Node, #{quote_line := Line} = Options) ->
    Node1 = quote_1(Node, Options),
    astranaut_traverse:fun_return_to_monad(Node1, Node, #{line => Line, formatter => ?MODULE}).
    
quote_list([H|T], #{quote_line := Line} = Opts) ->
    astranaut_traverse_monad:bind(
      quote_m(H, Opts),
      fun(H1) ->
              astranaut_traverse_monad:bind(
                quote_m(T, Opts),
                fun(T1) ->
                        astranaut_traverse_monad:return({cons, Line, H1, T1})
                end)
      end);
quote_list([], #{quote_line := Line}) ->
    astranaut_traverse_monad:return({nil, Line}).

quote_tuple(Tuple, #{quote_line := Line} = Opts) ->
    TupleList = tuple_to_list(Tuple),
    case TupleList of
        [_Action, TupleLine|_Rest] when is_integer(TupleLine) ->
            astranaut_traverse_monad:bind(
              quote_tuple_list(TupleList, Opts),
              fun(TupleList1) ->
                      astranaut_traverse_monad:return({tuple, TupleLine, TupleList1})
              end);
        _ ->
            astranaut_traverse_monad:bind(
              quote_tuple_list_1(TupleList, Opts),
              fun(TupleList1) ->
                      astranaut_traverse_monad:return({tuple, Line, TupleList1})
              end)
    end.

quote_tuple_list([MA, Spec], #{attribute := spec} = Opts) ->
    %% special form of {attribute, Line, spec, {{F, A}, Spec}}.
    %% there is no line in {F, A}.
    NOpts = maps:remove(attribute, Opts),
    astranaut_traverse_monad:bind(
      quote_m(MA, Opts#{attribute => attr}),
      fun(MA1) ->
              astranaut_traverse_monad:bind(
                quote_m(Spec, NOpts),
                fun(Spec1) ->
                        astranaut_traverse_monad:return([MA1, Spec1])
                end)
      end);
quote_tuple_list(TupleList, #{attribute := attr} = Opts) ->
    %% special form of {attribute, Line, export, [{F, A}...]}.
    %% special form of {attribute, Line, Attribute, T}.
    %% there is no line in {F, A} and T.
    quote_tuple_list_1(TupleList, Opts);
quote_tuple_list([Action, TupleLine|Rest] = TupleList, Opts) ->
    NOpts = update_attribute_opt(TupleList, Opts),
    astranaut_traverse_monad:bind(
      quote_tuple_list_1(Rest, NOpts),
      fun(Rest1) ->
              Action1 = quote_1(Action, Opts),
              Line1 = quote_line(Opts#{quote_line => TupleLine}),
              astranaut_traverse_monad:return([Action1, Line1|Rest1])
      end).

quote_tuple_list_1(List, Opts) ->
    astranaut_traverse_monad:map_m(
      fun(Item) ->
              quote_m(Item, Opts)
      end, List).

quote_line(#{quote_line := Line, quote_type := pattern}) ->
    {var, Line, '_'};
quote_line(#{quote_line := Line, quote_type := expression, code_line := true} = Opts) ->
    quote_1(Line, Opts);
quote_line(#{quote_type := expression} = Opts) ->
    quote_1(0, Opts).

unquote(Exp, #{type := value}) ->
    Exp;
unquote(Exp, #{type := Type, quote_line := Line} = Opts) ->
    {tuple, Line, [quote_1(Type, Opts), quote_line(Opts), Exp]}.

unquote_splicing(Unquotes, Rest, #{quote_line := Line, quote_type := expression, join := list} = Opts) ->
    astranaut_traverse_monad:bind(
      quote_m(Rest, Opts),
      fun(Rest1) ->
              astranaut_traverse_monad:return(
                call_remote(?MODULE, uncons, [Unquotes, Rest1], Line))
      end);
unquote_splicing(Unquotes, Rest, #{quote_line := Line, quote_type := expression, join := cons} = Opts) ->
    astranaut_traverse_monad:bind(
      quote_m(Rest, Opts),
      fun(Rest1) ->
              astranaut_traverse_monad:return(
                call_remote(?MODULE, cons, [Unquotes, Rest1], Line))
      end);
unquote_splicing(Unquotes, [], #{quote_type := pattern, join := list}) ->
    Unquotes;
unquote_splicing(Unquotes, {nil, _}, #{quote_type := pattern, join := cons}) ->
    Unquotes;
unquote_splicing(Unquotes, Rest, #{quote_type := pattern}) ->
    {warning, Unquotes, {non_empty_tail_unquote_splicing_pattern, Rest}}.

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

to_options(Ast) ->
    Type = erl_syntax:type(Ast),
    case lists:member(Type, [list, nil, map_expr, atom]) of
        true ->
            Line = erl_syntax:get_pos(Ast),
            {Options, Warnings} = astranaut:ast_to_options(Ast),
            validate_options(Options, Line, Warnings);
        false ->
            #{line => Ast}
    end.

validate_options(Line, _QuoteLine, Warnings) when is_integer(Line) ->
    {#{code_line => Line}, Warnings};
validate_options(Options, Line, Warnings) ->
    {Options1, Warnings1} = 
        astranaut:validate_options(
          fun validate_quote_option/2,
          Options),
    Warnings2 = 
        lists:map(
          fun(Warning) ->
                  {Line, astranaut_quote, Warning}
          end, Warnings1),
    Options1#{warnings => Warnings ++ Warnings2}.

validate_quote_option(debug, Boolean) when is_boolean(Boolean) ->
    ok;
validate_quote_option(code_line, Boolean) when is_boolean(Boolean) ->
    ok;
validate_quote_option(line, _) ->
    ok;
validate_quote_option(_Key, _Value) ->
    error.

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
            {ok, NCodes, {nil, 0}};
        {_NCodes, _NonCodes} ->
            {error, invalid_quote_code}
    end.

call_remote(Module, Function, Arguments, Line) ->
    {call, Line, {remote, Line, {atom, Line, Module}, {atom, Line, Function}}, Arguments}.

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
