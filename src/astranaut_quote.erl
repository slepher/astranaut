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
-export([flattencons/2, mergecons/2]).

%%%===================================================================
%%% API
%%%===================================================================
parse_transform(Forms, _Options) ->
    File = astranaut_lib:analyze_forms_file(Forms),
    Module = astranaut_lib:analyze_forms_module(Forms),
    astranaut_return:to_compiler(
      astranaut_return:bind(
        astranaut_lib:validate_attribute_option(quote_validator(), ?MODULE, quote_options, Forms),
        fun(#{debug := Debug, debug_module := DebugModule}) ->
                WalkOpts = #{file => File, module => Module, debug => Debug},
                astranaut_return:lift_m(
                  fun(Forms1) ->
                          debug_module(Forms1, DebugModule),
                          Forms1
                  end,
                  astranaut_traverse:eval(
                    astranaut:map_m(
                      fun(Node) ->
                              astranaut_traverse:bind(
                                astranaut_traverse:ask(),
                                fun(Attr) ->
                                        walk(Node, Attr, WalkOpts)
                                end)
                      end, Forms, #{traverse => pre}),
                    ?MODULE, #{}, ok))
        end)).

quote_validator() ->
    #{debug => [boolean, {default, false}], debug_module => [boolean, {default, false}]}.

format_error({invalid_unquote_splicing, Binding, Var}) ->
    io_lib:format("expected unquote, not unquote_splicing ~s in ~s",
                  [astranaut_lib:ast_safe_to_string(Binding), astranaut_lib:ast_safe_to_string(Var)]);
format_error({only_bindings_supported, Bindings, VarName, Name}) ->
    BindingsStr = string:join(
                    lists:map(
                      fun(Binding) ->
                              io_lib:format("_~s@~s", [Binding, VarName])
                      end, Bindings), " or "),
    io_lib:format("expected _~s, not ~s", [BindingsStr, Name]);
format_error({dynamic_binding_in_expression, Expr}) ->
    io_lib:format("dynamic_binding _D@~s only avaliable in pattern", [Expr]);
format_error({unquote_splicing_pattern_non_empty_tail, Rest}) ->
    io_lib:format("non empty expression '~s' after unquote_splicing in pattern", [astranaut_lib:ast_safe_to_string(Rest)]);
format_error({invalid_quote, Node}) ->
    io_lib:format("invalid quote ~s", [astranaut_lib:ast_safe_to_string(Node)]);
format_error(Message) ->
    astranaut:format_error(Message).

debug_module(Forms1, true) ->
    io:format("~s~n", [astranaut_lib:ast_to_string(Forms1)]);
debug_module(_Forms1, _) ->
    ok.

flattencons(Cons, []) ->
    flattencons_1(Cons);
flattencons(Cons, Rest) ->
    flattencons_1(Cons) ++ Rest.

flattencons_1({cons, _pos, Head, Tail}) ->
    [Head|flattencons_1(Tail)];
flattencons_1({nil, _pos}) ->
    [];
flattencons_1(Value) ->
    Value.

mergecons([H|T], Rest) ->
    {cons, 0, H, mergecons(T, Rest)};
mergecons([], Rest) ->
    Rest;
mergecons({cons, Pos, Head, Tail}, Rest) ->
    {cons, Pos, Head, mergecons(Tail, Rest)};
mergecons({nil, _pos}, Rest) ->
    Rest.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
walk({call, _pos1, {atom, _pos2, quote}, [Form]} = Node, Attr, WalkOpts) ->
    %% transform quote(Code)
    quote(Form, #{}, Node, Attr, WalkOpts);
walk({call, _pos1, {atom, _pos2, quote}, [Form, Options]} = Node, Attr, WalkOpts) ->
    %% transform quote(Code, Options)
    astranaut_traverse:bind(
      astranaut_traverse:astranaut_traverse(to_options(Options)),
      fun(Options1) ->
              quote(Form, Options1, Node, Attr, WalkOpts)
      end);
walk({call, Pos1, {atom, _pos2, quote_code}, Codes} = Node, #{node := NodeType} = Attr, WalkOpts) ->
    %% transform quote_code("Code1", "Code2",...)
    %% transform quote_code("Code1", "Code2",..., Options)
    case split_codes(Codes, NodeType) of
        {ok, NCodes, Options} ->
            Options1 = to_options(Options),
            Form = merl:quote(Pos1, NCodes),
            quote(Form, Options1, Node, Attr, WalkOpts);
        {error, invalid_quote_code} ->
            astranaut_traverse:fail({invalid_quote, Node})
    end;
walk({match, _pos1, {atom, _pos2, quote}, Form} = Node, #{node := pattern} = Attr, WalkOpts) ->
    %% transform quote = Form in pattern match
    quote(Form, #{}, Node, Attr, WalkOpts);
walk({match, Pos1, {atom, _pos2, quote_code}, {string, _, Code}} = Node, #{node := pattern} = Attr, WalkOpts) ->
    %% transform quote_code = Code in pattern match
    Forms = merl:quote(Pos1, Code),
    quote(Forms, #{}, Node, Attr, WalkOpts);
walk(Node, Attr, _File) ->
    Type = erl_syntax:type(Node),
    astranaut_traverse:return(
      astranaut_uniplate:with_subtrees(
        fun(Subtrees) ->
                astranaut_syntax:subtrees_pge(Type, Subtrees, Attr)
        end)).

quote(Value, Options, Node, Attr, #{file := File, module := Module, debug := Debug}) ->
    QuotePos = erl_syntax:get_pos(Node),
    QuoteType = quote_type(Attr),
    Options1 = maps:merge(#{quote_pos => QuotePos, quote_type => QuoteType, 
                            file => File, module => Module, debug => Debug}, Options),
    astranaut_traverse:set_updated(quote(Value, Options1)).

quote(Node, #{debug := true} = Options) ->
    astranaut_traverse:lift_m(
      fun(QuotedAst) ->
              format_quoted_ast(QuotedAst, Options),
              QuotedAst
      end, quote(Node, Options#{debug => false}));
quote(Node, #{line := Pos, quote_pos := QuotePos} = Options) ->
    Options1 = maps:remove(line, Options),
    astranaut_traverse:lift_m(
      fun(Quoted) ->
              call_remote(astranaut_lib, replace_line_zero, [Quoted, Pos], QuotePos)
      end, quote(Node, Options1));
quote(Node, #{} = Options) ->
    quote_1(Node, Options).

%% unquote
quote_1({call, _pos1, {atom, _pos2, unquote}, [Unquote]}, _Opts) ->
    unquote(Unquote);
quote_1({match, _, {atom, _, unquote}, Unquote}, _Opts) ->
    %% quote = Unquote in pattern
    unquote(Unquote);
%% unquote_splicing
quote_1({cons, _pos1, {call, _pos2, {atom, _pos3, unquote_splicing}, [Unquotes]}, T}, Opts) ->
    %quote([a, b, unquote_splicing(V), c, d]),
    unquote_splicing(Unquotes, T, Opts#{join => cons});
quote_1([{call, _pos1, {atom, _pos2, unquote_splicing}, [Unquotes]}|T], Opts) ->
    %quote({a, b, unquote_splicing(V), c, d}),
    unquote_splicing(Unquotes, T, Opts#{join => list});
quote_1([{match, _, {atom, _, unquote_splicing}, Unquotes}|T], Opts) ->
    %% unquote_splicing = Unquotes in pattern
    unquote_splicing(Unquotes, T, Opts#{join => list});
%% unquote variables
quote_1({match, _pos1, Pattern, Value}, #{quote_pos := Pos, quote_type := pattern} = Opts) ->
    % _A@World = World2 => {atom, _, World} = World2
    astranaut_traverse:bind(
      quote_1(Pattern, Opts),
      fun(Pattern1) ->
              astranaut_traverse:return({match, Pos, Pattern1, Value})
      end);
quote_1({cons, _pos1, {var, Pos, VarName}, T} = Tuple, Opts) when is_atom(VarName) ->
    %% [A, _L@Unquotes, B] expression.
    case parse_binding_var(VarName, Pos) of
        {value_list, Unquotes} ->
            unquote_splicing(Unquotes, T, Opts#{join => cons});
        default ->
            quote_tuple(Tuple, Opts)
    end;
quote_1([{var, Pos, VarName}|T] = List, Opts) when is_atom(VarName) ->
    %% any L@Unquotes in list in absformat, like
    %% {A, _L@Unquotes, B} expression.
    %% fun(A, _L@Unquotes, B) -> _L@Unquotes end.
    case parse_binding_var(VarName, Pos) of
        {value_list, Unquotes} ->
            unquote_splicing(Unquotes, T, Opts#{quote_pos => Pos, join => list});
        _ ->
            quote_list(List, Opts)
    end;
quote_1({var, _Pos, '_'} = Var, Opts) ->
    quote_tuple(Var, Opts);
quote_1({var, Pos, VarName} = Var, #{module := Module} = Opts) when is_atom(VarName) ->
    case parse_binding_var(VarName, Pos) of
        {value_list, Unquotes} ->
            astranaut_traverse:then(
                astranaut_traverse:update_pos(
                  Pos, astranaut_traverse:warning({invalid_unquote_splicing, Unquotes, Var})),
              quote_tuple(Var, Opts));
        {BindingType, Unquote} ->
            unquote_binding(Unquote, Opts#{type => BindingType, quote_pos => Pos});
        default ->
            VarName1 = list_to_atom(atom_to_list(VarName) ++ "@" ++ atom_to_list(Module)),
            Var1 = {var, Pos, VarName1},
            quote_tuple(Var1, Opts)
    end;
quote_1({atom, Pos, Name} = Atom, #{type := true} = Opts) ->
    %% typename transform is type
    %% -type '_A@Name'() :: '_A@Type'().
    case parse_binding_var(Name, Pos) of
        {value, Unquote} ->
            unquote_binding(Unquote, Opts#{type => value, quote_pos => Pos});
        {atom, Unquote} ->
            unquote_binding(Unquote, Opts#{type => atom, quote_pos => Pos});
        {_Type, {var, _, Varname} = Var} ->
            astranaut_traverse:then(
              astranaut_traverse:warning({only_bindings_suported, ["A", ""], Varname, Name}),
              astranaut_traverse:return(Var));
        defaut ->
            quote_tuple(Atom, Opts)
      end;
%% quote values
quote_1({LiteralType, Pos, Literal}, Opts) 
  when LiteralType == atom ;
       LiteralType == integer ;
       LiteralType == char ;
       LiteralType == float ;
       LiteralType == string ->
    astranaut_traverse:return(quote_literal(LiteralType, Literal, Opts#{quote_pos => Pos}));
quote_1(Tuple, Opts) when is_tuple(Tuple) ->
    quote_tuple(Tuple, Opts);
quote_1(List, Opts) when is_list(List) ->
    quote_list(List, Opts);
quote_1(Atom, Opts) when is_atom(Atom) ->
    astranaut_traverse:return(quote_literal_value(Atom, Opts));
quote_1(Integer, Opts) when is_integer(Integer) ->
    astranaut_traverse:return(quote_literal_value(Integer, Opts)).

quote_list([H|T], #{quote_pos := Pos} = Opts) ->
    astranaut_traverse:bind(
      quote_1(H, Opts),
      fun(H1) ->
              astranaut_traverse:bind(
                quote_1(T, Opts),
                fun(T1) ->
                        astranaut_traverse:return({cons, Pos, H1, T1})
                end)
      end);
quote_list([], #{quote_pos := Pos}) ->
    astranaut_traverse:return({nil, Pos}).

quote_tuple(Tuple, Opts) ->
    quote_tuple_list(tuple_to_list(Tuple), Opts).

quote_tuple_list([user_type, Pos, Name, Params], Opts) ->
    astranaut_traverse:bind(
        quote_type_name(Name, Opts),
        fun(QuotedName) ->
                astranaut_traverse:bind(
                  quote_1(Params, Opts),
                  fun(QuotedParams) ->
                          astranaut_traverse:return(
                            {tuple, Pos, [quote_type_style(Name, Opts), quote_pos(Opts), QuotedName, QuotedParams]})
                  end)
        end);
quote_tuple_list([attribute, Pos, type, {Name, Type, Params}], Opts) ->
    Opts1 = Opts#{type => true},
    astranaut_traverse:bind(
      astranaut_traverse:bind(
        quote_type_name(Name, Opts),
        fun(QuotedName) ->
                astranaut_traverse:bind(
                  quote_1(Type, Opts1),
                  fun(QuotedType) ->
                          astranaut_traverse:bind(
                            quote_1(Params, Opts1),
                            fun(QuotedParams) ->
                                    astranaut_traverse:return(
                                      {tuple, Pos, [QuotedName, QuotedType, QuotedParams]})
                            end)
                  end)
        end),
      fun(QuotedTypeBody) ->
              astranaut_traverse:return(
                {tuple, Pos, [quote_literal_value(attribute, Opts), quote_pos(Opts), quote_literal_value(type, Opts),
                              QuotedTypeBody]})
      end);

quote_tuple_list([Type|Rest], #{quote_pos := Pos, attribute := type} = Opts) ->
    %% special form of {attribute, Pos, spec, {{F, A}, Spec}}.
    %% special form of {attribute, Pos, type, {Name, Params, Type}}.
    %% there is no line in {F, A}.
    Opts1 = maps:remove(attribute, Opts),
    astranaut_traverse:bind(
      quote_tuple_list_rest(Rest, Opts1),
      fun(QuotedRest) ->
              astranaut_traverse:return({tuple, Pos, [quote_literal_value(Type, Opts)|QuotedRest]})
      end);
quote_tuple_list(TupleList, #{attribute := attr} = Opts) ->
    %% special form of {attribute, Pos, export, [{F, A}...]}.
    %% special form of {attribute, Pos, Attribute, T}.
    %% there is no line in {F, A} and T.
    quoted_tuple(quote_tuple_list_rest(TupleList, Opts), Opts);
quote_tuple_list([clauses, Clauses], #{quote_pos := Pos} = Opts) ->
    %% if tuple is the function clauses value, use the original line.
    astranaut_traverse:bind(
      quote_1(Clauses, Opts),
      fun(QuotedClauses) ->
              astranaut_traverse:return({tuple, Pos, [quote_literal_value(clauses, Opts), QuotedClauses]})
      end);
quote_tuple_list([Action, TuplePos|Rest] = TupleList, #{} = Opts) ->
    case astranaut_syntax:is_pos(TuplePos) of
        true ->
            Opts1 = update_attribute_opt(TupleList, Opts),
            astranaut_traverse:bind(
              quote_tuple_list_rest(Rest, Opts1),
              fun(QuotedRest) ->
                      QuotedAction = quote_literal_value(Action, Opts),
                      QuotedPos = quote_pos(Opts#{quote_pos => TuplePos}),
                      astranaut_traverse:return({tuple, TuplePos, [QuotedAction, QuotedPos|QuotedRest]})
              end);
        false ->
            astranaut_traverse:then(
              astranaut_traverse:warning({could_not_get_tuple_pos_value, list_to_tuple(TupleList)}),
              quoted_tuple(quote_tuple_list_rest(TupleList, Opts), Opts))
    end.

quote_type_style(Name, #{quote_pos := Pos} = Opts) ->
    case parse_binding_var(Name, Pos) of
        {type, _} ->
            quote_literal_value(type, Opts);
        _ ->
            quote_literal_value(user_type, Opts)
    end.

quote_type_name(Name, #{quote_pos := Pos} = Opts) ->
    case parse_binding_var(Name, Pos) of
        {atom, Var} ->
            astranaut_traverse:return(Var);
        {type, Var} ->
            astranaut_traverse:return(Var);
        {_Type, {var, _, VarName} = Var} ->
            astranaut_traverse:then(
              astranaut_traverse:warning({only_bindings_supported, ["", "T", "A"], VarName, Name}),
              astranaut_traverse:return(Var));
        default ->
            astranaut_traverse:return(quote_literal_value(Name, Opts))
    end.

update_attribute_opt([attribute, _Pos, spec|_T], Opts) ->
    Opts#{attribute => type};
update_attribute_opt([attribute, _Pos, type|_T], Opts) ->
    Opts#{attribute => type};
update_attribute_opt([attribute, _Pos, record|_T], Opts) ->
    Opts#{attribute => type};
update_attribute_opt([attribute|_T], Opts) ->
    Opts#{attribute => attr};
update_attribute_opt(_, Opts) ->
    Opts.

quoted_tuple(QuotedTupleListM, #{quote_pos := Pos}) ->
    astranaut_traverse:lift_m(
      fun(QuotedTupleList) ->
              {tuple, Pos, QuotedTupleList}
      end, QuotedTupleListM).

quote_tuple_list_rest(List, Opts) ->
    astranaut_traverse:map_m(
      fun(Item) ->
              quote_1(Item, Opts)
      end, List).

quote_literal(LiteralType, LiteralValue, #{quote_pos := Pos} = Opts) ->
    {tuple, Pos, [quote_literal_value(LiteralType, Opts), quote_pos(Opts), quote_literal_value(LiteralValue, Opts)]}.

quote_literal_value(Literal, #{quote_pos := Pos}) ->
    astranaut_lib:replace_line(astranaut_lib:abstract_form(Literal), Pos).

%% quote_pos not return monad
quote_pos(#{quote_pos := Pos, quote_type := pattern}) ->
    {var, Pos, '_'};
quote_pos(#{quote_pos := Pos, quote_type := expression, code_pos := true} = Opts) ->
    quote_literal_value(Pos, Opts);
quote_pos(#{quote_type := expression} = Opts) ->
    quote_literal_value(0, Opts).

%% unquote return monad
unquote(Exp) ->
    astranaut_traverse:return(Exp).

%% unquote return monad
unquote_binding(Exp, #{type := value}) ->
    astranaut_traverse:return(Exp);
unquote_binding(Exp, #{type := dynamic, quote_type := pattern, quote_pos := Pos} = Opts) ->
    astranaut_traverse:return({tuple, Pos, [{var, Pos, '_'}, quote_pos(Opts), Exp]});
unquote_binding({var, _, Varname} = Exp, #{type := dynamic, quote_type := expression}) ->
    astranaut_traverse:then(
      astranaut_traverse:error({dynamic_binding_in_expression, Varname}),
      astranaut_traverse:return(Exp));
unquote_binding(Exp, #{type := Type, quote_pos := Pos} = Opts) ->
    astranaut_traverse:return({tuple, Pos, [quote_literal_value(Type, Opts), quote_pos(Opts), Exp]}).

%% unquote_splicing return monad.
unquote_splicing(Unquotes, Rest, #{quote_pos := Pos, quote_type := expression, join := list} = Opts) ->
    astranaut_traverse:bind(
      quote_1(Rest, Opts),
      fun(Rest1) ->
              astranaut_traverse:return(
                call_remote(?MODULE, flattencons, [Unquotes, Rest1], Pos))
      end);
unquote_splicing(Unquotes, Rest, #{quote_pos := Pos, quote_type := expression, join := cons} = Opts) ->
    astranaut_traverse:bind(
      quote_1(Rest, Opts),
      fun(Rest1) ->
              astranaut_traverse:return(
                call_remote(?MODULE, mergecons, [Unquotes, Rest1], Pos))
      end);
unquote_splicing(Unquotes, [], #{quote_type := pattern, join := list}) ->
    astranaut_traverse:return(Unquotes);
unquote_splicing(Unquotes, {nil, _}, #{quote_type := pattern, join := cons}) ->
    astranaut_traverse:return(Unquotes);
unquote_splicing(Unquotes, Rest, #{quote_type := pattern, quote_pos := Pos}) ->
    Warning = {unquote_splicing_pattern_non_empty_tail, Rest},
    astranaut_traverse:then(
      astranaut_traverse:update_pos(
        Pos, astranaut_traverse:warning(Warning)),
      astranaut_traverse:return(Unquotes)).

quote_type(#{node := pattern}) ->
    pattern;
quote_type(_) ->
    expression.

to_options(Ast) ->
    Type = erl_syntax:type(Ast),
    case lists:member(Type, [list, nil, map_expr, atom]) of
        true ->
            Pos = astranaut_syntax:get_pos(Ast),
            Options = ast_to_options(Ast),
            validate_options(Options, Pos);
        false ->
            astranaut_return:return(#{line => Ast})
    end.

ast_to_options(AstOptions) ->
    StaticTypes = [atom, char, float, integer, nil, string, list, map_expr, map_field_assoc, tuple],
    AstOptions1 =
        astranaut:smap(
          fun(Node, _Attr) ->
                  Type = erl_syntax:type(Node),
                  case lists:member(Type, StaticTypes) of
                      true ->
                          Node;
                      false ->
                          astranaut_uniplate:skip(astranaut_lib:abstract_form(Node))
                  end
          end, AstOptions, #{traverse => pre}),
    erl_syntax:concrete(AstOptions1).

validate_options(Pos, _QuotePos) when is_integer(Pos) ->
    astranaut_return:return(#{line => Pos});
validate_options(Options, Pos) ->
    Return = astranaut_lib:validate(#{debug => boolean, code_pos => boolean, line => any}, Options),
    astranaut_return:with_error(
        fun(ErrorStruct) ->
            astranaut_error:update_pos(Pos, ?MODULE, ErrorStruct)
        end, Return).

%% check ast in quote_code valid.
split_codes(Codes, NodeType) ->
    case lists:partition(
           fun({string, _, _}) ->
                   true;
              (_) ->
                   false
           end, Codes) of
        {NCodes, [Pos]} ->
            case NodeType of
                expression ->
                    NCodes1 = lists:map(fun({string, _, StringCode}) -> StringCode end, NCodes),
                    {ok, NCodes1, Pos};
                _ ->
                    {error, invalid_quote_code}
            end;
        {NCodes, []} ->
            NCodes1 = lists:map(fun({string, _, StringCode}) -> StringCode end, NCodes),
            {ok, NCodes1, {nil, 0}};
        {_NCodes, _NonCodes} ->
            {error, invalid_quote_code}
    end.

call_remote(Module, Function, Arguments, Pos) ->
    {call, Pos, {remote, Pos, {atom, Pos, Module}, {atom, Pos, Function}}, Arguments}.

%% 
%% 
parse_binding_var(Atom, Pos) ->
    case parse_binding_name(Atom) of
        {VarType, VarName} ->
            {VarType, {var, Pos, VarName}};
        default ->
            default
    end.

parse_binding_name(Atom) ->
    case parse_binding_1(atom_to_list(Atom)) of
        {VarType, VarName} ->
            {VarType, list_to_atom(VarName)};
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
parse_binding_1([$_,$D,$@|T]) ->
    {dynamic, T};
parse_binding_1([$_,$T,$@|T]) ->
    {type, T};
parse_binding_1([$_,$L,$@|T]) ->
    {value_list, T};
parse_binding_1([$_,$@|T]) ->
    {value, T};
parse_binding_1(_) ->
    default.

format_quoted_ast(QuotedAst, #{quote_pos := QuotePos, file := File}) ->
    QuotedCode = astranaut_lib:ast_safe_to_string(QuotedAst),
    RelaPath = astranaut_lib:relative_path(File),
    io:format("~ts:~s~n~s~n", [RelaPath, format_quote_pos(QuotePos), QuotedCode]).

format_quote_pos({Line, Col}) ->
    io_lib:format("~p,~p", [Line, Col]);
format_quote_pos(Line) ->
    io_lib:format("~p", [Line]).
