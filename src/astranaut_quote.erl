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
-export([flattencons/1, flattencons/2, mergecons/2]).
-export([bind_var/2]).
-export([fix_user_type/1]).
-export([validate_pos/2]).
-export([quote_type_code/1, quoted/1, quoted/2]).
-export([parse_transform/2, format_error/1]).

%%%===================================================================
%%% API for quote flattencons/2, mergecons/2
%%%===================================================================
flattencons({cons, _pos, Head, Tail}) ->
    [Head|flattencons(Tail)];
flattencons({nil, _pos}) ->
    [];
flattencons(Value) when is_list(Value) ->
    Value.

flattencons(Cons, []) ->
    flattencons(Cons);
flattencons(Cons, Rest) ->
    flattencons(Cons) ++ Rest.

mergecons([H|T], Rest) ->
    {cons, 0, H, mergecons(T, Rest)};
mergecons([], Rest) ->
    Rest;
mergecons({cons, Pos, Head, Tail}, Rest) ->
    {cons, Pos, Head, mergecons(Tail, Rest)};
mergecons({nil, _pos}, Rest) ->
    Rest.
%%%===================================================================
%%% API for quote bind_var/2
%%%===================================================================
bind_var(Var, #{type := value}) ->
    Var;
bind_var(Var, #{type := atom_value} = Opts) ->
    var_value(Var, Opts);
bind_var(Var, #{type := dynamic, pos := Pos}) ->
    astranaut_lib:abstract_form(Var, Pos);
bind_var(Var, #{type := Type, pos := Pos} = Opts) ->
    {Type, Pos, var_value(Var, Opts)}.

var_value(Var, #{type := integer}) when is_integer(Var) ->
    Var;
var_value(Var, #{type := float}) when is_float(Var) ->
    Var;
var_value(Var, #{type := float}) when is_integer(Var) ->
    float(Var);
var_value(Var, #{type := string, name := Name}) when is_list(Var) ->
    case lists:all(fun is_integer/1, Var) of
        true ->
            Var;
        false ->
            exit({unexpected_type_of_var, Name, string, Var})
    end;
var_value(Var, #{type := string}) when is_binary(Var) ->
    unicode:characters_to_list(Var);
var_value(Var, #{type := atom} = Opts) ->
    atom_value(Var, Opts);
var_value(Var, #{type := var} = Opts) ->
    atom_value(Var, Opts);
var_value(Var, #{type := atom_value} = Opts) ->
    atom_value(Var, Opts).

atom_value(Var, #{}) when is_atom(Var) ->
    Var;
atom_value(Var, #{}) when is_list(Var) ->
    list_to_atom(Var);
atom_value(Var, #{}) when is_list(Var) ->
    binary_to_atom(Var, utf8);
atom_value(Var, #{type := Type, name := Name}) ->
    erlang:error({unexpected_type_of_var, Name, Type, Var}).
%%%===================================================================
%%% API for quote fix_user_type/1
%%%===================================================================
fix_user_type({user_type, Pos, map, []}) ->
    {type, Pos, map, any};
fix_user_type({user_type, Pos, tuple, []}) ->
    {type, Pos, tuple, any};
fix_user_type({user_type, Pos, Type, Args}) ->
    ParamsLen = length(Args),
    case erl_internal:is_type(Type, ParamsLen) of
        true ->
            {type, Pos, Type, Args};
        false ->
            {user_type, Pos, Type, Args}
    end.
%%%===================================================================
%%% API for quote validate_pos/1
%%%===================================================================
validate_pos(Quoted, Pos) ->
    case astranaut_syntax:is_pos(Pos) of
        true ->
            Quoted;
        false ->
            erlang:error({invalid_pos_value, Pos})
    end.
%%%===================================================================
%%% API quote_type_code/2
%%%===================================================================
quote_type_code(Code) ->
    {attribute, 0, type, {dummy, Type, []}} = merl:quote(0, "-type dummy() :: " ++ Code ++ "."),
    Type.

%%%===================================================================
%%% API quoted/1, quoted/2
%%%===================================================================
quoted(Node) ->
    quoted(Node, #{}).

quoted(Node, #{} = Opts) ->
    QuotePos = erl_syntax:get_pos(Node),
    Opts1 = maps:merge(#{quote_type => expression}, Opts#{quote_pos => QuotePos}),
    Opts2 = maps:map(fun(pos, Pos) -> astranaut_lib:abstract_form(Pos, QuotePos); (_Key, Value) -> Value end, Opts1),
    Quoted = quote(Node, Opts2),
    {just, Return} = astranaut_return:run(Quoted),
    ErrorStruct = astranaut_return:run_error(Quoted),
    case maps:find(formatted_warnings, astranaut_error:printable(ErrorStruct)) of
        {ok, Warnings} ->
            {warning, Return, Warnings};
        error ->
            Return
    end;
quoted(Node, Pos) ->
    quoted(Node, #{pos => Pos}).

%%%===================================================================
%%% parse_transform/2
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

debug_module(Forms1, true) ->
    io:format("~s~n", [astranaut_lib:ast_to_string(Forms1)]);
debug_module(_Forms1, _) ->
    ok.
%%%===================================================================
%%% transform walk function
%%%===================================================================
walk({call, _Pos1, {atom, _Pos2, quote}, [Form]} = Node, Attr, WalkOpts) ->
    %% transform quote(Code)
    quote(Form, #{}, Node, Attr, WalkOpts);
walk({call, _Pos1, {atom, _Pos2, quote}, [Form, Options]} = Node, Attr, WalkOpts) ->
    %% transform quote(Code, Options)
    astranaut_traverse:bind(
      astranaut_traverse:astranaut_traverse(to_options(Options)),
      fun(Options1) ->
              quote(Form, Options1, Node, Attr, WalkOpts)
      end);
walk({call, Pos1, {atom, _Pos2, quote_code}, Codes} = Node, #{node := NodeType} = Attr, WalkOpts) ->
    %% transform quote_code("Code1", "Code2",...)
    %% transform quote_code("Code1", "Code2",..., Options)
    case split_codes(Codes, NodeType) of
        {ok, NCodes, Options} ->
            astranaut_traverse:bind(
              astranaut_traverse:astranaut_traverse(to_options(Options)),
              fun(Options1) ->
                      Form = merl:quote(Pos1, NCodes),
                      quote(Form, Options1, Node, Attr, WalkOpts)
              end);
        {error, invalid_quote_code} ->
            astranaut_traverse:fail({invalid_quote, Node})
    end;
walk({call, Pos1, {atom, _pos2, quote_type_code}, Codes} = Node, #{node := NodeType} = Attr, WalkOpts) ->
    %% transform quote_code("Code1", "Code2",...)
    %% transform quote_code("Code1", "Code2",..., Options)
    case split_codes(Codes, NodeType) of
        {ok, [CodeH|CodesT], Options} ->
            astranaut_traverse:bind(
              astranaut_traverse:astranaut_traverse(to_options(Options)),
              fun(Options1) ->
                      {attribute, _Pos, type, {dummy, Type, []}} =
                          merl:quote(Pos1, ["-type dummy() :: " ++ CodeH| CodesT] ++ ["."]),
                      quote(Type, Options1, Node, Attr, WalkOpts)
              end);
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

%%%===================================================================
%%% get options from quoted ast
%%%===================================================================
to_options(Ast) ->
    Type = erl_syntax:type(Ast),
    case lists:member(Type, [list, nil, map_expr, atom]) of
        true ->
            Pos = astranaut_syntax:get_pos(Ast),
            Options = ast_to_options(Ast),
            validate_options(Options, Pos);
        false ->
            astranaut_return:return(#{pos => Ast})
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
    astranaut_return:return(#{pos => Pos});
validate_options(Options, Pos) ->
    Return = astranaut_lib:validate(#{debug => boolean, code_pos => boolean, pos => any}, Options),
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

quote(Value, Options, Node, Attr, #{file := File, module := Module, debug := Debug}) ->
    QuotePos = erl_syntax:get_pos(Node),
    QuoteType = quote_type(Attr),
    Options1 = maps:merge(#{quote_pos => QuotePos, quote_type => QuoteType, 
                            file => File, module => Module, debug => Debug}, Options),
    astranaut_traverse:set_updated(astranaut_traverse:astranaut_traverse(quote(Value, Options1))).

quote(Node, #{debug := true} = Opts) ->
    Opts1 = maps:remove(debug, Opts),
    astranaut_return:lift_m(
      fun(QuotedAst) ->
              format_quoted_ast(QuotedAst, Opts1),
              QuotedAst
      end, quote(Node, Opts1));
quote(Node, #{pos := Pos, quote_pos := QuotePos} = Options) ->
    astranaut_return:lift_m(
      fun(QuotedAst) ->
              call_remote(?MODULE, validate_pos, [QuotedAst, Pos], QuotePos)
      end, quote_1(Node, Options));
quote(Node, #{} = Opts) ->
    quote_1(Node, Opts).

format_quoted_ast(QuotedAst, #{} = Opts) ->
    QuotedCode = astranaut_lib:ast_safe_to_string(QuotedAst),
    io:format("~ts~s~n", [format_file(Opts), QuotedCode]).

format_file(#{quote_pos := QuotePos, file := File}) ->
    RelaPath = astranaut_lib:relative_path(File),
    io_lib:format("~ts:~s~n", [RelaPath, format_quote_pos(QuotePos)]);
format_file(#{}) ->
    "".

format_quote_pos({Line, Col}) ->
    io_lib:format("~p,~p", [Line, Col]);
format_quote_pos(Line) ->
    io_lib:format("~p", [Line]).

%% unquote
quote_1({call, _Pos1, {atom, _Pos2, unquote}, [Unquote]}, _Opts) ->
    %% quote({ok, unquote(Unquote)})
    unquote(Unquote);
quote_1({match, _Pos1, {atom, _Pos2, unquote}, Unquote}, _Opts) ->
    %% quote(fun(unquote = Unquote) -> Unquote end) in pattern
    unquote(Unquote);
quote_1({map_field_assoc, _Pos1, {atom, _Pos2, unquote}, Unquote}, _Opts) ->
    %% quote(#{unquote => Unquote})
    unquote(Unquote);
quote_1({record_field, _Pos1, {atom, _Pos2, unquote}, Unquote}, _Opts) ->
    %% quote(#Rec{unquote = Unquote})
    unquote(Unquote);
%% unquote_splicing
quote_1({cons, _Pos1, {call, _Pos2, {atom, _Pos3, unquote_splicing}, [Unquotes]}, T}, Opts) ->
    %% quote([a, b, unquote_splicing(V), c, d]),
    unquote_splicing(Unquotes, T, Opts#{join => cons});

%% invalid place of unquote_splicing
quote_1({call, Pos1, {atom, _Pos2, unquote_splicing}, [Unquote]}, _Opts) ->
    astranaut_return:then(
      astranaut_return:formatted_warning(Pos1, ?MODULE, {invalid_unquote_splicing, Unquote}),
      unquote(Unquote));

%% unquote_splicing variables
quote_1({cons, Pos1, {var, _Pos2, _VarName} = Var, T}, Opts) ->
    %% [A, _L@Unquotes, B] expression.
    case parse_binding_var(Var) of
        {value_list, Unquotes} ->
            unquote_splicing(Unquotes, T, Opts#{join => cons});
        Binding ->
            Opts1 = Opts#{quote_pos => Pos1},
            astranaut_return:lift_m(
              fun(T1) ->
                      tuple([quote_literal_value(cons, Opts1),
                             quote_pos(Opts1),
                             quote_variable(Binding, Opts1),
                             T1], Opts1)
              end, quote_1(T, Opts1))
    end;

%% unquote variables
quote_1({var, Pos, VarName} = Var, #{} = Opts) when is_atom(VarName) ->
    case parse_binding_var(Var) of
        {value_list, {var, _, Name} = Unquote} ->
            astranaut_return:then(
              astranaut_return:formatted_warning(Pos, ?MODULE, {invalid_unquote_splicing_binding, Name}),
              astranaut_return:return(quote_literal_tuple(Unquote, Opts)));
        Binding ->
            astranaut_return:return(quote_variable(Binding, Opts#{quote_pos => Pos}))
    end;
quote_1({atom, Pos, Name} = Atom, #{attribute := type_body} = Opts) ->
    %% typename transform is type
    %% -type '_A@Name'() :: '_A@Type'().
    Opts1 = Opts#{quote_pos => Pos},
    case parse_binding_name(Name, Pos) of
        {value, Unquote, _} ->
            astranaut_return:return(Unquote);
        {atom, Unquote, _} ->
            astranaut_return:return(unquote_binding(Unquote, Opts1#{type => atom}));
        {_Type, Var, VarName} ->
            astranaut_return:then(
              astranaut_return:formatted_warning(Pos, ?MODULE, {only_bindings_suported, ["A", ""], VarName, Name}),
              astranaut_return:return(Var));
        default ->
            astranaut_return:return(quote_literal_tuple(Atom, Opts))
      end;
quote_1({match, _Pos1, Pattern, {var, _Pos2, _} = Var}, #{quote_pos := Pos, quote_type := pattern} = Opts) ->
    % _A@World = World2 => {atom, _, World} = World2 in pattern
    astranaut_return:lift_m(
      fun(Pattern1) ->
            {match, Pos, Pattern1, Var}
      end, quote_1(Pattern, Opts));

quote_1({user_type, Pos, Name, Params}, Opts) ->
    Opts1 = Opts#{quote_pos => Pos},
    astranaut_return:lift_m(
      fun([QuotedName, QuotedParams]) ->
              Quoted = tuple([quote_literal_value(user_type, Opts1),
                              quote_pos(Opts1),
                              QuotedName,
                              QuotedParams], Opts1),
              call_remote(?MODULE, fix_user_type, [Quoted], Pos)
      end, astranaut_return:sequence_m([quote_type_name(Name, Opts1), quote_1(Params, Opts1)]));
%% special tuple expression dose not contains pos
quote_1({clauses, Clauses}, #{} = Opts) ->
    %% if tuple is the function clauses value, there is no pos.
    astranaut_return:lift_m(
      fun(QuotedClauses) ->
              tuple([quote_literal_value(clauses, Opts), QuotedClauses], Opts)
      end, quote_1(Clauses, Opts));

quote_1({function, Name, Arity}, #{} = Opts) ->
    %% fun Name/Arity.
    astranaut_return:lift_m(
      fun(QuotedName) ->
              tuple([quote_literal_value(function, Opts),
                     QuotedName,
                     quote_literal_value(Arity, Opts)], Opts)
      end, quote_atom_literal_name(Name, Opts));

%% Name in named_fun is literal atom, but should treated as variable.
quote_1({named_fun, Pos1, Name, Clauses}, #{} = Opts) ->
    %% fun Name/Arity.
    Opts1 = Opts#{quote_pos => Pos1},
    astranaut_return:lift_m(
      fun([QuotedName, QuotedClauses]) ->
              tuple([quote_literal_value(named_fun, Opts1),
                     quote_pos(Opts1),
                     QuotedName,
                     QuotedClauses], Opts)
      end, astranaut_return:sequence_m([quote_var_literal_name(Name, Opts), quote_list(Clauses, Opts1)]));

%% quote values
quote_1({LiteralType, _Pos, _Literal} = Tuple, Opts) 
  when LiteralType == atom ;
       LiteralType == integer ;
       LiteralType == char ;
       LiteralType == float ;
       LiteralType == string ->
    astranaut_return:return(quote_literal_tuple(Tuple, Opts));
quote_1(Tuple, Opts) when is_tuple(Tuple) ->
    quote_tuple_list(tuple_to_list(Tuple), Opts);
quote_1(List, Opts) when is_list(List) ->
    quote_list(List, Opts);
quote_1(Name, Opts) when is_atom(Name) ->
    quote_atom_literal_name(Name, Opts);
quote_1(Value, Opts) ->
    astranaut_return:return(quote_literal_value(Value, Opts)).

quote_variable({default, Var}, Opts) ->
    rename_variable(Var, Opts);
quote_variable({BindingType, Unquote}, #{} = Opts) ->
    unquote_binding(Unquote, Opts#{type => BindingType}).

rename_variable({var, Pos, VarName}, Opts) ->
    Var = {var, Pos, rename_variable_name(VarName, Opts)},
    quote_literal_tuple(Var, Opts).

rename_variable_name('_', #{}) ->
    '_';
rename_variable_name(VarName, #{module := Module}) ->
    list_to_atom(atom_to_list(VarName) ++ "@" ++ atom_to_list(Module));
rename_variable_name(VarName, #{}) ->
    VarName.

quote_list([{call, _Pos1, {atom, _Pos2, unquote_splicing}, [Unquotes]}|T], Opts) ->
    %% quote({a, b, unquote_splicing(V), c, d}),
    unquote_splicing(Unquotes, T, Opts#{join => list});
quote_list([{match, _, {atom, _, unquote_splicing}, Unquotes}|T], Opts) ->
    %% unquote_splicing = Unquotes in pattern
    unquote_splicing(Unquotes, T, Opts#{join => list});
quote_list([{map_field_assoc, _, {atom, _, unquote_splicing}, Unquotes}|T], Opts) ->
    %% quote(#{a => 1, b => 2, unquote_splicing => V, c => 3, d => 4}),
    unquote_splicing(Unquotes, T, Opts#{join => list});
quote_list([{record_field, _, {atom, _, unquote_splicing}, Unquotes}|T], Opts) ->
    %% quote(#record{a = 1, b = 2, unquote_splicing = V, c = 3, d = 4}),
    unquote_splicing(Unquotes, T, Opts#{join => list});
%% unquote variables
quote_list([{var, Pos, VarName} = Var|T], Opts) when is_atom(VarName) ->
    %% any L@Unquotes in list in absformat, like
    %% {A, _L@Unquotes, B} expression.
    %% fun(A, _L@Unquotes, B) -> _L@Unquotes end.
    Opts1 = Opts#{quote_pos => Pos},
    case parse_binding_var(Var) of
        {value_list, Unquotes} ->
            unquote_splicing(Unquotes, T, Opts1#{join => list});
        Binding ->
            quote_list_1(quote_variable(Binding, Opts1), T, Opts)
    end;
quote_list([H|T], #{} = Opts) ->
    astranaut_return:bind(
      quote_1(H, Opts),
      fun(H1) ->
              quote_list_1(H1, T, Opts)
      end);
quote_list([], #{quote_pos := Pos}) ->
    astranaut_return:return({nil, Pos}).

quote_list_1(H, T, #{quote_pos := Pos} = Opts) ->
    astranaut_return:lift_m(
      fun(T1) ->
              {cons, Pos, H, T1}
      end, quote_list(T, Opts)).

quote_tuple_list([Type|Rest], #{attribute := type_header} = Opts) ->
    %% special form of {attribute, Pos, spec, {{F, A}, Spec}}.
    %% special form of {attribute, Pos, type, {Name, Params, Type}}.
    %% there is no line in {F, A}.
    Opts1 = Opts#{attribute => type_body},
    astranaut_return:lift_m(
      fun([QuotedType, QuotedRest]) ->
              tuple([QuotedType|QuotedRest], Opts)
      end, astranaut_return:sequence_m([quote_type_name(Type, Opts), quote_tuple_list_rest(Rest, Opts1)]));
quote_tuple_list(TupleList, #{attribute := attr} = Opts) ->
    %% special form of {attribute, Pos, export, [{F, A}...]}.
    %% special form of {attribute, Pos, Attribute, T}.
    %% there is no line in {F, A} and T.
    quoted_tuple(quote_tuple_list_rest(TupleList, Opts), Opts);

quote_tuple_list([Action, TuplePos|Rest] = TupleList, #{} = Opts) ->
    case astranaut_syntax:is_pos(TuplePos) of
        true ->
            Opts1 = Opts#{quote_pos => TuplePos},
            RestOpts = update_attribute_opt(TupleList, Opts1),
            astranaut_return:lift_m(
              fun(QuotedRest) ->
                      tuple([quote_literal_value(Action, Opts1), quote_pos(Opts1)|QuotedRest], Opts1)
              end, quote_tuple_list_rest(Rest, RestOpts));
        false ->
            astranaut_return:then(
              astranaut_return:warning({could_not_get_tuple_pos_value, list_to_tuple(TupleList)}),
              quoted_tuple(quote_tuple_list_rest(TupleList, Opts), Opts))
    end.

quote_type_name(Name, #{} = Opts) when is_atom(Name) ->
    quote_atom_literal_name(Name, Opts);
quote_type_name({Name, Arity}, #{} = Opts) when is_atom(Name), is_integer(Arity) ->
      astranaut_return:lift_m(
        fun(QuotedName) ->
                tuple([QuotedName, quote_literal_value(Arity, Opts)], Opts)
        end, quote_atom_literal_name(Name, Opts)).

quote_atom_literal_name(Name, Opts) ->
    quote_literal_name(Name, Opts#{type => atom}).

quote_var_literal_name(Name, Opts) ->
    quote_literal_name(Name, Opts#{type => var}).

quote_literal_name(Name, #{quote_pos := Pos, type := Type} = Opts) when is_atom(Name) ->
    case parse_binding_name(Name, Pos) of
        { Type, Var, _VarName} ->
            astranaut_return:return(unquote_binding(Var, Opts#{type => atom_value}));
        {_Type, Var,  VarName} ->
            astranaut_return:then(
              astranaut_return:warning({only_bindings_supported, supported_bindings(Type), VarName, Name}),
              astranaut_return:return(unquote_binding(Var, Opts#{type => atom_value})));
        default ->
            case Type of
                var ->
                    astranaut_return:return(quote_literal_value(rename_variable_name(Name, Opts), Opts));
                atom ->
                    astranaut_return:return(quote_literal_value(Name, Opts))
            end
    end.

supported_bindings(atom) ->
    ["A"];
supported_bindings(var) ->
    ["V"].

update_attribute_opt([attribute, _Pos, spec|_T], Opts) ->
    Opts#{attribute => type_header};
update_attribute_opt([attribute, _Pos, type|_T], Opts) ->
    Opts#{attribute => type_header};
update_attribute_opt([attribute, _Pos, record|_T], Opts) ->
    Opts#{attribute => type_header};
update_attribute_opt([attribute|_T], Opts) ->
    Opts#{attribute => attr};
update_attribute_opt(_, Opts) ->
    Opts.

quoted_tuple(QuotedTupleListM, #{} = Opts) ->
    astranaut_return:lift_m(
      fun(QuotedTupleList) ->
              tuple(QuotedTupleList, Opts)
      end, QuotedTupleListM).

tuple(QuotedTupleList, #{quote_pos := Pos}) ->
    {tuple, Pos, QuotedTupleList}.

quote_tuple_list_rest(List, Opts) ->
    astranaut_return:map_m(
      fun(Item) ->
              quote_1(Item, Opts)
      end, List).

quote_literal_tuple({LiteralType, Pos, LiteralValue}, #{} = Opts) ->
    Opts1 = Opts#{quote_pos => Pos},
    tuple([quote_literal_value(LiteralType, Opts1),
           quote_pos(Opts1),
           quote_literal_value(LiteralValue, Opts1)], Opts1).

quote_literal_value(Atom, #{quote_pos := Pos}) when is_atom(Atom) ->
    {atom, Pos, Atom};
quote_literal_value(Integer, #{quote_pos := Pos}) when is_integer(Integer) ->
    {integer, Pos, Integer};
quote_literal_value(Float, #{quote_pos := Pos}) when is_float(Float) ->
    {float, Pos, Float};
quote_literal_value([], #{quote_pos := Pos}) ->
    {nil, Pos};
quote_literal_value(String, #{quote_pos := Pos}) when is_list(String) ->
    {string, Pos, String}.

%% quote_pos not return monad
quote_pos(#{pos := Pos}) ->
    Pos;
quote_pos(#{quote_pos := Pos, quote_type := pattern}) ->
    {var, Pos, '_'};
quote_pos(#{quote_type := expression} = Opts) ->
    quote_literal_value(quote_pos_value(Opts), Opts).

quote_pos_value(#{quote_pos := Pos, code_pos := true}) ->
    Pos;
quote_pos_value(#{}) ->
    0.

%% unquote return monad
unquote(Exp) ->
    astranaut_return:return(Exp).

unquote_binding(Exp, #{type := value}) ->
    Exp;
unquote_binding(Exp, #{type := dynamic, quote_type := pattern, quote_pos := Pos} = Opts) ->
    tuple([{var, Pos, '_'}, quote_pos(Opts), Exp], Opts);
unquote_binding(Exp, #{type := Type, quote_type := pattern} = Opts) ->
    tuple([quote_literal_value(Type, Opts), quote_pos(Opts), Exp], Opts);
unquote_binding({var, _, Varname} = Exp, #{quote_pos := Pos, type := Type} = Opts) ->
    call_bind_var(Exp, #{name => Varname, pos => quote_pos_value(Opts), type => Type}, Pos).

unquote_splicing(Unquotes, Rest, #{quote_pos := Pos, quote_type := expression, join := list} = Opts) ->
    astranaut_return:bind(
      quote_1(Rest, Opts),
      fun(Rest1) ->
              astranaut_return:return(
                call_remote(?MODULE, flattencons, [Unquotes, Rest1], Pos))
      end);
unquote_splicing(Unquotes, Rest, #{quote_pos := Pos, quote_type := expression, join := cons} = Opts) ->
    astranaut_return:bind(
      quote_1(Rest, Opts),
      fun(Rest1) ->
              astranaut_return:return(
                call_remote(?MODULE, mergecons, [Unquotes, Rest1], Pos))
      end);

unquote_splicing(Unquotes, [], #{quote_type := pattern, join := list}) ->
    astranaut_return:return(Unquotes);
unquote_splicing(Unquotes, {nil, _}, #{quote_type := pattern, join := cons}) ->
    astranaut_return:return(Unquotes);
unquote_splicing(Unquotes, Rest, #{quote_type := pattern, quote_pos := Pos}) ->
    Warning = {unquote_splicing_pattern_non_empty_tail, Rest},
    astranaut_return:then(
      astranaut_return:formatted_warning(Pos, ?MODULE, Warning),
      astranaut_return:return(Unquotes)).

quote_type(#{node := pattern}) ->
    pattern;
quote_type(_) ->
    expression.

call_remote(Module, Function, Arguments, Pos) ->
    {call, Pos, {remote, Pos, {atom, Pos, Module}, {atom, Pos, Function}}, Arguments}.

parse_binding_var({var, Pos, Varname} = Var) ->
    case parse_binding_1(atom_to_list(Varname)) of
        {VarType, VarNameStr} ->
            Varname1 = list_to_atom(VarNameStr),
            {VarType, {var, Pos, Varname1}};
        default ->
            {default, Var}
    end.

parse_binding_name(Name, Pos) ->
    case parse_binding_1(atom_to_list(Name)) of
        {VarType, VarName} ->
            VarName1 = list_to_atom(VarName),
            {VarType, {var, Pos, VarName1}, VarName1};
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
parse_binding_1([$_,$L,$@|T]) ->
    {value_list, T};
parse_binding_1([$_,$@|T]) ->
    {value, T};
parse_binding_1(_) ->
    default.

call_bind_var(Var, #{type := value}, _QuotePos) ->
    Var;
call_bind_var(Var, #{type := value_list}, _QuotePos) ->
    Var;
call_bind_var(Var, #{} = Opts, Pos) ->
    Opts1 = astranaut_lib:abstract_form(Opts, Pos),
    call_remote(?MODULE, bind_var, [Var, Opts1], Pos).

%%%===================================================================
%%% format_error/1
%%%===================================================================
format_error({invalid_unquote_splicing_binding, VarName}) ->
    io_lib:format("_L@~s not works without list in abstract tree, "
                  " _@~s expected.", [VarName, VarName]);
format_error({invalid_unquote_splicing, Unquote}) ->
    UnquoteStr = astranaut_lib:ast_safe_to_string(Unquote),
    io_lib:format("unquote_splicing(~s) not works without list in abstract tree, "
                  "unquote(~s) expected", [UnquoteStr, UnquoteStr]);
format_error({only_bindings_supported, Bindings, VarName, Name}) ->
    BindingsStr = string:join(
                    lists:map(
                      fun(Binding) ->
                              io_lib:format("_~s@~s", [Binding, VarName])
                      end, Bindings), " or "),
    io_lib:format("~s expected, not ~s.", [BindingsStr, Name]);
format_error({unquote_splicing_pattern_non_empty_tail, Rest}) ->
    io_lib:format("non empty expression '~s' after unquote_splicing in pattern", [astranaut_lib:ast_safe_to_string(Rest)]);
format_error({invalid_quote, Node}) ->
    io_lib:format("invalid quote ~s", [astranaut_lib:ast_safe_to_string(Node)]);
format_error(Message) ->
    astranaut:format_error(Message).
