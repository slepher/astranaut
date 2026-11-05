%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2019, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 20 Sep 2019 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_rebinding).

-include("do.hrl").

%% API
-export([parse_transform/2, format_error/1]).

-record(rebinding_options, {global_options, fun_options}).
%%%===================================================================
%%% API
%%%===================================================================
-spec parse_transform(astranaut:forms(), [compile:option()]) ->
    astranaut:forms().
parse_transform(Forms, _Options) ->
    Return =
        do([
            return
         || RebindingOptions <- load_attributes(Forms),
            astranaut_traverse:eval(
                astranaut_traverse:map_m(
                    fun(Form) ->
                        astranaut:traverse_form(
                            fun(Node) -> walk_form(Node, RebindingOptions) end, Form
                        )
                    end,
                    Forms
                ),
                ?MODULE,
                #{},
                #{}
            )
        ]),
    astranaut_return:to_compiler(Return).

-spec format_error(term()) -> term().
format_error({invalid_rebinding_fun, Function}) ->
    io_lib:format("invalid rebinding function: ~p", [Function]);
format_error({validate_key_failure, _Failure, _Key, _Value} = GenericError) ->
    astranaut:format_error(GenericError);
format_error({invalid_option_value, _Value} = GenericError) ->
    astranaut:format_error(GenericError).
%%%===================================================================
%%% load options
%%%===================================================================
load_attributes(Forms) ->
    do([
        return
     || GlobalOptions <-
            astranaut_lib:validate_attribute_option(
                rebinding_validator(), ?MODULE, rebinding_all, Forms
            ),
        FunOptions <-
            astranaut_lib:with_attribute(
                fun(Attr, Acc) ->
                    add_fun_options(Attr, Acc)
                end,
                #{},
                Forms,
                rebinding_fun,
                #{formatter => ?MODULE, simplify_return => false}
            ),
        return(#rebinding_options{fun_options = FunOptions, global_options = GlobalOptions})
    ]).

add_fun_options(FName, Acc) when is_atom(FName) ->
    add_fun_options(FName, #{}, Acc);
add_fun_options({FName, Arity}, Acc) when is_atom(FName), is_integer(Arity) ->
    add_fun_options({FName, Arity}, #{}, Acc);
add_fun_options({Function, Options}, Acc) ->
    do([
        return
     || Options1 <- astranaut_lib:validate(rebinding_validator(), Options),
        add_fun_options(Function, Options1, Acc)
    ]);
add_fun_options(Function, Acc) ->
    add_fun_options(Function, #{}, Acc).

add_fun_options(Functions, Options, Acc) when is_list(Functions) ->
    astranaut_return:foldl_m(
        fun(Function, Acc1) ->
            add_fun_options(Function, Options, Acc1)
        end,
        Acc,
        Functions
    );
add_fun_options({FName, Arity}, Options, Acc) when is_atom(FName), is_integer(Arity) ->
    merge_fun_options({FName, Arity}, Options, Acc);
add_fun_options(FName, Options, Acc) when is_atom(FName) ->
    merge_fun_options(FName, Options, Acc);
add_fun_options(Other, _Options, Acc) ->
    astranaut_return:warning_ok({invalid_rebinding_fun, Other}, Acc).

merge_fun_options(Function, Options, Acc) ->
    FAcc = maps:get(Function, Acc, #{}),
    astranaut_return:return(maps:put(Function, maps:merge(FAcc, Options), Acc)).

rebinding_validator() ->
    #{
        clause_pinned => boolean,
        strict => boolean,
        debug => boolean,
        rebinding => [paired, {default, true}]
    }.

%%%===================================================================
%%% walk form
%%%===================================================================
walk_form({function, Pos, Name, Arity, Clauses} = Function, RebindingOptionsRec) ->
    case match_rebinding(Name, Arity, RebindingOptionsRec) of
        {ok, RebindingOptions} ->
            ClausesM =
                astranaut_traverse:map_m(
                    fun(Clause) ->
                        walk_function_clause(Clause, RebindingOptions)
                    end,
                    Clauses
                ),
            astranaut_traverse:lift_m(
                fun(Clauses1) ->
                    Function1 = {function, Pos, Name, Arity, Clauses1},
                    case maps:get(debug, RebindingOptions, false) of
                        true ->
                            io:format("~s~n", [astranaut_lib:ast_safe_to_string(Function1)]);
                        false ->
                            ok
                    end,
                    Function1
                end,
                ClausesM
            );
        error ->
            astranaut_traverse:return(Function)
    end;
walk_form(Form, _RebindingOptionsRec) ->
    astranaut_traverse:return(Form).

walk_function_clause(Clause, Options) ->
    %% The rebinding dialect historically defaults to sequential arguments.
    %% Translate its strict switch here; vars defaults to standard isolation.
    ArgumentScope =
        case maps:get(strict, Options, false) of
            true -> isolated;
            false -> sequential
        end,
    astranaut_vars:map_m(
        fun(Node, Scope) -> walk_clause_node(Node, Scope, Options) end,
        Clause,
        #{argument_scope => ArgumentScope, collect => []}
    ).

walk_clause_node({var, _, '_'}, _, _) ->
    keep;
walk_clause_node(
    {op, _, '+', {var, _, _} = Var},
    #{role := pattern, pattern_kind := Kind},
    _
) when Kind =:= match_left; Kind =:= clause_match ->
    {use, Var};
walk_clause_node(
    {var, _, _} = Var,
    #{role := pattern, pattern_kind := clause_match, is_bound := true},
    #{clause_pinned := true}
) ->
    {use, Var};
walk_clause_node(
    {var, _, Name},
    #{role := pattern, same_pattern := false, allocated := Allocated},
    _
) ->
    {bind, astranaut_vars:fresh_var(Name, Allocated, #{sep => "_"})};
walk_clause_node(_, _, _) ->
    keep.

match_rebinding(Name, Arity, RebindingOptionsRec) ->
    RebindingOptions = find_rebinding_options(Name, Arity, RebindingOptionsRec),
    case maps:get(rebinding, RebindingOptions, true) of
        false ->
            error;
        true ->
            {ok, RebindingOptions}
    end.

find_rebinding_options(Name, Arity, #rebinding_options{
    fun_options = FunOptions, global_options = GlobalOptions
}) ->
    case maps:find({Name, Arity}, FunOptions) of
        {ok, Options} ->
            Options;
        error ->
            case maps:find(Name, FunOptions) of
                {ok, Options} ->
                    Options;
                error ->
                    GlobalOptions
            end
    end.
