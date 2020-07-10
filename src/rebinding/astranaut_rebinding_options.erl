%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2019, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 25 Sep 2019 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_rebinding_options).

%% API
-export([rebinding_options/1]).
-export([match_rebinding/3]).
-export([keys/0]).

-record(rebinding_options, {all_options, fun_options}).

%%%===================================================================
%%% API
%%%===================================================================
rebinding_options(Forms) ->
    BFunAttrs = astranaut:attributes_with_line(rebinding_fun, Forms),
    BAllAttrs = astranaut:attributes_with_line(rebinding_all, Forms),
    case {BFunAttrs, BAllAttrs} of
        {[], []} ->
            {#rebinding_options{all_options = #{}, fun_options = #{}}, []};
        _ ->
            {BFunOptions, Warnings2} = binding_fun_options(BFunAttrs),
            {BAllOptions, Warnings1} = binding_all_options(BAllAttrs),
            {#rebinding_options{fun_options = BFunOptions, all_options = BAllOptions}, Warnings1 ++ Warnings2}
    end.

match_rebinding(Name, Arity, RebindingOptionsRec) ->
    case find_rebinding_options(Name, Arity, RebindingOptionsRec) of
        {ok, RebindingOptions} ->
            case maps:get(non_rebinding, RebindingOptions, false) of
                true ->
                    error;
                false ->
                    {ok, RebindingOptions}
            end;
        error ->
            error
    end.

keys() ->
    [clause_pinned, strict].
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================

find_rebinding_options(Name, Arity, #rebinding_options{fun_options = FunOptions, all_options = AllOptions}) ->
    case maps:find(Name, FunOptions) of
        {ok, Options} ->
            {ok, Options};
        error ->
            case maps:find({Name, Arity}, FunOptions) of
                {ok, Options} ->
                    {ok, Options};
                error ->
                    case AllOptions of
                        none ->
                            error;
                        #{} ->
                            {ok, AllOptions}
                    end
            end
    end.

binding_all_options([]) ->
    {none, []};
binding_all_options([{Line, Options}|_T]) ->
    {Options1, Warnings1} = astranaut:validate_options(fun validate_option_key/2, Options),
    Warnings2 = 
        lists:map(
          fun(Warning) ->
                  {Line, astranaut_macro, Warning}
          end, Warnings1),
    {Options1, Warnings2}.

binding_fun_options(BindingAttributes) ->
    lists:foldl(
      fun({Line, FunName}, Acc) when is_atom(FunName) ->
              add_binding_options(FunName, #{}, Line, Acc);
         ({Line, {FunName, Arity}}, Acc) when is_atom(FunName), is_integer(Arity) ->
              add_binding_options({FunName, Arity}, #{}, Line, Acc);
         ({Line, {FunName, Opts}}, Acc) ->
              add_binding_options(FunName, Opts, Line, Acc)
      end, {maps:new(), []}, BindingAttributes).

add_binding_options(Funs, Options, Line, {Acc, Warnings}) when is_list(Funs) ->
    lists:foldl(
      fun(Fun, {Acc1, Warnings1}) ->
              add_binding_options(Fun, Options, Line, {Acc1, Warnings1})
      end, {Acc, Warnings}, Funs);

add_binding_options(Fun, Options, Line, {Acc, Warnings}) ->
    {Options1, Warnings1} = astranaut:validate_options(fun validate_option_key/2, Options),
    Warnings2 = 
        lists:map(
          fun(Warning) ->
                  {Line, astranaut_rebinding, Warning}
          end, Warnings1),
    {maps:put(Fun, Options1, Acc), Warnings ++ Warnings2}.

validate_option_key(debug, true) ->
    ok;
validate_option_key(debug, false) ->
    ok;
validate_option_key(non_rebinding, true) ->
    ok;
validate_option_key(non_rebinding, false) ->
    ok;
validate_option_key(non_rebinding, _NoRebinding) ->
    error;
validate_option_key(clause_pinned, true) ->
    ok;
validate_option_key(clause_pinned, false) ->
    ok;
validate_option_key(clause_pinned, _ClausePinned) ->
    error;
validate_option_key(strict, true) ->
    ok;
validate_option_key(strict, false) ->
    ok;
validate_option_key(strict, _Strict) ->
    error;
validate_option_key(debug, _Debug) ->
    error;
validate_option_key(_Key, _Value) ->
    error.
