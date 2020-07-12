%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2020, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 11 Jul 2020 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_options).

-include_lib("astranaut/include/astranaut_struct_name.hrl").

-type options() :: option() | [option()] | option_map().
-type option() :: atom() | {atom(), term()}.
-type option_map() :: #{atom() => term()}.
-type validators() :: validator() | [validator()].
-type validator() :: atom() | {atom(), term()} | validator_fun().
-type validator_fun() :: fun((term()) -> term()) | fun((term(), term()) -> term()).

%% API
-export([with_attribute/5]).
-export([forms_with_attribute/5]).
-export([options/1]).
-export([validate/3]).
-export([attr_walk_return/1]).
-export([by_validator/2]).

%%%===================================================================
%%% API
%%%===================================================================
with_attribute(Fun, Init, Forms, Attr, Opts) ->
    astranaut_traverse:reduce(
      fun({attribute, Line, Attr1, AttrValue}, Acc, #{}) when Attr1 == Attr ->
              values_apply_fun(Fun, AttrValue, Acc, #{line => Line});
         (_Node, Acc, #{}) ->
              Acc
      end, Init, Forms, Opts#{traverse => list}).

forms_with_attribute(Fun, Init, Forms, Attr, Opts) ->
    Fun1 = update_with_attribute_f(Fun),
    ReturnM = 
        astranaut_traverse:reduce(
          fun({attribute, Line, Attr1, AttrValue} = Node, {FormsAcc, Acc}, #{}) when Attr1 == Attr ->
                  %% it's too complex, try simplify it.
                  astranaut_traverse_m:bind(
                    astranaut_traverse_m:astranaut_traverse_m(
                      astranaut_base_m:bind(
                        values_apply_fun(Fun1, AttrValue, {[], Acc}, #{line => Line}),
                        fun({Nodes, Acc1}) ->
                                astranaut_base_m:return({Nodes ++ [Node|FormsAcc], Acc1})
                        end)),
                    fun(State) ->
                            astranaut_traverse_m:put(State)
                    end);
             (Node, {FormsAcc, Acc}, #{}) ->
                  {[Node|FormsAcc], Acc}
          end, {[], Init}, Forms, Opts#{traverse => list, simplify_return => false}),
    LiftFun = fun({Forms1, Final}) -> {lists:reverse(Forms1), Final} end,
    astranaut_traverse:transform_return_m(astranaut_monad:lift_m(LiftFun, ReturnM, astranaut_return_m), Opts).

-spec options(options()) -> astranaut_base_m:astranaut_base_m(option_map()).
options(Atom) when is_atom(Atom) ->
    options([Atom]);
options({Key, Value}) when is_atom(Key) ->
    options([{Key, Value}]);
options(OptionList) when is_list(OptionList) ->
    astranaut_monad:foldl_m(
      fun({Key, Value}, Acc) when is_atom(Key) ->
              maps:put(Key, Value, Acc);
         (Key, Acc) when is_atom(Key) ->
              maps:put(Key, true, Acc);
         (Value, Acc) ->
              {warning, Acc, {invalid_option_value, Value}}
      end, maps:new(), OptionList, astranaut_base_m);
options(Options) when is_map(Options) ->
    options(maps:to_list(Options));
options(Options) ->
    astranaut_base_m:to_monad({warning, #{}, {invalid_option_value, Options}}).

-spec validate(#{atom() => validators()}, options(), #{}) -> astranaut_base_m:astranaut_base_m(option_map()).
validate(Validator, ToValidate, Options) ->
    astranaut_base_m:bind(
      options(ToValidate),
      fun(ToValidate1) ->
              validate_1(Validator, ToValidate1, Options)
      end).

attr_walk_return(#{node := Node} = Map) ->
    Map1 = maps:remove(node, Map),
    attr_walk_return(Map1#{nodes => [Node]});
attr_walk_return(#{} = Map) ->
    Nodes = maps:get(nodes, Map, []),
    A = maps:get(return, Map, ok),
    astranaut_walk_return:new(Map#{return => {Nodes, A}});
attr_walk_return(Return) ->
    case astranaut_walk_return:to_map(Return) of
        {ok, Map} ->
            attr_walk_return(Map);
        error ->
            attr_walk_return(#{return => Return})
    end.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
values_apply_fun(Fun, AttrValues, Acc, Opts) when is_list(AttrValues) ->
    case maps:get(deep_attr, Opts, true) of
        true ->
            astranaut_monad:foldl_m(
              fun(AttrValue, Acc1) ->
                      value_apply_fun(Fun, AttrValue, Acc1, Opts)
              end, Acc, AttrValues, astranaut_traverse_m);
        false ->
            value_apply_fun(Fun, AttrValues, Acc, Opts)
    end;
values_apply_fun(Fun, AttrValue, Acc, Opts) ->
    value_apply_fun(Fun, AttrValue, Acc, Opts).

value_apply_fun(Fun, Value, Acc, _Opts) when is_function(Fun, 2) ->
    Fun(Value, Acc);
value_apply_fun(Fun, Value, Acc, Opts) when is_function(Fun, 3) ->
    Fun(Value, Acc, Opts).

update_with_attribute_f(Fun) ->
    fun(Value, {Nodes0, Acc0}, Opts) ->
            astranaut_base_m:bind(
              value_apply_fun(Fun, Value, Acc0, Opts),
              fun({Nodes1, Acc1}) ->
                      astranaut_base_m:return({Nodes0 ++ Nodes1, Acc1})
              end)
    end.              

validate_1(ValidatorMap, ToValidate, _Options) when is_map(ValidatorMap) ->
    astranaut_monad:foldl_m(
      fun({Key, Validator}, Acc) ->
              Value = maps:get(Key, ToValidate, undefined),
              case by_validator(Validator, Value) of
                  {ok, Value1} ->
                      maps:put(Key, Value1, Acc);
                  {error, Reason} ->
                      {error, Acc, {Reason, Key, Value}};
                  {warning, Reason} ->
                      {warning, Acc, {Reason, Key, Value}}
              end
      end, maps:new(), maps:to_list(ValidatorMap), astranaut_base_m).

by_validator(Validator, Value) ->
    Return = by_validator_1(Validator, Value),
    update_return(Value, Return, Validator).

by_validator_1(required, Value) ->
    astranaut_validator:required(Value);
by_validator_1({defaut, Default}, Value) ->
    astranaut_validator:defaut(Value, Default);
by_validator_1(_Validator, undefined = Value) ->
    {ok, Value};
by_validator_1(Validator, Value) when is_atom(Validator) ->
    astranaut_validator:Validator(Value);
by_validator_1({Validator, Args}, Value) when is_atom(Validator) ->
    astranaut_validator:Validator(Value, Args);
by_validator_1(Validator, Value) when is_function(Validator, 1) ->
    Validator(Value);
by_validator_1([Validator|T], Value) ->
    case by_validator(Validator, Value) of
        {ok, Value1} ->
            by_validator(T, Value1);
        {warning, Reason} ->
            {warning, Reason};
        {error, Reason} ->
            {error, Reason}
    end;
by_validator_1([], Value) ->
    {ok, Value}.

update_return(Value, ok, _Validator) -> 
    {ok, Value};
update_return(_Value, {ok, Value1}, _Validator) -> 
    {ok, Value1};
update_return(Value, true, _Validator) -> 
    {ok, Value};
update_return(_Value, false, _Validator) -> 
    {warning, invalid_value};
update_return(_Value, {warning, Reason}, _Validator) -> 
    {warning, Reason};
update_return(_Value, {error, Reason}, _Validator) -> 
    {error, Reason};
update_return(Value, Other, Validator) -> 
    exit({invalid_validator_return_for, Validator, Value, Other}).
