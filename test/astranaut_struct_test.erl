%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2020, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 15 Jun 2020 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_struct_test).

-include("struct.hrl").

%% API
-export([new/0]).
-export([update_name/2]).
-export([test/1]).
-export([from_record/1, to_record/1, from_map/1, update/1]).

-include("test_record.hrl").

-type test() :: #test{}.
-type test_with_name(Name) :: #test{ name :: Name }.

%%%===================================================================
%%% API
%%%===================================================================
-spec new() -> test().
new() ->
    #test{}.

-spec update_name(#test{}, atom()) -> #test{}.
update_name(#test{} = Test, Name) ->
    Test#test{name = Name}.

test(#test{name = #test{name = A} = B}) ->
    {A, B};
test(#test{name = A}) ->
    A;
test(#test2{'name' = A}) ->
    A.

-spec from_record(astranaut_struct:record(#test{})) -> #test{}.
from_record(Record) ->
    astranaut_struct:from_record(test, Record).

-spec to_record(#test{}) -> astranaut_struct:record(#test{}).
to_record(Struct) ->
    astranaut_struct:to_record(test, Struct).

-spec update(#test{}) -> #test{}.
update(Struct) ->
    astranaut_struct:update(test, Struct).

-spec from_map(#{}) -> #test{}.
from_map(Map) ->
    astranaut_struct:from_map(test, Map).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
