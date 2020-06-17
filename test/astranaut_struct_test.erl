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

%%%===================================================================
%%% API
%%%===================================================================
new() ->
    #test{}.

update_name(#test{} = Test, Name) ->    
    Test#test{name = Name}.

test(#test{name = #test{name = A} = B}) ->
    {A, B};
test(#test{name = A}) ->
    A;
test(#test2{'name' = A}) ->
    A.

from_record(Record) ->
    astranaut_struct:from_record(test, Record).

to_record(Struct) ->
    astranaut_struct:to_record(test, Struct).

update(Struct) ->
    astranaut_struct:update(test, Struct).

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
