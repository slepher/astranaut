%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2018, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  8 Dec 2018 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(macro_test).

-quote_options(debug).
-macro_options(debug_module).

-include_lib("astranaut/include/quote.hrl").
-include_lib("astranaut/include/macro.hrl").

-define(MACRO_MODULE, macro_example).

%% API
-export([test_ok/0, test_function/1]).
-export([test_unquote/0, test_binding/0]).
-export([test_unquote_splicing/0, test_unquote_splicing_mix/0]).
-export([test_match_pattern/0]).
-export([test_function_pattern_1/0, test_function_pattern_2/0]).
-export([test_case_pattern_1/0, test_case_pattern_2/0, test_case_pattern_3/0]).
-export([test_quote_code/0, test_quote_line_1/0, test_quote_line_2/0]).
-export([test_try_catch/0, test_case/0, test_function/0]).
-export([test_attributes/0]).
-export([test_group_args/0]).
-export([test_macro_with_vars/1]).
-export([test_macro_order/0]).

-import_macro(?MACRO_MODULE).

-use_macro({?MACRO_MODULE, macro_exported_function/2, [alias]}).
-use_macro({?MACRO_MODULE, quote_code/0, #{alias => macro_quote_code}}).
-use_macro({?MACRO_MODULE, macro_with_vars_1/1, [alias]}).
-use_macro({?MACRO_MODULE, macro_with_vars_2/1, [alias]}).

-local_macro([quote_ok/0]).

-exec_macro({macro_exported_function, [hello, world]}).

%%%===================================================================
%%% API
%%%===================================================================
-spec test_ok() -> ok.
test_ok() ->
    quote_ok().

test_function(World) ->
    hello(World).

test_unquote() ->
    macro_example:quote_unquote(ok).

test_binding() ->
    macro_example:quote_binding(ok).

test_unquote_splicing() ->
    macro_example:quote_unquote_splicing(foo, bar).

test_unquote_splicing_mix() ->
    F = macro_example:quote_unquote_splicing_mix(Foo, bar),
    A1 = F(foo, bar),
    A2 = F(foo, zaa),
    {A1, A2}.

test_match_pattern() ->
    macro_example:quote_match_pattern(hello(world, foo, bar)).

test_function_pattern_1() ->
    macro_example:quote_function_pattern({hello, world}).

test_function_pattern_2() ->
    macro_example:quote_function_pattern({foo, bar}).

test_case_pattern_1() ->
    F = fun(X) -> X end,
    macro_example:quote_case_pattern(F(10)).

test_case_pattern_2() ->
    macro_example:quote_case_pattern(hello:world(foo, bar)).

test_case_pattern_3() ->
    macro_example:quote_case_pattern(task).

test_quote_code() ->
    macro_quote_code().

test_fun() ->
    ok.

test_quote_line_1() ->
    macro_example:quote_line_1(ok).

test_quote_line_2() ->
    macro_example:quote_line_2(ok).

test_case() ->
    macro_example:macro_case(one_plus(), 2, 3).

test_try_catch() ->
    macro_example:macro_try_catch().

test_function() ->
    F = macro_example:macro_function([_, _], ok),
    F(hello, foo, bar, world).

test_attributes() ->
    macro_example:macro_with_attributes().

test_group_args() ->
    macro_example:macro_group_args(hello, world).

test_macro_with_vars(N) ->
    A1 = macro_with_vars_1(N),
    A2 = macro_with_vars_2(A1),
    A3 = macro_with_vars_2(N),
    A4 = macro_with_vars_1(A1),
    A1 + A2 + A3 + A4.

test_macro_order() ->
    Value1 = macro_example:macro_order_outer(macro_example:macro_order_outer(ok)),
    Value2 = macro_example:macro_order_inner(macro_example:macro_order_inner(ok)),
    {Value1, Value2}.

-merge_function([test_merged_function, ok_1]).

test_merged_function(ok_2)->
    test_merged_function(ok_1);
test_merged_function(ok_3)->
    test_merged_function_1(ok_3);
test_merged_function(ok_4)->
    test_merged_function(ok_4, ok_4);
test_merged_function(_A) ->
    ok_2.

test_merged_function_1(ok_3) ->
    ok_3.

test_merged_function(A, _B) ->
    A.

quote_ok() ->
    quote(ok).

one_plus() ->
    1 + 1.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
