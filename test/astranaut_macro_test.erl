%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2018, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  8 Dec 2018 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_macro_test).

-define(DEBUG_OPT, [{debug, false}, {debug_ast, false}, formatter]).

-include("astranaut.hrl").

-define(MACRO_MODULE, astranaut_macro_example).

%% API
-export([test_ok/0, test_function/1]).
-export([test_unquote/0, test_binding/0]).
-export([test_unquote_splicing/0, test_unquote_splicing_mix_1/0, test_unquote_splicing_mix_2/0]).
-export([test_match_pattern/0]).
-export([test_function_pattern_1/0, test_function_pattern_2/0]).
-export([test_case_pattern_1/0, test_case_pattern_2/0, test_case_pattern_3/0]).
-export([test_quote_code/0]).
-export([test_try_catch/0, test_case/0, test_function/0]).
-export([test_attributes/0]).
-export([test_group_args/0]).

-export([quote_ok/0]).

-use_macro({quote_ok/0}).
-use_macro({?MACRO_MODULE, macro_exported_function/2, [auto_export, alias]}).
-use_macro({?MACRO_MODULE, quote_unquote/1, [alias]}).
-use_macro({?MACRO_MODULE, quote_binding/1, [alias]}).
-use_macro({?MACRO_MODULE, quote_unquote_splicing/2, [alias]}).
-use_macro({?MACRO_MODULE, quote_unquote_splicing_mix/2, [alias]}).
-use_macro({?MACRO_MODULE, quote_match_pattern/1, [alias]}).
-use_macro({?MACRO_MODULE, quote_match_pattern/1, [alias]}).
-use_macro({?MACRO_MODULE, quote_function_pattern/1, [alias]}).
-use_macro({?MACRO_MODULE, quote_case_pattern/1, [alias]}).
-use_macro({?MACRO_MODULE, quote_code/0, #{alias => macro_quote_code}}).
-use_macro({?MACRO_MODULE, macro_case/3, [alias]}).
-use_macro({?MACRO_MODULE, macro_try_catch/0, [alias]}).
-use_macro({?MACRO_MODULE, macro_function/2, [alias]}).
-use_macro({?MACRO_MODULE, macro_with_attributes/1, [alias, attrs]}).
-use_macro({?MACRO_MODULE, macro_group_args/1, [alias, group_args]}).

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
    quote_unquote(ok).

test_binding() ->
    quote_binding(ok).

test_unquote_splicing() ->
    quote_unquote_splicing(foo, bar).

test_unquote_splicing_mix_1() ->
    (quote_unquote_splicing_mix(foo, Bar))(foo, bar).

test_unquote_splicing_mix_2() ->
    (quote_unquote_splicing_mix(foo, Bar))(zaa, bar).

test_match_pattern() ->
    quote_match_pattern(hello(world, foo, bar)).

test_function_pattern_1() ->
    quote_function_pattern({hello, world}).

test_function_pattern_2() ->
    quote_function_pattern({foo, bar}).

test_case_pattern_1() ->
    F = fun(X) -> X end,
    quote_case_pattern(F(10)).

test_case_pattern_2() ->
    quote_case_pattern(hello:world(foo, bar)).

test_case_pattern_3() ->
    quote_case_pattern(task).

test_quote_code() ->
    macro_quote_code().

test_fun() ->
    ok.

test_case() ->
    macro_case(one_plus(), 2, 3).

test_try_catch() ->
    macro_try_catch().

test_function() ->
    F = macro_function([_, _], ok),
    F(hello, foo, bar, world).

test_attributes() ->
    macro_with_attributes().

test_group_args() ->
    macro_group_args(hello, world).

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