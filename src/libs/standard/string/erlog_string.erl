%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Окт. 2014 20:54
%%%-------------------------------------------------------------------
-module(erlog_string).
-author("tihon").

-include("erlog_string.hrl").
-include("erlog_core.hrl").

-behaviour(erlog_stdlib).

%% API
-export([load/1, prove_goal/1]).


load(DbState) ->
  lists:foldl(fun(Head, UDBState) ->
    erlog_memory:load_kernel_space(UDBState, ?MODULE, Head) end, DbState, ?ERLOG_STRING).

prove_goal(Params = #param{goal = {concat, Strings, Res}, next_goal = Next, bindings = Bs0}) ->
  case erlog_ec_support:dderef_list(Strings, Bs0) of
    List when is_list(List) ->
      ConcatMe = lists:foldr(fun preprocess_concat/2, [], List),
      Bs1 = erlog_ec_support:add_binding(Res, ConcatMe, Bs0),
      erlog_ec_core:prove_body(Params#param{goal = Next, bindings = Bs1});
    _ -> erlog_errors:fail(Params)
  end;
prove_goal(Params = #param{goal = {substring, _, _, _, _} = Goal, next_goal = Next, bindings = Bs0}) ->
  {substring, From, To, Str, Res} = erlog_ec_support:dderef(Goal, Bs0),
  Bs1 = erlog_ec_support:add_binding(Res, lists:sublist(Str, From, To - From), Bs0),
  erlog_ec_core:prove_body(Params#param{goal = Next, bindings = Bs1});
prove_goal(Params = #param{goal = {indexof, _, _, _} = Goal, next_goal = Next, bindings = Bs0}) ->
  {indexof, Str1, Str2, Res} = erlog_ec_support:dderef(Goal, Bs0),
  case string:str(Str1, Str2) of
    0 -> erlog_errors:fail(Params);
    Num ->
      Bs1 = erlog_ec_support:add_binding(Res, Num, Bs0),
      erlog_ec_core:prove_body(Params#param{goal = Next, bindings = Bs1})
  end;
prove_goal(Params = #param{goal = {split, _, _, _} = Goal, next_goal = Next, bindings = Bs0}) ->
  {split, Str, Del, Res} = erlog_ec_support:dderef(Goal, Bs0),
  List = string:tokens(Str, Del),
  Bs1 = erlog_ec_support:add_binding(Res, List, Bs0),
  erlog_ec_core:prove_body(Params#param{goal = Next, bindings = Bs1}).


%% @private
preprocess_concat(Object, Acc) when is_list(Object); is_tuple(Object) ->
  case io_lib:printable_list(Object) of
    true -> [Object | Acc];
    false -> [lists:flatten(io_lib:format("~p", [Object])) | Acc]
  end;
preprocess_concat(Object, Acc) -> [Object | Acc].