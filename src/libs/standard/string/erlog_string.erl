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
  Sublist = lists:sublist(Str, From, To - From + 1),
  case erlog_ec_support:try_add(Sublist, Res, Bs0) of
    error -> erlog_errors:fail(Params);
    Bs -> erlog_ec_core:prove_body(Params#param{goal = Next, bindings = Bs})
  end;
prove_goal(Params = #param{goal = {indexof, _, _, _} = Goal, next_goal = Next, bindings = Bs0}) ->
  {indexof, Str1, Str2, Res} = erlog_ec_support:dderef(Goal, Bs0),
  case string:str(Str1, Str2) of
    0 -> erlog_errors:fail(Params);
    Num ->
      Bs1 = erlog_ec_support:add_binding(Res, Num, Bs0),
      erlog_ec_core:prove_body(Params#param{goal = Next, bindings = Bs1})
  end;
prove_goal(Params = #param{goal = {parse_int, _, _} = Goal, next_goal = Next, bindings = Bs0}) ->
  {parse_int, Str, Int} = erlog_ec_support:dderef(Goal, Bs0),
  case string:to_integer(Str) of
    {error, _} ->
      erlog_errors:fail(Params);
    {Integer, _} ->
      case erlog_ec_support:try_add(Integer, Int, Bs0) of
        error -> erlog_errors:fail(Params);
        Bs -> erlog_ec_core:prove_body(Params#param{goal = Next, bindings = Bs})
      end
  end;
prove_goal(Params = #param{goal = {parse_float, _, _} = Goal, next_goal = Next, bindings = Bs0}) ->
  {parse_float, Str, Res} = erlog_ec_support:dderef(Goal, Bs0),
  case string:to_float(Str) of
    {error, _} ->
      erlog_errors:fail(Params);
    {Float, _} ->
      case erlog_ec_support:try_add(Float, Res, Bs0) of
        error -> erlog_errors:fail(Params);
        Bs -> erlog_ec_core:prove_body(Params#param{goal = Next, bindings = Bs})
      end
  end;
prove_goal(Params = #param{goal = {str_number, _, _} = Goal, bindings = Bs0}) ->
  {str_number, Str, Num} = erlog_ec_support:dderef(Goal, Bs0),
  case erlog_ec_support:is_bound(Str) of
    true ->
      convert_string(Str, Num, Params); %string not bound,
    false ->
      convert_numeric(Str, Num, Params)
  end.


%% @private
preprocess_concat(Object, Acc) when is_list(Object); is_tuple(Object) ->
  case io_lib:printable_list(Object) of
    true -> [Object | Acc];
    false -> [lists:flatten(io_lib:format("~p", [Object])) | Acc]
  end;
preprocess_concat(Object, Acc) -> [Object | Acc].

%% @private
convert_string(Str, Num, Params) ->
  case string:to_float(Str) of
    {Float, _} ->
      match_num(Float, Num, Params);
    error ->
      case string:to_integer(Str) of
        {Integer, _} ->
          match_num(Integer, Num, Params);
        error ->
          erlog_errors:fail(Params)
      end
  end.

%% @private
convert_numeric(Str, Num, Params = #param{next_goal = Next, bindings = Bs0}) ->
  case erlog_ec_support:is_bound(Num) of
    true when is_number(Num) ->
      S = lists:flatten(io_lib:format("~p", [Num])),
      Bs1 = erlog_ec_support:add_binding(Str, S, Bs0),
      erlog_ec_core:prove_body(Params#param{goal = Next, bindings = Bs1});
    false -> erlog_errors:fail(Params)
  end.

%% @private
match_num(Number, ResultVar, Params = #param{next_goal = Next, bindings = Bs0}) ->
  case erlog_ec_support:try_add(Number, ResultVar, Bs0) of
    error -> erlog_errors:fail(Params);
    Bs -> erlog_ec_core:prove_body(Params#param{goal = Next, bindings = Bs})
  end.