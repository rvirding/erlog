%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. Янв. 2015 20:06
%%%-------------------------------------------------------------------
-module(erlog_parallel).
-author("tihon").

-behaviour(erlog_stdlib).

-include("erlog_core.hrl").
-include("erlog_parallel.hrl").

%% API
-export([load/1, prove_goal/1]).

load(DbState) ->
  lists:foldl(fun(Proc, UDBState) ->
    erlog_memory:load_kernel_space(UDBState, ?MODULE, Proc) end, DbState, ?ERLOG_PARALLEL).

prove_goal(Params = #param{goal = {spawn, _, _} = G, next_goal = Next, bindings = Bs0}) ->
  {spawn, Goal, Res} = erlog_ec_support:dderef(G, Bs0),
  Parent = self(),
  Pid = spawn(
    fun() ->
      Parent ! {self(), (catch erlog_ec_core:prove_goal(Params#param{goal = Goal, next_goal = []}))},
      Parent ! {self(), finish}
    end),
  Bs1 = erlog_ec_support:add_binding(Res, Pid, Bs0),
  erlog_ec_core:prove_body(Params#param{goal = Next, bindings = Bs1});
prove_goal(Params = #param{goal = {join, _, _} = G, next_goal = Next, bindings = Bs0}) ->
  {join, Pid, Timeout} = erlog_ec_support:dderef(G, Bs0),
  case catch join(Pid, Timeout) of
    ok -> erlog_ec_core:prove_body(Params#param{goal = Next});
    _ -> erlog_errors:fail(Params)
  end;
prove_goal(Params = #param{goal = {check, _, _} = G, next_goal = Next, bindings = Bs0, var_num = Vn0}) ->
  {check, Pid, Result} = erlog_ec_support:dderef(G, Bs0),
  case recv_res(Pid, 0) of
    empty ->
      Bs1 = erlog_ec_support:add_binding(Result, false, Bs0),
      erlog_ec_core:prove_body(Params#param{goal = Next, bindings = Bs1});
    {succeed, _, Bs1, Vn1, _} ->
      Bs = erlog_ec_support:add_binding(Result, true, dict:merge(fun merge_dicts/3, Bs0, Bs1)),
      erlog_ec_core:prove_body(Params#param{goal = Next, bindings = Bs, var_num = Vn0 + Vn1});
    _ -> erlog_errors:fail(Params)
  end.


%% @private
merge_dicts(_, _, Value2) -> Value2.

%% @private
join_proc(Pid, TM) ->
  receive
    {Pid, finish} -> ok
  after TM -> throw(timeout)
  end.

%% @private
recv_res(Pid, TM) ->
  receive
    {Pid, Result} when Result /= finish -> Result
  after TM -> empty
  end.


%% @private
join(Pids, Timeout) when is_list(Pids) ->
  lists:foreach(fun(Pid) -> join_proc(Pid, Timeout) end, Pids);
join(Pid, Timeout) -> join([Pid], Timeout).