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
      R = (catch erlog_ec_core:prove_goal(Params#param{goal = Goal, next_goal = []})),
      reply(Parent, R, erlog_ec_support:get_vars(Goal, Bs0)),
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
prove_goal(Params = #param{goal = {check, _, _} = G, next_goal = Next, bindings = Bs0}) ->
  {check, Pid, Result} = erlog_ec_support:dderef(G, Bs0),
  case recv_res(Pid, 0) of
    error -> erlog_errors:fail(Params);
    empty -> Bs1 = erlog_ec_support:add_binding(Result, false, Bs0),
      erlog_ec_core:prove_body(Params#param{goal = Next, bindings = Bs1});
    VarsGot ->
      UBs = lists:foldl(fun({Var, Value}, UpdBs) -> erlog_ec_support:add_binding(Var, Value, UpdBs) end, Bs0, VarsGot),
      erlog_ec_core:prove_body(Params#param{goal = Next, bindings = UBs})
  end.


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

%% @private
reply(Parent, {succeed, _, Bs1, _, _}, StartVars) -> Parent ! {self(), extract_vars(StartVars, Bs1)};
reply(Parent, _, _) -> Parent ! {self(), error}.

%% @private
extract_vars(VarList, Bs) ->
  lists:foldl(
    fun({Var}, Acc) ->
      case dict:find(Var, Bs) of
        {ok, Value} -> [{{Var}, Value} | Acc];
        error -> Acc
      end
    end, [], VarList).