%% Copyright (c) 2013 Robert Virding
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

%% File    : erlog_lists.erl
%% Author  : Robert Virding
%% Purpose : Standard Erlog lists library.
%% 
%% This is a standard lists library for Erlog. Everything here is
%% pretty basic and common to most Prologs. We are experimenting here
%% and some predicates are compiled. We only get a small benefit when
%% only implementing indexing on the first argument.

-module(erlog_lists).

-include("erlog_core.hrl").
-include("erlog_lists.hrl").

-behaviour(erlog_stdlib).

%% Main interface functions.
-export([load/1]).
-export([prove_goal/1]).

load(DbState) ->
  lists:foldl(fun(Head, UDBState) -> erlog_memory:load_kernel_space(UDBState, ?MODULE, Head) end, DbState, ?ERLOG_LISTS).

prove_goal(Params = #param{goal = {length, ListVar, Len}, next_goal = Next, bindings = Bs0}) ->
  case erlog_ec_support:deref(ListVar, Bs0) of
    List when is_list(List) ->
      Bs1 = erlog_ec_support:add_binding(Len, length(List), Bs0),
      erlog_ec_core:prove_body(Params#param{goal = Next, bindings = Bs1});
    _ -> erlog_errors:fail(Params)
  end;
prove_goal(Params = #param{goal = {append, A1, L, A3}, next_goal = Next0, bindings = Bs0, choice = Cps, var_num = Vn}) ->
  case erlog_ec_support:deref(A1, Bs0) of
    [] ->          %Cannot backtrack
      erlog_ec_body:unify_prove_body(L, A3, Params);
    [H | T] ->        %Cannot backtrack
      L1 = {Vn},
      Next1 = [{append, T, L, L1} | Next0],
      erlog_ec_body:unify_prove_body(A3, [H | L1], Params#param{next_goal = Next1, var_num = Vn + 1});
    {_} = Var ->        %This can backtrack
      FailFun = fun(LCp, LCps, LDb) ->  %TODO db not needed
        erlog_el_logic:fail_append(LCp, Params#param{choice = LCps, database = LDb}, Var, L, A3)
      end,
      Cp = #cp{type = compiled, data = FailFun, next = Next0, bs = Bs0, vn = Vn},
      Bs1 = erlog_ec_support:add_binding(Var, [], Bs0),
      erlog_ec_body:unify_prove_body(L, A3, Params#param{choice = [Cp | Cps], bindings = Bs1});
    _ -> erlog_errors:fail(Params)      %Will fail here!
  end;
prove_goal(Params = #param{goal = {insert, _, _, _}}) ->
  erlog_el_logic:insert(Params);
prove_goal(Params = #param{goal = {delete, A, B, C}}) ->
  erlog_el_logic:insert(Params#param{goal = {insert, C, B, A}});
prove_goal(Params = #param{goal = {member, A1, A2}, next_goal = Next, bindings = Bs, choice = Cps, var_num = Vn}) ->
  FailFun = fun(LCp, LCps, LDb) ->
    erlog_el_logic:fail_member(LCp, Params#param{choice = LCps, database = LDb}, A1, A2)
  end,
  Cp = #cp{type = compiled, data = FailFun, next = Next, bs = Bs, vn = Vn},
  T = {Vn},
  erlog_ec_body:unify_prove_body(A2, [A1 | T], Params#param{choice = [Cp | Cps], var_num = Vn + 1});
prove_goal(Params = #param{goal = {memberchk, A1, A2}}) ->
  erlog_el_logic:memberchk({memberchk, A1, A2}, Params);
prove_goal(Params = #param{goal = {sort, L0, S}, bindings = Bs}) ->
  %% This may throw an erlog error, we don't catch it here.
  L1 = lists:usort(erlog_ec_support:dderef_list(L0, Bs)),
  erlog_ec_body:unify_prove_body(S, L1, Params);
%% reverse([], []).
%% reverse([H|L1], L) :- reverse(L1, L2), append(L2, [H], L).
%%  Here we attempt to compile indexing in the first argument.
prove_goal(Params = #param{goal = {reverse, A1, A2}}) ->
  erlog_el_logic:reverse({reverse, A1, A2}, Params).