%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Авг. 2014 18:01
%%%-------------------------------------------------------------------
-module(erlog_el_logic).
-author("tihon").

-include("erlog_core.hrl").

%% API
-export([fail_append/5,
  fail_insert/5,
  fail_member/4,
  memberchk/2,
  insert/1,
  reverse/2]).

insert(Params = #param{goal = {insert, A1, A2, A3}, next_goal = Next, bindings = Bs, choice = Cps, var_num = Vn}) ->
  FailFun = fun(LCp, LCps, LDb) ->  %TODO db not needed
    erlog_el_logic:fail_insert(LCp, Params#param{choice = LCps, database = LDb}, A1, A2, A3)
  end,
  Cp = #cp{type = compiled, data = FailFun, next = Next, bs = Bs, vn = Vn},
  erlog_ec_body:unify_prove_body(A3, [A2 | A1], Params#param{choice = [Cp | Cps]}).

fail_append(#cp{next = Next0, bs = Bs0, vn = Vn}, Params, A1, L, A3) ->
  H = {Vn},
  T = {Vn + 1},
  L1 = {Vn + 2},
  Bs1 = erlog_ec_support:add_binding(A1, [H | T], Bs0),    %A1 always a variable here.
  Next1 = [{append, T, L, L1} | Next0],
  erlog_ec_body:unify_prove_body(A3, [H | L1], Params#param{next_goal = Next1, bindings = Bs1,
    var_num = Vn + 3}).

fail_insert(#cp{next = Next0, bs = Bs, vn = Vn}, Params, A1, X, A3) ->
  H = {Vn},
  L = {Vn + 1},
  L1 = {Vn + 2},
  Next1 = [{insert, L, X, L1} | Next0],
  erlog_ec_body:unify_prove_body(A1, [H | L], A3, [H | L1], Params#param{next_goal = Next1, bindings = Bs, var_num = Vn + 3}).

fail_member(#cp{next = Next0, bs = Bs, vn = Vn}, Params, A1, A2) ->
  H = {Vn},
  T = {Vn + 1},
  Next1 = [{member, A1, T} | Next0],
  erlog_ec_body:unify_prove_body(A2, [H | T], Params#param{next_goal = Next1, bindings = Bs, var_num = Vn + 2}).

%% memberchk_2(Head, NextGoal, Choicepoints, Bindings, VarNum, Database) -> void.
%% memberchk(X, [X|_]) :- !.
%% memberchk(X, [_|T]) :- member(X, T).
%%  We don't build the list and we never backtrack so we can be smart
%%  and match directly. Should we give a type error?
memberchk({memberchk, A1, A2}, Params = #param{next_goal = Next, bindings = Bs0}) ->
  case erlog_ec_support:deref(A2, Bs0) of
    [H | T] ->
      case erlog_ec_unify:unify(A1, H, Bs0) of
        {succeed, Bs1} ->
          erlog_ec_core:prove_body(Params#param{goal = Next, bindings = Bs1});
        fail ->
          memberchk({memberchk, A1, T}, Params)
      end;
    {_} -> erlog_errors:instantiation_error();
    _ -> erlog_errors:fail(Params)
  end.

%% reverse_2(Head, NextGoal, Choicepoints, Bindings, VarNum, Database) -> void.
%% reverse([], []).
%% reverse([H|L1], L) :- reverse(L1, L2), append(L2, [H], L).
%%  Here we attempt to compile indexing in the first argument.
reverse({reverse, A1, A2}, Params = #param{next_goal = Next0, bindings = Bs0, choice = Cps, var_num = Vn}) ->
  case erlog_ec_support:deref(A1, Bs0) of
    [] ->
      erlog_ec_body:unify_prove_body(A2, [], Params);
    [H | T] ->
      L = {Vn},
      L1 = A2,
      %% Naive straight expansion of body.
      %%Next1 = [{reverse,T,L},{append,L,[H],L1}|Next0],
      %%prove_body(Next1, Cps, Bs0, Vn+1, Db);
      %% Smarter direct calling of local function.
      Next1 = [{append, L, [H], L1} | Next0],
      reverse({reverse, T, L}, Params#param{next_goal = Next1, var_num = Vn + 1});
    {_} = Var ->
      FailFun = fun(LCp, LCps, LDb) ->  %TODO db not needed
        fail_reverse(LCp, Params#param{choice = LCps, database = LDb}, Var, A2)
      end,
      Cp = #cp{type = compiled, data = FailFun, next = Next0, bs = Bs0, vn = Vn},
      Bs1 = erlog_ec_support:add_binding(Var, [], Bs0),
      erlog_ec_body:unify_prove_body(A2, [], Params#param{choice = [Cp | Cps], bindings = Bs1});
    _ -> erlog_errors:fail(Params)      %Will fail here!
  end.

%% @private
fail_reverse(#cp{next = Next, bs = Bs0, vn = Vn}, Params, A1, A2) ->
  H = {Vn},
  T = {Vn + 1},
  L1 = A2,
  L = {Vn + 2},
  Bs1 = erlog_ec_support:add_binding(A1, [H | T], Bs0),
  %%Next1 = [{reverse,T,L},{apperse,L,[H],L1}|Next],
  %%prove_body(Next1, Cps, Bs1, Vn+3, Db).
  Next1 = [{append, L, [H], L1} | Next],
  reverse({reverse, T, L}, Params#param{next_goal = Next1, bindings = Bs1, var_num = Vn + 3}).