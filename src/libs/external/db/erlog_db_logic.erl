%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Нояб. 2014 1:59
%%%-------------------------------------------------------------------
-module(erlog_db_logic).
-author("tihon").

-include("erlog_core.hrl").

%% API
-export([check_call_result/5, prove_retract/3, prove_retractall/3, fail_retract/2]).

prove_retract({':-', H, B}, Table, Params) ->
  prove_retract(H, B, Table, Params);
prove_retract(H, Table, Params) ->
  prove_retract(H, true, Table, Params).

prove_retractall({':-', H, B}, Table, Params) ->
  prove_retractall(H, B, Table, Params);
prove_retractall(H, Table, Params) ->
  prove_retractall(H, true, Table, Params).

fail_retract(#cp{data = {Ch, Cb, {Db, Cursor}, Table}, next = Next, bs = Bs, vn = Vn}, Param) ->
  {UCursor, Res} = erlog_memory:db_next(Db, Cursor, Table),
  retract_clauses(Ch, Cb, Res, Param#param{next_goal = Next, bindings = Bs, var_num = Vn, cursor = UCursor}, Table).

check_call_result([], Param, _, _, _) -> erlog_errors:fail(Param);
check_call_result({clauses, Cs}, Param, G, Table, Next) -> prove_call(G, Cs, Next, Table, Param);
check_call_result({erlog_error, E}, #param{database = Db}, _, _, _) -> erlog_errors:erlog_error(E, Db);
check_call_result(Cs, Param, G, Table, Next) -> prove_call(G, Cs, Next, Table, Param).

%% prove_goal_clauses(Goal, Clauses, Next, ChoicePoints, Bindings, VarNum, Database) ->
%%      void.
%% Try to prove Goal using Clauses which all have the same functor.
prove_goal_clauses([], _, Params) ->  %end of checking clauses
  erlog_errors:fail(Params);
prove_goal_clauses([C], _, Params = #param{choice = Cps, var_num = Vn}) -> %for clauses with body
  %% Must be smart here and test whether we need to add a cut point.
  %% C has the structure {Tag,Head,{Body,BodyHasCut}}.
  case element(2, element(3, C)) of
    true ->
      Cut = #cut{label = Vn},
      erlog_ec_core:prove_goal_clause(C, Params#param{choice = [Cut | Cps]});
    false ->
      erlog_ec_core:prove_goal_clause(C, Params)
  end;
prove_goal_clauses(C, Table, Params = #param{goal = G, next_goal = Next, var_num = Vn, bindings = Bs, choice = Cps, database = Db, cursor = Cursor}) ->
  Cp = #cp{type = goal_clauses, label = Vn, data = {G, Db, Table, Cursor}, next = Next, bs = Bs, vn = Vn},
  erlog_ec_core:prove_goal_clause(C, Params#param{choice = [Cp | Cps]}).


%% @private
prove_call(G, Cs, Next0, Table, Param = #param{bindings = Bs, choice = Cps, database = Db, var_num = Vn}) ->
  case erlog_ec_logic:check_goal(G, Next0, Bs, Db, false, Vn) of
    {[Next1 | _], true} ->
      %% Must increment Vn to avoid clashes!!!
      Cut = #cut{label = Vn},
      prove_goal_clauses(Cs, Table, Param#param{goal = Next1, choice = [Cut | Cps], var_num = Vn + 1});
    {[Next1 | _], false} -> erlog_ec_core:prove_goal_clauses(Cs, Param#param{goal = Next1, var_num = Vn + 1})
  end.

%% @private
prove_retract(H, B, Table, Params = #param{database = Db}) ->
  case erlog_memory:get_db_procedure(Db, Table, H) of
    {cursor, Cursor, result, {clauses, Cs}} ->
      erlog_ec_core:run_n_close(fun(Param) ->
        retract_clauses(H, B, Cs, Param, Table) end, Params#param{cursor = Cursor});
    undefined -> erlog_errors:fail(Params);
    _ ->
      Functor = erlog_ec_support:functor(H),
      erlog_errors:permission_error(modify, static_procedure, erlog_ec_support:pred_ind(Functor))
  end.

%% @private
prove_retractall(H, B, Table, Params = #param{database = Db}) ->
  Functor = erlog_ec_support:functor(H),
  case erlog_memory:get_db_procedure(Db, Table, H) of
    {cursor, Cursor, result, Res} ->
      check_retractall_result(Res, H, B, Functor, Table, Params#param{cursor = Cursor});
    Res ->
      check_retractall_result(Res, H, B, Functor, Table, Params)
  end.

%% @private
retract(Ch, Cb, C, Cursor, Param = #param{next_goal = Next, choice = Cps, bindings = Bs0, var_num = Vn0, database = Db}, Bs1, Vn1, Table) ->
  erlog_memory:db_retract_clause(Db, Table, erlog_ec_support:functor(Ch), element(1, C)),
  Cp = #cp{type = db_retract, data = {Ch, Cb, {Db, Cursor}, Table}, next = Next, bs = Bs0, vn = Vn0},
  erlog_ec_core:prove_body(Param#param{goal = Next, choice = [Cp | Cps], bindings = Bs1, var_num = Vn1}).

%% @private
%% retract_clauses(Head, Body, Clauses, Next, ChoicePoints, Bindings, VarNum, Database) ->
%%      void.
%% Try to retract Head and Body using Clauses which all have the same functor.
retract_clauses(_, _, [], Param, _) -> erlog_errors:fail(Param);
retract_clauses(Ch, Cb, [C], Param, Table) -> retract_clauses(Ch, Cb, C, Param, Table);
retract_clauses(Ch, Cb, C, Param = #param{bindings = Bs0, var_num = Vn0, database = Db, cursor = Cursor}, Table) ->
  case erlog_ec_unify:unify_clause(Ch, Cb, C, Bs0, Vn0) of
    {succeed, Bs1, Vn1} ->
      %% We have found a right clause so now retract it.
      retract(Ch, Cb, C, Cursor, Param, Bs1, Vn1, Table);
    fail ->
      {UCursor, Res} = erlog_memory:db_next(Db, Cursor, Table),
      retract_clauses(Ch, Cb, Res, Param#param{cursor = UCursor}, Table)
  end.

retractall_clauses(_, [], _, _, Params = #param{next_goal = Next}) ->
  erlog_ec_core:prove_body(Params#param{goal = Next});
retractall_clauses(Table, [Clause], H, B, Params) -> retractall_clauses(Table, Clause, H, B, Params);
retractall_clauses(Table, Clause, H, B, Params = #param{bindings = Bs0, var_num = Vn0, database = Db, cursor = Cursor}) ->
  case erlog_ec_unify:unify_clause(H, B, Clause, Bs0, Vn0) of
    {succeed, _, _} ->
      erlog_memory:db_retract_clause(Db, Table, erlog_ec_support:functor(H), element(1, Clause)),
      {UCursor, Res} = erlog_memory:db_next(Db, Cursor, Table),
      retractall_clauses(Table, Res, H, B, Params#param{cursor = UCursor});
    fail ->
      retractall_clauses(Table, [], H, B, Params)
  end.

%% @private
check_retractall_result({built_in, _}, _, _, Functor, _, _) ->
  erlog_errors:permission_error(modify, static_procedure, erlog_ec_support:pred_ind(Functor));
check_retractall_result({code, _}, _, _, Functor, _, _) ->
  erlog_errors:permission_error(modify, static_procedure, erlog_ec_support:pred_ind(Functor));
check_retractall_result({clauses, Cs}, H, B, _, Table, Params = #param{cursor = Cursor}) ->
  Fun = fun(Param) -> retractall_clauses(Table, Cs, H, B, Param) end,
  erlog_ec_core:run_n_close(Fun, Params#param{cursor = Cursor});
check_retractall_result(undefined, _, _, _, _, Params = #param{next_goal = Next}) ->
  erlog_ec_core:prove_body(Params#param{goal = Next});
check_retractall_result({erlog_error, E}, _, _, _, _, #param{database = Db}) -> erlog_errors:erlog_error(E, Db).