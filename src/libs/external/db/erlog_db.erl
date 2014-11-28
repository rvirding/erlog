%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Июль 2014 11:18
%%%-------------------------------------------------------------------
-module(erlog_db).
-author("tihon").

-include("erlog_core.hrl").
-include("erlog_db.hrl").

-behaviour(erlog_exlib).

%% API
-export([load/1,
  db_assert_2/1,
  db_asserta_2/1,
  db_abolish_2/1,
  db_retract_2/1,
  db_retractall_2/1,
  fail_retract/2,
  db_call_2/1,
  db_listing_2/1,
  db_listing_3/1,
  db_listing_4/1, prove_call/4]).

load(DbState) ->
  lists:foldl(fun(Proc, UDBState) -> erlog_memory:load_native_library(UDBState, Proc) end, DbState, ?ERLOG_DB).

db_call_2(Param = #param{goal = {db_call, _, _} = Goal, next_goal = Next0, bindings = Bs, database = Db}) ->
  {db_call, Table, G} = erlog_ec_support:dderef(Goal, Bs),
  case erlog_memory:db_findall(Db, Table, G) of
    {{cursor, Cursor, result, Result}, UDb} ->
      Fun = fun(Params) -> check_call_result(Result, Params, G, Next0) end,
      erlog_ec_core:run_n_close(Fun, Param#param{cursor = Cursor, database = UDb});
    {Result, UDb} -> check_call_result(Result, Param#param{database = UDb}, G, Next0)
  end.

db_assert_2(Params = #param{goal = {db_assert, _, _} = Goal, next_goal = Next, bindings = Bs, database = Db}) ->
  {db_assert, Table, Fact} = erlog_ec_support:dderef(Goal, Bs),
  {_, UDb} = erlog_memory:db_assertz_clause(Db, Table, Fact),
  erlog_ec_core:prove_body(Params#param{goal = Next, database = UDb}).

db_asserta_2(Params = #param{goal = {db_asserta, _, _} = Goal, next_goal = Next, bindings = Bs, database = Db}) ->
  {db_asserta, Table, Fact} = erlog_ec_support:dderef(Goal, Bs),
  {_, UDb} = erlog_memory:db_asserta_clause(Db, Table, Fact),
  erlog_ec_core:prove_body(Params#param{goal = Next, database = UDb}).

db_abolish_2(Params = #param{goal = {db_abolish, _, _} = Goal, next_goal = Next, bindings = Bs, database = Db}) ->
  {db_abolish, Table, Fact} = erlog_ec_support:dderef(Goal, Bs),
  case Fact of
    {'/', N, A} when is_atom(N), is_integer(A), A > 0 ->
      {_, UDb} = erlog_memory:db_abolish_clauses(Db, Table, {N, A}),
      erlog_ec_core:prove_body(Params#param{goal = Next, database = UDb});
    Pi -> erlog_errors:type_error(predicate_indicator, Pi, Db)
  end.

db_retract_2(Params = #param{goal = {db_retract, _, _} = Goal, bindings = Bs}) ->
  {db_retract, Table, Fact} = erlog_ec_support:dderef(Goal, Bs),
  prove_retract(Fact, Table, Params).

db_retractall_2(Params = #param{goal = {db_retractall, _, _} = Goal, bindings = Bs}) ->
  {db_retractall, Table, Fact} = erlog_ec_support:dderef(Goal, Bs),
  prove_retractall(Fact, Table, Params).

db_listing_2(Params = #param{goal = {db_listing, _, _} = Goal, next_goal = Next, bindings = Bs0, database = Db}) ->
  {db_listing, Table, Res} = erlog_ec_support:dderef(Goal, Bs0),
  {Content, UDb} = erlog_memory:db_listing(Db, Table, []),
  Bs = erlog_ec_support:add_binding(Res, Content, Bs0),
  erlog_ec_core:prove_body(Params#param{goal = Next, bindings = Bs, database = UDb}).

db_listing_3(Params = #param{goal = {db_listing, _, _, _} = Goal, next_goal = Next, bindings = Bs0, database = Db}) ->
  {db_listing, Table, Functor, Res} = erlog_ec_support:dderef(Goal, Bs0),
  {Content, UDb} = erlog_memory:db_listing(Db, Table, [Functor]),
  Bs = erlog_ec_support:add_binding(Res, Content, Bs0),
  erlog_ec_core:prove_body(Params#param{goal = Next, bindings = Bs, database = UDb}).

db_listing_4(Params = #param{goal = {db_listing, _, _, _, _} = Goal, next_goal = Next, bindings = Bs0, database = Db}) ->
  {db_listing, Table, Functor, Arity, Res} = erlog_ec_support:dderef(Goal, Bs0),
  {Content, UDb} = erlog_memory:db_listing(Db, Table, [Functor, Arity]),
  Bs = erlog_ec_support:add_binding(Res, Content, Bs0),
  erlog_ec_core:prove_body(Params#param{goal = Next, bindings = Bs, database = UDb}).

prove_retract({':-', H, B}, Table, Params) ->
  prove_retract(H, B, Table, Params);
prove_retract(H, Table, Params) ->
  prove_retract(H, true, Table, Params).

prove_retractall({':-', H, B}, Table, Params) ->
  prove_retractall(H, B, Table, Params);
prove_retractall(H, Table, Params) ->
  prove_retractall(H, true, Table, Params).

prove_call(G, Cs, Next0, Param = #param{bindings = Bs, choice = Cps, database = Db, var_num = Vn}) ->
  case erlog_ec_logic:check_goal(G, Next0, Bs, Db, false, Vn) of
    {[Next1 | _], true} ->
      %% Must increment Vn to avoid clashes!!!
      Cut = #cut{label = Vn},
      erlog_ec_core:prove_goal_clauses(Cs, Param#param{goal = Next1, choice = [Cut | Cps], var_num = Vn + 1});
    {[Next1 | _], false} -> erlog_ec_core:prove_goal_clauses(Cs, Param#param{goal = Next1, var_num = Vn + 1})
  end.


%% @private
prove_retract(H, B, Table, Params = #param{database = Db}) ->
  case erlog_memory:get_db_procedure(Db, Table, H) of
    {{cursor, Cursor, result, {clauses, Cs}, UDB}} ->
      erlog_ec_core:run_n_close(fun(Param) ->
        retract_clauses(H, B, Cs, Param, Table) end, Params#param{cursor = Cursor, database = UDB});
    {undefined, UDB} -> erlog_errors:fail(Params#param{database = UDB});
    _ ->
      Functor = erlog_ec_support:functor(H),
      erlog_errors:permission_error(modify, static_procedure, erlog_ec_support:pred_ind(Functor))
  end.

%% @private
prove_retractall(H, B, Table, Params = #param{database = Db}) ->
  Functor = erlog_ec_support:functor(H),
  case erlog_memory:get_db_procedure(Db, Table, H) of
    {{cursor, Cursor, result, Res}, UDb} ->
      check_retractall_result(Res, H, B, Functor, Table, Params#param{cursor = Cursor, database = UDb});
    {Res, UDb} ->
      check_retractall_result(Res, H, B, Functor, Table, Params#param{database = UDb})
  end.

%% @private
retract(Ch, Cb, C, Cursor, Param = #param{next_goal = Next, choice = Cps, bindings = Bs0, var_num = Vn0, database = Db}, Bs1, Vn1, Table) ->
  {_, UDb} = erlog_memory:db_retract_clause(Db, Table, erlog_ec_support:functor(Ch), element(1, C)),
  Cp = #cp{type = db_retract, data = {Ch, Cb, {UDb, Cursor}, Table}, next = Next, bs = Bs0, vn = Vn0},
  erlog_ec_core:prove_body(Param#param{goal = Next, choice = [Cp | Cps], bindings = Bs1, var_num = Vn1, database = UDb}).

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
      {{UCursor, Res}, UDb} = erlog_memory:db_next(Db, Cursor, Table),
      retract_clauses(Ch, Cb, Res, Param#param{cursor = UCursor, database = UDb}, Table)
  end.

fail_retract(#cp{data = {Ch, Cb, {Db, Cursor}, Table}, next = Next, bs = Bs, vn = Vn}, Param) ->
  {{UCursor, Res}, UDb} = erlog_memory:db_next(Db, Cursor, Table),
  retract_clauses(Ch, Cb, Res, Param#param{next_goal = Next, bindings = Bs, var_num = Vn, cursor = UCursor, database = UDb}, Table).

%% @private
check_call_result([], Param, _, _) -> erlog_errors:fail(Param);
check_call_result({clauses, Cs}, Param, G, Next) -> prove_call(G, Cs, Next, Param);
check_call_result({erlog_error, E}, #param{database = Db}, _, _) -> erlog_errors:erlog_error(E, Db);
check_call_result(Cs, Param, G, Next) -> prove_call(G, Cs, Next, Param).

retractall_clauses(_, [], _, _, Params = #param{next_goal = Next}) ->
  erlog_ec_core:prove_body(Params#param{goal = Next});
retractall_clauses(Table, [Clause], H, B, Params) -> retractall_clauses(Table, Clause, H, B, Params);
retractall_clauses(Table, Clause, H, B, Params = #param{bindings = Bs0, var_num = Vn0, database = Db, cursor = Cursor}) ->
  case erlog_ec_unify:unify_clause(H, B, Clause, Bs0, Vn0) of
    {succeed, _, _} ->
      {_, UDb1} = erlog_memory:db_retract_clause(Db, Table, erlog_ec_support:functor(H), element(1, Clause)),
      {{UCursor, Res}, UDb2} = erlog_memory:db_next(UDb1, Cursor, Table),
      retractall_clauses(Table, Res, H, B, Params#param{cursor = UCursor, database = UDb2});
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