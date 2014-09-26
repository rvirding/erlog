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
  db_listing_4/1]).

load(Db) ->
  lists:foreach(fun(Proc) -> erlog_memory:load_library_space(Db, Proc) end, ?ERLOG_DB).

db_call_2(Param = #param{goal = {db_call, _, _} = Goal, next_goal = Next0, bindings = Bs, database = Db}) ->
  {db_call, Table, G} = ec_support:dderef(Goal, Bs),
  case erlog_memory:db_findall(Db, Table, ec_support:functor(G)) of
    [] -> erlog_errors:fail(Param);
    {erlog_error, E} -> erlog_errors:erlog_error(E, Db);
    {clauses, Cs} -> prove_call(G, Cs, Next0, Param);
    Cs -> prove_call(G, Cs, Next0, Param)
  end.

db_assert_2(Params = #param{goal = {db_assert, _, _} = Goal, next_goal = Next, bindings = Bs, database = Db}) ->
  {db_assert, Table, Fact} = ec_support:dderef(Goal, Bs),
  erlog_memory:db_assertz_clause(Db, Table, Fact),
  ec_core:prove_body(Params#param{goal = Next}).

db_asserta_2(Params = #param{goal = {db_asserta, _, _} = Goal, next_goal = Next, bindings = Bs, database = Db}) ->
  {db_asserta, Table, Fact} = ec_support:dderef(Goal, Bs),
  erlog_memory:db_asserta_clause(Db, Table, Fact),
  ec_core:prove_body(Params#param{goal = Next}).

db_abolish_2(Params = #param{goal = {db_abolish, _, _} = Goal, next_goal = Next, bindings = Bs, database = Db}) ->
  {db_abolish, Table, Fact} = ec_support:dderef(Goal, Bs),
  case Fact of
    {'/', N, A} when is_atom(N), is_integer(A), A > 0 ->
      erlog_memory:db_abolish_clauses(Db, Table, {N, A}),
      ec_core:prove_body(Params#param{goal = Next});
    Pi -> erlog_errors:type_error(predicate_indicator, Pi, Db)
  end.

db_retract_2(Params = #param{goal = {db_retract, _, _} = Goal, bindings = Bs}) ->
  {db_retract, Table, Fact} = ec_support:dderef(Goal, Bs),
  prove_retract(Fact, Table, Params).

db_retractall_2(Params = #param{goal = {db_retractall, _, _} = Goal, bindings = Bs}) ->
  {db_retractall, Table, Fact} = ec_support:dderef(Goal, Bs),
  prove_retractall(Fact, Table, Params).

db_listing_2(Params = #param{goal = {db_listing, _, _} = Goal, next_goal = Next, bindings = Bs0, database = Db}) ->
  {db_listing, Table, Res} = ec_support:dderef(Goal, Bs0),
  Content = erlog_memory:db_listing(Db, Table, []),
  Bs = ec_support:add_binding(Res, Content, Bs0),
  ec_core:prove_body(Params#param{goal = Next, bindings = Bs}).

db_listing_3(Params = #param{goal = {db_listing, _, _, _} = Goal, next_goal = Next, bindings = Bs0, database = Db}) ->
  {db_listing, Table, Functor, Res} = ec_support:dderef(Goal, Bs0),
  Content = erlog_memory:db_listing(Db, Table, [Functor]),
  Bs = ec_support:add_binding(Res, Content, Bs0),
  ec_core:prove_body(Params#param{goal = Next, bindings = Bs}).

db_listing_4(Params = #param{goal = {db_listing, _, _, _, _} = Goal, next_goal = Next, bindings = Bs0, database = Db}) ->
  {db_listing, Table, Functor, Arity, Res} = ec_support:dderef(Goal, Bs0),
  Content = erlog_memory:db_listing(Db, Table, [Functor, Arity]),
  Bs = ec_support:add_binding(Res, Content, Bs0),
  ec_core:prove_body(Params#param{goal = Next, bindings = Bs}).

prove_retract({':-', H, B}, Table, Params) ->
  prove_retract(H, B, Table, Params);
prove_retract(H, Table, Params) ->
  prove_retract(H, true, Table, Params).

prove_retractall({':-', H, B}, Table, Params) ->
  prove_retractall(H, B, Table, Params);
prove_retractall(H, Table, Params) ->
  prove_retractall(H, true, Table, Params).

%% @private
prove_call(G, Cs, Next0, Param = #param{bindings = Bs, choice = Cps, database = Db, var_num = Vn}) ->
  case ec_logic:check_goal(G, Next0, Bs, Db, false, Vn) of
    {[Next1 | _], true} ->
      %% Must increment Vn to avoid clashes!!!
      Cut = #cut{label = Vn},
      prove_goal_clauses(Cs, Param#param{goal = Next1, choice = [Cut | Cps], var_num = Vn + 1});
    {[Next1 | _], false} -> prove_goal_clauses(Cs, Param#param{goal = Next1, var_num = Vn + 1})
  end.

%% @private
prove_retract(H, B, Table, Params = #param{database = Db}) ->
  Functor = ec_support:functor(H),
  case erlog_memory:get_db_procedure(Db, Table, Functor) of
    {clauses, Cs} -> retract_clauses(H, B, Cs, Params, Table);
    undefined -> erlog_errors:fail(Params)
  end.

%% @private
prove_retractall(H, B, Table, Params = #param{next_goal = Next, bindings = Bs0, var_num = Vn0, database = Db}) ->
  Functor = ec_support:functor(H),
  case erlog_memory:get_db_procedure(Db, Table, Functor) of
    {clauses, Cs} ->
      lists:foreach(
        fun(Clause) ->
          case ec_unify:unify_clause(H, B, Clause, Bs0, Vn0) of
            {succeed, _, _} ->
              erlog_memory:db_retract_clause(Db, Table, ec_support:functor(H), element(1, Clause));
            fail -> ok
          end
        end, Cs),
      ec_core:prove_body(Params#param{goal = Next});
    {code, _} ->
      erlog_errors:permission_error(modify, static_procedure, ec_support:pred_ind(Functor));
    built_in ->
      erlog_errors:permission_error(modify, static_procedure, ec_support:pred_ind(Functor));
    undefined -> ec_core:prove_body(Params#param{goal = Next})
  end.

%% @private
retract(Ch, Cb, C, Cs, Param = #param{next_goal = Next, choice = Cps, bindings = Bs0, var_num = Vn0, database = Db}, Bs1, Vn1, Table) ->
  erlog_memory:db_retract_clause(Db, Table, ec_support:functor(Ch), element(1, C)),
  Cp = #cp{type = db_retract, data = {Ch, Cb, Cs, Table}, next = Next, bs = Bs0, vn = Vn0},
  ec_core:prove_body(Param#param{goal = Next, choice = [Cp | Cps], bindings = Bs1, var_num = Vn1}).

%% @private
%% retract_clauses(Head, Body, Clauses, Next, ChoicePoints, Bindings, VarNum, Database) ->
%%      void.
%% Try to retract Head and Body using Clauses which all have the same functor.
retract_clauses(_Ch, _Cb, [], Param, _) -> erlog_errors:fail(Param);
retract_clauses(Ch, Cb, [C | Cs], Param = #param{bindings = Bs0, var_num = Vn0}, Table) -> %TODO foreach vs handmade recursion?
  case ec_unify:unify_clause(Ch, Cb, C, Bs0, Vn0) of
    {succeed, Bs1, Vn1} ->
      %% We have found a right clause so now retract it.
      retract(Ch, Cb, C, Cs, Param, Bs1, Vn1, Table);
    fail -> retract_clauses(Ch, Cb, Cs, Param, Table)
  end.

fail_retract(#cp{data = {Ch, Cb, Cs, Table}, next = Next, bs = Bs, vn = Vn}, Param) ->
  retract_clauses(Ch, Cb, Cs, Param#param{next_goal = Next, bindings = Bs, var_num = Vn}, Table).

%% prove_goal_clauses(Goal, Clauses, Next, ChoicePoints, Bindings, VarNum, Database) ->
%%      void.
%% Try to prove Goal using Clauses which all have the same functor.
prove_goal_clauses([C], Params = #param{choice = Cps, var_num = Vn}) ->
  %% Must be smart here and test whether we need to add a cut point.
  %% C has the structure {Tag,Head,{Body,BodyHasCut}}.
  case element(2, element(3, C)) of
    true ->
      Cut = #cut{label = Vn},
      erlog_errors:fail(Params#param{choice = [Cut | Cps]});
    false ->
      erlog_errors:fail(Params)
  end;
prove_goal_clauses(C, Params = #param{goal = G, next_goal = Next, var_num = Vn, bindings = Bs, choice = Cps, database = Db}) ->
  Cp = #cp{type = db_goal_clauses, label = Vn, data = {G, Db, C}, next = Next, bs = Bs, vn = Vn},
  ec_core:prove_goal_clause(C, Params#param{choice = [Cp | Cps]}).