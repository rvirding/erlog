%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. июн 2014 23:16
%%%-------------------------------------------------------------------
-module(erlog_errors).
-author("tihon").

-include("erlog_core.hrl").

%% API
-export([type_error/3, instantiation_error/1, permission_error/3,
  type_error/2, instantiation_error/0, erlog_error/2, erlog_error/1, fail/1]).

%% Errors
%% To keep dialyzer quiet.
-spec type_error(_, _) -> no_return().
-spec type_error(_, _, _) -> no_return().
-spec instantiation_error() -> no_return().
-spec instantiation_error(_) -> no_return().
-spec permission_error(_, _, _) -> no_return().
-spec erlog_error(_) -> no_return().
-spec erlog_error(_, _) -> no_return().

type_error(Type, Value, Db) -> erlog_error({type_error, Type, Value}, Db).
type_error(Type, Value) -> erlog_error({type_error, Type, Value}).

instantiation_error(Db) -> erlog_error(instantiation_error, Db).
instantiation_error() -> erlog_error(instantiation_error).

permission_error(Op, Type, Value) ->
  erlog_error({permission_error, Op, Type, Value}).

erlog_error(E, Db) -> throw({erlog_error, E, Db}).  %TODO remove DB!!
erlog_error(E) -> throw({erlog_error, E}).

%% fail(ChoicePoints, Database) -> {fail,Database}.
%% cut(Label, Last, Next, ChoicePoints, Bindings, VarNum, Database) -> void.
%%
%%  The functions which manipulate the choice point stack.  fail
%%  backtracks to next choicepoint skipping cut labels cut steps
%%  backwards over choice points until matching cut.
fail(Param = #param{choice = [#cp{type = goal_clauses} = Cp | Cps]}) ->
  fail_goal_clauses(Cp, Param#param{choice = Cps});
fail(Param = #param{choice = [#cp{type = db_goal_clauses} = Cp | Cps]}) ->
  erlog_db_logic:fail_goal_clauses(Cp, Param#param{choice = Cps});
fail(Param = #param{choice = [#cp{type = Type} = Cp | Cps]}) when Type == disjunction; Type == if_then_else ->
  fail_disjunction(Cp, Param#param{choice = Cps});
fail(Param = #param{choice = [#cp{type = clause} = Cp | Cps]}) ->
  fail_clause(Cp, Param#param{choice = Cps});
fail(Param = #param{choice = [#cp{type = findall} = Cp | Cps]}) ->
  fail_findall(Cp, Param#param{choice = Cps});
fail(Param = #param{choice = [#cp{type = retract} = Cp | Cps]}) ->
  fail_retract(Cp, Param#param{choice = Cps});
fail(Param = #param{choice = [#cp{type = db_retract} = Cp | Cps]}) ->
  erlog_db_logic:fail_retract(Cp, Param#param{choice = Cps});
fail(Param = #param{choice = [#cp{type = current_predicate} = Cp | Cps]}) ->
  fail_current_predicate(Cp, Param#param{choice = Cps});
fail(Param = #param{choice = [#cp{type = ecall} = Cp | Cps]}) ->
  fail_ecall(Cp, Param#param{choice = Cps});
fail(#param{choice = [#cp{type = compiled, data = F} = Cp | Cps], database = Db}) ->
  F(Cp, Cps, Db); %TODO test this
fail(Param = #param{choice = [#cut{} | Cps]}) ->
  fail(Param#param{choice = Cps});        %Fail over cut points.
fail(#param{choice = [], database = Db}) -> {fail, Db}.

%% @private
fail_disjunction(#cp{next = Next, bs = Bs, vn = Vn}, Param) ->
  erlog_ec_core:prove_body(Param#param{goal = Next, bindings = Bs, var_num = Vn}).

%% @private
fail_ecall(#cp{data = {Efun, Val}, next = Next, bs = Bs, vn = Vn}, Param) ->
  erlog_ec_logic:prove_ecall(Efun, Val, Param#param{next_goal = Next, bindings = Bs, var_num = Vn}).

%% @private
fail_clause(#cp{data = {Ch, Cb, Db, Cursor}, next = Next, bs = Bs, vn = Vn}, Param) ->
  {UCursor, Res} = erlog_memory:next(Db, Cursor),
  erlog_ec_unify:unify_clauses(Ch, Cb, Res, Param#param{next_goal = Next, bindings = Bs, var_num = Vn, cursor = UCursor}).

%% @private
fail_retract(#cp{data = {Ch, Cb, {Db, Cursor}}, next = Next, bs = Bs, vn = Vn}, Param) ->
  {UCursor, Res} = erlog_memory:next(Db, Cursor),
  erlog_ec_logic:retract_clauses(Ch, Cb, Res, Param#param{next_goal = Next, bindings = Bs, var_num = Vn, cursor = UCursor}).

%% @private
fail_current_predicate(#cp{data = {Pi, Fs}, next = Next, bs = Bs, vn = Vn}, Param) ->
  erlog_ec_logic:prove_predicates(Pi, Fs, Param#param{next_goal = Next, bindings = Bs, var_num = Vn}).

%% @private
fail_goal_clauses(#cp{data = {G, Db, Cursor}, next = Next, bs = Bs, vn = Vn}, Param) ->
  {UCursor, Res} = erlog_memory:next(Db, Cursor),
  erlog_ec_core:prove_goal_clauses(Res, Param#param{goal = G, next_goal = Next, bindings = Bs, var_num = Vn, cursor = UCursor}).

fail_findall(#cp{next = Next, data = {Tag, Bag}, bs = Bs, vn = Vn0}, Param = #param{database = Db}) ->
  Data = erlog_memory:raw_fetch(Db, Tag),
  erlog_memory:raw_erase(Db, Tag),  %Clear special entry
  {Bs1, Vn1} = lists:mapfoldl(fun(B0, V0) ->  %Create proper instances
    {B1, _, V1} = erlog_ec_term:term_instance(erlog_ec_support:dderef(B0, Bs), V0),
    {B1, V1}
  end, Vn0, Data),
  erlog_ec_body:unify_prove_body(Bag, Bs1, Param#param{next_goal = Next, var_num = Vn1}).