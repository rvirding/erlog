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

-include("erlog_int.hrl").

%% API
-export([type_error/3, instantiation_error/1, permission_error/4,
	type_error/2, instantiation_error/0, erlog_error/2, erlog_error/1, fail/3]).

%% Errors
%% To keep dialyzer quiet.
-spec type_error(_, _) -> no_return().
-spec type_error(_, _, _) -> no_return().
-spec instantiation_error() -> no_return().
-spec instantiation_error(_) -> no_return().
-spec permission_error(_, _, _, _) -> no_return().
-spec erlog_error(_) -> no_return().
-spec erlog_error(_, _) -> no_return().

type_error(Type, Value, Db) -> erlog_error({type_error, Type, Value}, Db).
type_error(Type, Value) -> erlog_error({type_error, Type, Value}).

instantiation_error(Db) -> erlog_error(instantiation_error, Db).
instantiation_error() -> erlog_error(instantiation_error).

permission_error(Op, Type, Value, Db) ->
	erlog_error({permission_error, Op, Type, Value}, Db).

erlog_error(E, Db) -> throw({erlog_error, E, Db}).
erlog_error(E) -> throw({erlog_error, E}).

%% fail(ChoicePoints, Database) -> {fail,Database}.
%% cut(Label, Last, Next, ChoicePoints, Bindings, VarNum, Database) -> void.
%%
%%  The functions which manipulate the choice point stack.  fail
%%  backtracks to next choicepoint skipping cut labels cut steps
%%  backwards over choice points until matching cut.
fail([#cp{type = goal_clauses} = Cp | Cps], Db, Fcon) ->
	fail_goal_clauses(Cp, Cps, Db, Fcon);
fail([#cp{type = disjunction} = Cp | Cps], Db, Fcon) ->
	fail_disjunction(Cp, Cps, Db, Fcon);
fail([#cp{type = if_then_else} = Cp | Cps], Db, Fcon) ->
	fail_if_then_else(Cp, Cps, Db, Fcon);
fail([#cp{type = clause} = Cp | Cps], Db, Fcon) ->
	fail_clause(Cp, Cps, Db, Fcon);
fail([#cp{type = retract} = Cp | Cps], Db, Fcon) ->
	fail_retract(Cp, Cps, Db, Fcon);
fail([#cp{type = current_predicate} = Cp | Cps], Db, Fcon) ->
	fail_current_predicate(Cp, Cps, Db, Fcon);
fail([#cp{type = ecall} = Cp | Cps], Db, Fcon) ->
	fail_ecall(Cp, Cps, Db, Fcon);
fail([#cp{type = compiled, data = F} = Cp | Cps], Db, _) ->
	F(Cp, Cps, Db);
fail([#cut{} | Cps], Db, Fcon) ->
	fail(Cps, Db, Fcon);        %Fail over cut points.
fail([], Db, _) -> {fail, Db}.

%% @private
fail_disjunction(#cp{next = Next, bs = Bs, vn = Vn}, Cps, Db, Fcon) ->
	erlog_core:prove_body(Next, Cps, Bs, Vn, Db, Fcon).

%% @private
fail_if_then_else(#cp{next = Next, bs = Bs, vn = Vn}, Cps, Db, Fcon) ->
	erlog_core:prove_body(Next, Cps, Bs, Vn, Db, Fcon).

%% @private
fail_ecall(#cp{data = {Efun, Val}, next = Next, bs = Bs, vn = Vn}, Cps, Db, Fcon) ->
	erlog_core:prove_ecall(Efun, Val, Next, Cps, Bs, Vn, Db, Fcon).

%% @private
fail_clause(#cp{data = {Ch, Cb, Cs}, next = Next, bs = Bs, vn = Vn}, Cps, Db, Fcon) ->
	erlog_core:unify_clauses(Ch, Cb, Cs, Next, Cps, Bs, Vn, Db, Fcon).

%% @private
fail_retract(#cp{data = {Ch, Cb, Cs}, next = Next, bs = Bs, vn = Vn}, Cps, Db, Fcon) ->
	erlog_core:retract_clauses(Ch, Cb, Cs, Next, Cps, Bs, Vn, Db, Fcon).

%% @private
fail_current_predicate(#cp{data = {Pi, Fs}, next = Next, bs = Bs, vn = Vn}, Cps, Db, Fcon) ->
	erlog_core:prove_predicates(Pi, Fs, Next, Cps, Bs, Vn, Db, Fcon).

%% @private
fail_goal_clauses(#cp{data = {G, Cs}, next = Next, bs = Bs, vn = Vn}, Cps, Db, Fcon) ->
	erlog_core:prove_goal_clauses(G, Cs, Next, Cps, Bs, Vn, Db, Fcon).