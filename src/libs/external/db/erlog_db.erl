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

%% API
-export([load/1, db_assert_2/2, db_asserta_2/2, db_abolish_2/2, db_retract_2/2, db_retractall_2/2, fail_retract/2, db_call_2/2]).

load(Db) ->
	lists:foreach(fun(Proc) -> erlog_memory:add_compiled_proc(Db, Proc) end, ?ERLOG_DB).

db_call_2({db_call, Table, Goal}, Param = #param{database = Db}) ->
%% Only add cut CP to Cps if goal contains a cut.
	case erlog_memory:db_findall(Db, Table, Goal) of
		[] -> erlog_errors:fail(Param);
		Cs -> erlog_core:prove_goal_clauses(Goal, Cs, Param)
	end.

db_assert_2({db_assert, Table, Fact}, Params = #param{next_goal = Next, bindings = Bs, database = Db}) ->
	C = ec_support:dderef(Fact, Bs),
	erlog_memory:db_assertz_clause(Db, Table, C),
	ec_body:prove_body(Params#param{goal = Next}).

db_asserta_2({db_asserta, Table, Fact}, Params = #param{next_goal = Next, bindings = Bs, database = Db}) ->
	C = ec_support:dderef(Fact, Bs),
	erlog_memory:db_asserta_clause(Db, Table, C),
	ec_body:prove_body(Params#param{goal = Next}).

db_abolish_2({db_abolish, Table, Fact}, Params = #param{next_goal = Next, bindings = Bs, database = Db}) ->
	case ec_support:dderef(Fact, Bs) of
		{'/', N, A} when is_atom(N), is_integer(A), A > 0 ->
			erlog_memory:db_abolish_clauses(Db, Table, {N, A}),
			ec_body:prove_body(Params#param{goal = Next});
		Pi -> erlog_errors:type_error(predicate_indicator, Pi, Db)
	end.

db_retract_2({db_retract, Table, Fact}, Params = #param{bindings = Bs}) ->
	C = ec_support:dderef(Fact, Bs),
	prove_retract(C, Table, Params).

db_retractall_2({db_retractall, Table, Fact}, Params = #param{bindings = Bs}) ->
	C = ec_support:dderef(Fact, Bs),
	prove_retractall(C, Table, Params).


prove_retract({':-', H, B}, Table, Params) ->
	prove_retract(H, B, Table, Params);
prove_retract(H, Table, Params) ->
	prove_retract(H, true, Table, Params).

prove_retractall({':-', H, B}, Table, Params) ->
	prove_retractall(H, B, Table, Params);
prove_retractall(H, Table, Params) ->
	prove_retractall(H, true, Table, Params).

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
			ec_body:prove_body(Params#param{goal = Next});
		{code, _} ->
			erlog_errors:permission_error(modify, static_procedure, ec_support:pred_ind(Functor), Db);
		built_in ->
			erlog_errors:permission_error(modify, static_procedure, ec_support:pred_ind(Functor), Db);
		undefined -> ec_body:prove_body(Params#param{goal = Next})
	end.

%% @private
retract(Ch, Cb, C, Cs, Param = #param{next_goal = Next, choice = Cps, bindings = Bs0, var_num = Vn0, database = Db}, Bs1, Vn1, Table) ->
	erlog_memory:db_retract_clause(Db, Table, ec_support:functor(Ch), element(1, C)),
	Cp = #cp{type = db_retract, data = {Ch, Cb, Cs, Table}, next = Next, bs = Bs0, vn = Vn0},
	ec_body:prove_body(Param#param{goal = Next, choice = [Cp | Cps], bindings = Bs1, var_num = Vn1}).

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