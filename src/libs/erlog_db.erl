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
-export([load/1, db_assert_2/2, db_asserta_2/2, db_abolish_2/2, db_retract_2/2]).

load(Db) ->
	lists:foreach(fun(Proc) -> erlog_memory:add_compiled_proc(Db, Proc) end, ?ERLOG_DB).

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

db_retract_2({db_retract, Table, Fact}, Params = #param{next_goal = Next, bindings = Bs, database = Db}) ->
	C = ec_support:dderef(Fact, Bs),
	prove_retract(C, Table, Params).

prove_retract({':-', H, B}, Table, Params) ->
	prove_retract(H, B, Table, Params);
prove_retract(H, Table, Params) ->
	prove_retract(H, true, Table, Params).

prove_retract(H, B, Table, Params = #param{database = Db}) ->
	Functor = ec_support:functor(H),
	case erlog_memory:get_procedure(Db, Functor) of
		{clauses, Cs} -> retract_clauses(H, B, Cs, Params);
		{code, _} ->
			erlog_errors:permission_error(modify, static_procedure, ec_support:pred_ind(Functor), Db);
		built_in ->
			erlog_errors:permission_error(modify, static_procedure, ec_support:pred_ind(Functor), Db);
		undefined -> erlog_errors:fail(Params)
	end.

%% retract_clauses(Head, Body, Clauses, Next, ChoicePoints, Bindings, VarNum, Database) ->
%%      void.
%% Try to retract Head and Body using Clauses which all have the same functor.
retract_clauses(Ch, Cb, [C | Cs], Param = #param{next_goal = Next, choice = Cps, bindings = Bs0, var_num = Vn0, database = Db}) -> %TODO foreach vs handmade recursion?
	case ec_unify:unify_clause(Ch, Cb, C, Bs0, Vn0) of
		{succeed, Bs1, Vn1} ->
			%% We have found a right clause so now retract it.
			erlog_memory:retract_clause(Db, ec_support:functor(Ch), element(1, C)),
			Cp = #cp{type = retract, data = {Ch, Cb, Cs}, next = Next, bs = Bs0, vn = Vn0},
			ec_body:prove_body(Param#param{goal = Next, choice = [Cp | Cps], bindings = Bs1, var_num = Vn1});
		fail -> retract_clauses(Ch, Cb, Cs, Param)
	end;
retract_clauses(_Ch, _Cb, [], Param) -> erlog_errors:fail(Param).