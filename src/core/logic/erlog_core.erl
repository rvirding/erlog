%% Copyright (c) 2008-2013 Robert Virding
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

-module(erlog_core).

-include("erlog_int.hrl").

%% Main execution functions.
-export([
	retract_clauses/4,
	prove_predicates/3,
	prove_goal_clauses/3,
	prove_retract/2,
	prove_clause/3,
	prove_current_predicate/2,
	prove_ecall/3,
	prove_goal/4]).
%% Adding to database.
-export([load/1]).

%% built_in_db(Db) -> Database.
%% Create an initial clause database containing the built-in
%% predicates and predefined library predicates.

load(Db) ->
	lists:foreach(fun(Head) -> erlog_memory:add_built_in(Db, Head) end, ?ERLOG_CORE). %% Add the Erlang built-ins.

%% prove_goal(Goal, Database) -> Succeed | Fail.
%% This is the main entry point into the interpreter. Check that
%% everything is consistent then prove the goal as a call.
-spec prove_goal(Goal0 :: term(), Db :: pid(), Fcon :: fun(), Event :: pid()) -> term().
prove_goal(Goal0, Db, Fcon, Event) ->
	%% put(erlog_cut, orddict:new()),
	%% put(erlog_cps, orddict:new()),
	%% put(erlog_var, orddict:new()),
	%% Check term and build new instance of term with bindings.
	{Goal1, Bs, Vn} = ec_goals:initial_goal(Goal0),
	Params = #param{goal = [{call, Goal1}], choice = [], bindings = Bs, var_num = Vn,
		event_man = Event, database = Db, f_consulter = Fcon},
	ec_body:prove_body(Params). %TODO use lists:foldr instead!

%% prove_ecall(Generator, Value, Next, ChoicePoints, Bindings, VarNum, Database) ->
%%	void.
%% Call an external (Erlang) generator and handle return value, either
%% succeed or fail.
prove_ecall(Efun, Val, Param = #param{next_goal = Next, choice = Cps, bindings = Bs, var_num = Vn}) ->
	case Efun() of
		{succeed, Ret, Cont} ->      %Succeed and more choices
			Cp = #cp{type = ecall, data = {Cont, Val}, next = Next, bs = Bs, vn = Vn},
			ec_body:unify_prove_body(Val, Ret, Param#param{choice = [Cp | Cps]});
		{succeed_last, Ret} ->      %Succeed but last choice
			ec_body:unify_prove_body(Val, Ret, Param);
		fail -> erlog_errors:fail(Param)      %No more
	end.

%% prove_clause(Head, Body, Next, ChoicePoints, Bindings, VarNum, DataBase) ->
%%      void.
%% Unify clauses matching with functor from Head with both Head and Body.
prove_clause(H, B, Param = #param{database = Db}) ->
	Functor = ec_support:functor(H),
	case erlog_memory:get_procedure(Db, Functor) of
		{clauses, Cs} -> ec_unify:unify_clauses(H, B, Cs, Param);
		{code, _} ->
			erlog_errors:permission_error(access, private_procedure, ec_support:pred_ind(Functor), Db);
		built_in ->
			erlog_errors:permission_error(access, private_procedure, ec_support:pred_ind(Functor), Db);
		undefined -> erlog_errors:fail(Param)
	end.

%% prove_current_predicate(PredInd, Next, ChoicePoints, Bindings, VarNum, DataBase) ->
%%      void.
%% Match functors of existing user (interpreted) predicate with PredInd.
prove_current_predicate(Pi, Param = #param{database = Db}) ->
	case Pi of
		{'/', _, _} -> ok;
		{_} -> ok;
		Other -> erlog_errors:type_error(predicate_indicator, Other)
	end,
	Fs = erlog_memory:get_interp_functors(Db),
	prove_predicates(Pi, Fs, Param).

prove_predicates(Pi, [F | Fs], Param = #param{next_goal = Next, choice = Cps, bindings = Bs, var_num = Vn}) ->
	Cp = #cp{type = current_predicate, data = {Pi, Fs}, next = Next, bs = Bs, vn = Vn},
	ec_body:unify_prove_body(Pi, ec_support:pred_ind(F), Param#param{choice = [Cp | Cps]});
prove_predicates(_Pi, [], Param) -> erlog_errors:fail(Param).

%% prove_goal_clauses(Goal, Clauses, Next, ChoicePoints, Bindings, VarNum, Database) ->
%%      void.
%% Try to prove Goal using Clauses which all have the same functor.
prove_goal_clauses(G, [C], Params = #param{choice = Cps, var_num = Vn}) ->
	%% Must be smart here and test whether we need to add a cut point.
	%% C has the structure {Tag,Head,{Body,BodyHasCut}}.
	case element(2, element(3, C)) of
		true ->
			Cut = #cut{label = Vn},
			prove_goal_clause(G, C, Params#param{choice = [Cut | Cps]});
		false ->
			prove_goal_clause(G, C, Params)
	end;
%% prove_goal_clause(G, C, Next, Cps, Bs, Vn, Db);
prove_goal_clauses(G, [C | Cs], Params = #param{next_goal = Next, var_num = Vn, bindings = Bs, choice = Cps}) ->
	Cp = #cp{type = goal_clauses, label = Vn, data = {G, Cs}, next = Next, bs = Bs, vn = Vn},
	prove_goal_clause(G, C, Params#param{choice = [Cp | Cps]});
prove_goal_clauses(_G, [], Param) -> erlog_errors:fail(Param).

prove_goal_clause(G, {_Tag, H0, {B0, _}}, Param = #param{next_goal = Next, bindings = Bs0, var_num = Vn0}) ->
	%% io:fwrite("PGC1: ~p\n", [{G,H0,B0}]),
	Label = Vn0,
	case ec_unify:unify_head(G, H0, Bs0, Vn0 + 1) of
		{succeed, Rs0, Bs1, Vn1} ->
			%% io:fwrite("PGC2: ~p\n", [{Rs0}]),
			{B1, _Rs2, Vn2} = ec_body:body_instance(B0, Next, Rs0, Vn1, Label),
			%% io:fwrite("PGC3: ~p\n", [{B1,Next,Cps}]),
			ec_body:prove_body(Param#param{goal = B1, bindings = Bs1, var_num = Vn2});
		fail -> erlog_errors:fail(Param)
	end.

%% prove_retract(Clause, Next, ChoicePoints, Bindings, VarNum, Database) ->
%%      void.
%% Retract clauses in database matching Clause.
prove_retract({':-', H, B}, Params) ->
	prove_retract(H, B, Params);
prove_retract(H, Params) ->
	prove_retract(H, true, Params).

prove_retract(H, B, Params = #param{database = Db}) ->
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