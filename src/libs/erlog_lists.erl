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

-include("erlog_int.hrl").

%% Main interface functions.
-export([load/1]).

%% Library functions.
-export([append_3/2, insert_3/2, member_2/2, memberchk_2/2, reverse_2/2, sort_2/2]).

%% load(Database) -> Database.
%%  Assert predicates into the database.
load(Db) ->
	%% Compiled common list library.
	lists:foreach(fun(Proc) -> erlog_memory:add_compiled_proc(Db, Proc) end, ?ERLOG_LISTS),
	%% Finally interpreted common list library.
	lists:foreach(fun(Clause) -> erlog_memory:assertz_clause(Db, Clause) end,
		[
			%% insert(L, X, [X|L]). insert([H|L], X, [H|L1]) :- insert(L, X, L1).
			%% delete([X|L], X, L). delete([H|L], X, [H|L1]) :- delete(L, X, L1).
			{':-', {delete, {1}, {2}, {3}}, {insert, {3}, {2}, {1}}},
			%% perm([], []).
			%% perm([X|Xs], Ys1) :- perm(Xs, Ys), insert(Ys, X, Ys1).
			{perm, [], []},
			{':-', {perm, [{1} | {2}], {3}}, {',', {perm, {2}, {4}}, {insert, {4}, {1}, {3}}}}
		]).

%% append_3(Head, NextGoal, Choicepoints, Bindings, VarNum, Database) -> void.
%% append([], L, L).
%% append([H|T], L, [H|L1]) :- append(T, L, L1).
%%  Here we attempt to compile indexing in the first argument.
append_3({append, A1, L, A3}, Params = #param{next_goal = Next0, bindings = Bs0, choice = Cps,
	var_num = Vn, f_consulter = Fcon}) ->
	case erlog_core:deref(A1, Bs0) of
		[] ->          %Cannot backtrack
			erlog_core:unify_prove_body(L, A3, Params);
		[H | T] ->        %Cannot backtrack
			L1 = {Vn},
			Next1 = [{append, T, L, L1} | Next0],
			erlog_core:unify_prove_body(A3, [H | L1], Params#param{next_goal = Next1, var_num = Vn + 1});
		{_} = Var ->        %This can backtrack
			FailFun = fun(LCp, LCps, LDb) ->  %TODO db not needed
				fail_append_3(LCp, Params#param{choice = LCps, database = LDb, f_consulter = Fcon}, Var, L, A3)
			end,
			Cp = #cp{type = compiled, data = FailFun, next = Next0, bs = Bs0, vn = Vn},
			Bs1 = erlog_core:add_binding(Var, [], Bs0),
			erlog_core:unify_prove_body(L, A3, Params#param{choice = [Cp | Cps], bindings = Bs1});
		_ -> erlog_errors:fail(Params)      %Will fail here!
	end.

fail_append_3(#cp{next = Next0, bs = Bs0, vn = Vn}, Params, A1, L, A3) ->
	H = {Vn},
	T = {Vn + 1},
	L1 = {Vn + 2},
	Bs1 = erlog_core:add_binding(A1, [H | T], Bs0),    %A1 always a variable here.
	Next1 = [{append, T, L, L1} | Next0],
	erlog_core:unify_prove_body(A3, [H | L1], Params#param{next_goal = Next1, bindings = Bs1,
		var_num = Vn + 3}).

%% insert_3(Head, NextGoal, Choicepoints, Bindings, VarNum, Database) -> void.
%% insert(L, X, [X|L]).
%% insert([H|L], X, [H|L1]) :- insert(L, X, L1).
insert_3({insert, A1, A2, A3}, Params = #param{next_goal = Next, bindings = Bs, choice = Cps, var_num = Vn, f_consulter = Fcon}) ->
	FailFun = fun(LCp, LCps, LDb) ->  %TODO db not needed
		fail_insert_3(LCp, Params#param{choice = LCps, database = LDb, f_consulter = Fcon}, A1, A2, A3)
	end,
	Cp = #cp{type = compiled, data = FailFun, next = Next, bs = Bs, vn = Vn},
	erlog_core:unify_prove_body(A3, [A2 | A1], Params#param{choice = [Cp | Cps]}).

fail_insert_3(#cp{next = Next0, bs = Bs, vn = Vn}, Params, A1, X, A3) ->
	H = {Vn},
	L = {Vn + 1},
	L1 = {Vn + 2},
	Next1 = [{insert, L, X, L1} | Next0],
	erlog_core:unify_prove_body(A1, [H | L], A3, [H | L1], Params#param{next_goal = Next1, bindings = Bs, var_num = Vn + 3}).

%% member_2(Head, NextGoal, Choicepoints, Bindings, VarNum, Database) -> void.
%% member(X, [X|_]).
%% member(X, [_|T]) :- member(X, T).
member_2({member, A1, A2}, Param = #param{next_goal = Next, bindings = Bs, choice = Cps, var_num = Vn}) ->
	FailFun = fun(LCp, LCps, LDb) ->
		fail_member_2(LCp, Param#param{choice = LCps, database = LDb}, A1, A2)
	end,
	Cp = #cp{type = compiled, data = FailFun, next = Next, bs = Bs, vn = Vn},
	T = {Vn},
	erlog_core:unify_prove_body(A2, [A1 | T], Param#param{choice = [Cp | Cps], var_num = Vn + 1}).

fail_member_2(#cp{next = Next0, bs = Bs, vn = Vn}, Params, A1, A2) ->
	H = {Vn},
	T = {Vn + 1},
	Next1 = [{member, A1, T} | Next0],
	erlog_core:unify_prove_body(A2, [H | T], Params#param{next_goal = Next1, bindings = Bs, var_num = Vn + 2}).

%% memberchk_2(Head, NextGoal, Choicepoints, Bindings, VarNum, Database) -> void.
%% memberchk(X, [X|_]) :- !.
%% memberchk(X, [_|T]) :- member(X, T).
%%  We don't build the list and we never backtrack so we can be smart
%%  and match directly. Should we give a type error?
memberchk_2({memberchk, A1, A2}, Params = #param{next_goal = Next, bindings = Bs0}) ->
	case erlog_core:deref(A2, Bs0) of
		[H | T] ->
			case erlog_core:unify(A1, H, Bs0) of
				{succeed, Bs1} ->
					erlog_core:prove_body(Params#param{goal = Next, bindings = Bs1});
				fail ->
					memberchk_2({memberchk, A1, T}, Params)
			end;
		{_} -> erlog_errors:instantiation_error();
		_ -> erlog_errors:fail(Params)
	end.

%% reverse_2(Head, NextGoal, Choicepoints, Bindings, VarNum, Database) -> void.
%% reverse([], []).
%% reverse([H|L1], L) :- reverse(L1, L2), append(L2, [H], L).
%%  Here we attempt to compile indexing in the first argument.
reverse_2({reverse, A1, A2}, Params = #param{next_goal = Next0, bindings = Bs0, choice = Cps, var_num = Vn}) ->
	case erlog_core:deref(A1, Bs0) of
		[] ->
			erlog_core:unify_prove_body(A2, [], Params);
		[H | T] ->
			L = {Vn},
			L1 = A2,
			%% Naive straight expansion of body.
			%%Next1 = [{reverse,T,L},{append,L,[H],L1}|Next0],
			%%prove_body(Next1, Cps, Bs0, Vn+1, Db);
			%% Smarter direct calling of local function.
			Next1 = [{append, L, [H], L1} | Next0],
			reverse_2({reverse, T, L}, Params#param{next_goal = Next1, var_num = Vn + 1});
		{_} = Var ->
			FailFun = fun(LCp, LCps, LDb) ->  %TODO db not needed
				fail_reverse_2(LCp, Params#param{choice = LCps, database = LDb}, Var, A2)
			end,
			Cp = #cp{type = compiled, data = FailFun, next = Next0, bs = Bs0, vn = Vn},
			Bs1 = erlog_core:add_binding(Var, [], Bs0),
			erlog_core:unify_prove_body(A2, [], Params#param{choice = [Cp | Cps], bindings = Bs1});
		_ -> erlog_errors:fail(Params)      %Will fail here!
	end.

fail_reverse_2(#cp{next = Next, bs = Bs0, vn = Vn}, Params, A1, A2) ->
	H = {Vn},
	T = {Vn + 1},
	L1 = A2,
	L = {Vn + 2},
	Bs1 = erlog_core:add_binding(A1, [H | T], Bs0),
	%%Next1 = [{reverse,T,L},{apperse,L,[H],L1}|Next],
	%%prove_body(Next1, Cps, Bs1, Vn+3, Db).
	Next1 = [{append, L, [H], L1} | Next],
	reverse_2({reverse, T, L}, Params#param{next_goal = Next1, bindings = Bs1, var_num = Vn + 3}).

%% sort_2(Head, NextGoal, Choicepoints, Bindings, VarNum, Database) -> void.
%% sort(List, SortedList).
sort_2({sort, L0, S}, Param = #param{bindings = Bs}) ->
	%% This may throw an erlog error, we don't catch it here.
	L1 = lists:usort(erlog_core:dderef_list(L0, Bs)),
	erlog_core:unify_prove_body(S, L1, Param).