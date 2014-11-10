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

%% File    : erlog_bips.erl
%% Author  : Robert Virding
%% Purpose : Built-in predicates of Erlog interpreter.
%% 
%% These are the built-in predicates of the Prolog interpreter which
%% are not control predicates or database predicates.

-module(erlog_bips).

-behaviour(erlog_stdlib).

-include("erlog_core.hrl").
-include("erlog_bips.hrl").

%% Main interface functions.
-export([load/1]).
-export([prove_goal/1]).

%% load(Database) -> Database.
%%  Assert predicates into the database.
load(Db) ->
	lists:foreach(fun(Head) -> erlog_memory:load_kernel_space(Db, ?MODULE, Head) end, ?ERLOG_BIPS).

%% prove_goal(Goal, NextGoal, ChoicePoints, Bindings, VarNum, Database) ->
%%	{succeed,ChoicePoints,NewBindings,NewVarNum,NewDatabase} |
%%      {fail,NewDatabase}.
%% Prove one goal. We seldom return succeed here but usually go directly to
%% to NextGoal.

%% Term unification and comparison
prove_goal(Params = #param{goal = {'=', L, R}}) ->
	erlog_ec_body:unify_prove_body(L, R, Params);
prove_goal(Params = #param{goal = {'\\=', L, R}, next_goal = Next, bindings = Bs0}) ->
	case erlog_ec_unify:unify(L, R, Bs0) of
		{succeed, _Bs1} -> erlog_errors:fail(Params);
		fail -> erlog_ec_core:prove_body(Params#param{goal = Next})
	end;
prove_goal(Params = #param{goal = {'@>', L, R}}) ->
	erlog_eb_logic:term_test_prove_body('>', L, R, Params);
prove_goal(Params = #param{goal = {'@>=', L, R}}) ->
	erlog_eb_logic:term_test_prove_body('>=', L, R, Params);
prove_goal(Params = #param{goal = {'==', L, R}}) ->
	erlog_eb_logic:term_test_prove_body('==', L, R, Params);
prove_goal(Params = #param{goal = {'\\==', L, R}}) ->
	erlog_eb_logic:term_test_prove_body('/=', L, R, Params);
prove_goal(Params = #param{goal = {'@<', L, R}}) ->
	erlog_eb_logic:term_test_prove_body('<', L, R, Params);
prove_goal(Params = #param{goal = {'@=<', L, R}}) ->
	erlog_eb_logic:term_test_prove_body('=<', L, R, Params);
%% Term creation and decomposition.
prove_goal(Params = #param{goal = {arg, I, Ct, A}, bindings = Bs}) ->
	erlog_eb_logic:prove_arg(erlog_ec_support:deref(I, Bs), erlog_ec_support:deref(Ct, Bs), A, Params);
prove_goal(Params = #param{goal = {copy_term, T0, C}, bindings = Bs, var_num = Vn0}) ->
	%% Use term_instance to create the copy, can ignore orddict it creates.
	{T, _Nbs, Vn1} = erlog_ec_term:term_instance(erlog_ec_support:dderef(T0, Bs), Vn0),
	erlog_ec_body:unify_prove_body(T, C, Params#param{var_num = Vn1});
prove_goal(Params = #param{goal = {functor, T, F, A}, bindings = Bs}) ->
	erlog_eb_logic:prove_functor(erlog_ec_support:dderef(T, Bs), F, A, Params);
prove_goal(Params = #param{goal = {'=..', T, L}, bindings = Bs}) ->
	erlog_eb_logic:prove_univ(erlog_ec_support:dderef(T, Bs), L, Params);
%% Type testing.
prove_goal(Params = #param{goal = {atom, T0}, next_goal = Next, bindings = Bs}) ->
	case erlog_ec_support:deref(T0, Bs) of
		T when is_atom(T) -> erlog_ec_core:prove_body(Params#param{goal = Next});
		_Other -> erlog_errors:fail(Params)
	end;
prove_goal(Params = #param{goal = {atomic, T0}, next_goal = Next, bindings = Bs}) ->
	case erlog_ec_support:deref(T0, Bs) of
		T when ?IS_ATOMIC(T) -> erlog_ec_core:prove_body(Params#param{goal = Next});
		_Other -> erlog_errors:fail(Params)
	end;
prove_goal(Params = #param{goal = {compound, T0}, next_goal = Next, bindings = Bs}) ->
	case erlog_ec_support:deref(T0, Bs) of
		T when ?IS_ATOMIC(T) -> erlog_errors:fail(Params);
		_Other -> erlog_ec_core:prove_body(Params#param{goal = Next})
	end;
prove_goal(Params = #param{goal = {integer, T0}, next_goal = Next, bindings = Bs}) ->
	case erlog_ec_support:deref(T0, Bs) of
		T when is_integer(T) -> erlog_ec_core:prove_body(Params#param{goal = Next});
		_Other -> erlog_errors:fail(Params)
	end;
prove_goal(Params = #param{goal = {float, T0}, next_goal = Next, bindings = Bs}) ->
	case erlog_ec_support:deref(T0, Bs) of
		T when is_float(T) -> erlog_ec_core:prove_body(Params#param{goal = Next});
		_Other -> erlog_errors:fail(Params)
	end;
prove_goal(Params = #param{goal = {number, T0}, next_goal = Next, bindings = Bs}) ->
	case erlog_ec_support:deref(T0, Bs) of
		T when is_number(T) -> erlog_ec_core:prove_body(Params#param{goal = Next});
		_Other -> erlog_errors:fail(Params)
	end;
prove_goal(Params = #param{goal = {nonvar, T0}, next_goal = Next, bindings = Bs}) ->
	case erlog_ec_support:deref(T0, Bs) of
		{_} -> erlog_errors:fail(Params);
		_Other -> erlog_ec_core:prove_body(Params#param{goal = Next})
	end;
prove_goal(Params = #param{goal = {var, T0}, next_goal = Next, bindings = Bs}) ->
	case erlog_ec_support:deref(T0, Bs) of
		{_} -> erlog_ec_core:prove_body(Params#param{goal = Next});
		_Other -> erlog_errors:fail(Params)
	end;
%% Atom processing.
prove_goal(Params = #param{goal = {atom_chars, A, L}}) ->
	erlog_eb_logic:prove_atom_chars(A, L, Params);
prove_goal(Params = #param{goal = {atom_length, A0, L0}, bindings = Bs, database = Db}) ->
	case erlog_ec_support:dderef(A0, Bs) of
		A when is_atom(A) ->
			Alen = length(atom_to_list(A)),  %No of chars in atom
			case erlog_ec_support:dderef(L0, Bs) of
				L when is_integer(L) ->
					erlog_ec_body:unify_prove_body(Alen, L, Params);
				{_} = Var ->
					erlog_ec_body:unify_prove_body(Alen, Var, Params);
				Other -> erlog_errors:type_error(integer, Other, Db)
			end;
		{_} -> erlog_errors:instantiation_error(Db);
		Other -> erlog_errors:type_error(atom, Other, Db)
	end;
%% Arithmetic evalution and comparison.
prove_goal(Params = #param{goal = {is, N, E0}, bindings = Bs, database = Db}) ->
	E = erlog_eb_logic:eval_arith(erlog_ec_support:deref(E0, Bs), Bs, Db),
	erlog_ec_body:unify_prove_body(N, E, Params);
prove_goal(Params = #param{goal = {'>', L, R}}) ->
	erlog_eb_logic:arith_test_prove_body('>', L, R, Params);
prove_goal(Params = #param{goal = {'>=', L, R}}) ->
	erlog_eb_logic:arith_test_prove_body('>=', L, R, Params);
prove_goal(Params = #param{goal = {'=:=', L, R}}) ->
	erlog_eb_logic:arith_test_prove_body('==', L, R, Params);
prove_goal(Params = #param{goal = {'=\\=', L, R}}) ->
	erlog_eb_logic:arith_test_prove_body('/=', L, R, Params);
prove_goal(Params = #param{goal = {'<', L, R}}) ->
	erlog_eb_logic:arith_test_prove_body('<', L, R, Params);
prove_goal(Params = #param{goal = {'=<', L, R}}) ->
	erlog_eb_logic:arith_test_prove_body('=<', L, R, Params).