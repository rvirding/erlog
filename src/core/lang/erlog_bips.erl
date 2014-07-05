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

-include("erlog_int.hrl").

%% Main interface functions.
-export([load/1]).
-export([prove_goal/7]).

%%-compile(export_all).

%% load(Database) -> Database.
%%  Assert predicates into the database.
load(Db) ->
	lists:foreach(fun(Head) -> erlog_memory:add_built_in(Db, Head) end, ?ERLOG_BIPS).

%% prove_goal(Goal, NextGoal, ChoicePoints, Bindings, VarNum, Database) ->
%%	{succeed,ChoicePoints,NewBindings,NewVarNum,NewDatabase} |
%%      {fail,NewDatabase}.
%% Prove one goal. We seldom return succeed here but usually go directly to
%% to NextGoal.

%% Term unification and comparison
prove_goal({'=', L, R}, Next, Cps, Bs, Vn, Db, Fcon) ->
	erlog_core:unify_prove_body(L, R, Next, Cps, Bs, Vn, Db, Fcon);
prove_goal({'\\=', L, R}, Next, Cps, Bs0, Vn, Db, Fcon) ->
	case erlog_core:unify(L, R, Bs0) of
		{succeed, _Bs1} -> erlog_errors:fail(Cps, Db, Fcon);
		fail -> erlog_core:prove_body(Next, Cps, Bs0, Vn, Db, Fcon)
	end;
prove_goal({'@>', L, R}, Next, Cps, Bs, Vn, Db, Fcon) ->
	term_test_prove_body('>', L, R, Next, Cps, Bs, Vn, Db, Fcon);
prove_goal({'@>=', L, R}, Next, Cps, Bs, Vn, Db, Fcon) ->
	term_test_prove_body('>=', L, R, Next, Cps, Bs, Vn, Db, Fcon);
prove_goal({'==', L, R}, Next, Cps, Bs, Vn, Db, Fcon) ->
	term_test_prove_body('==', L, R, Next, Cps, Bs, Vn, Db, Fcon);
prove_goal({'\\==', L, R}, Next, Cps, Bs, Vn, Db, Fcon) ->
	term_test_prove_body('/=', L, R, Next, Cps, Bs, Vn, Db, Fcon);
prove_goal({'@<', L, R}, Next, Cps, Bs, Vn, Db, Fcon) ->
	term_test_prove_body('<', L, R, Next, Cps, Bs, Vn, Db, Fcon);
prove_goal({'@=<', L, R}, Next, Cps, Bs, Vn, Db, Fcon) ->
	term_test_prove_body('=<', L, R, Next, Cps, Bs, Vn, Db, Fcon);
%% Term creation and decomposition.
prove_goal({arg, I, Ct, A}, Next, Cps, Bs, Vn, Db, Fcon) ->
	prove_arg(erlog_core:deref(I, Bs), erlog_core:deref(Ct, Bs), A, Next, Cps, Bs, Vn, Db, Fcon);
prove_goal({copy_term, T0, C}, Next, Cps, Bs, Vn0, Db, Fcon) ->
	%% Use term_instance to create the copy, can ignore orddict it creates.
	{T, _Nbs, Vn1} = erlog_core:term_instance(erlog_core:dderef(T0, Bs), Vn0),
	erlog_core:unify_prove_body(T, C, Next, Cps, Bs, Vn1, Db, Fcon);
prove_goal({functor, T, F, A}, Next, Cps, Bs, Vn, Db, Fcon) ->
	prove_functor(erlog_core:dderef(T, Bs), F, A, Next, Cps, Bs, Vn, Db, Fcon);
prove_goal({'=..', T, L}, Next, Cps, Bs, Vn, Db, Fcon) ->
	prove_univ(erlog_core:dderef(T, Bs), L, Next, Cps, Bs, Vn, Db, Fcon);
%% Type testing.
prove_goal({atom, T0}, Next, Cps, Bs, Vn, Db, Fcon) ->
	case erlog_core:deref(T0, Bs) of
		T when is_atom(T) -> erlog_core:prove_body(Next, Cps, Bs, Vn, Db, Fcon);
		_Other -> erlog_errors:fail(Cps, Db, Fcon)
	end;
prove_goal({atomic, T0}, Next, Cps, Bs, Vn, Db, Fcon) ->
	case erlog_core:deref(T0, Bs) of
		T when ?IS_ATOMIC(T) -> erlog_core:prove_body(Next, Cps, Bs, Vn, Db, Fcon);
		_Other -> erlog_errors:fail(Cps, Db, Fcon)
	end;
prove_goal({compound, T0}, Next, Cps, Bs, Vn, Db, Fcon) ->
	case erlog_core:deref(T0, Bs) of
		T when ?IS_ATOMIC(T) -> erlog_errors:fail(Cps, Db, Fcon);
		_Other -> erlog_core:prove_body(Next, Cps, Bs, Vn, Db, Fcon)
	end;
prove_goal({integer, T0}, Next, Cps, Bs, Vn, Db, Fcon) ->
	case erlog_core:deref(T0, Bs) of
		T when is_integer(T) -> erlog_core:prove_body(Next, Cps, Bs, Vn, Db, Fcon);
		_Other -> erlog_errors:fail(Cps, Db, Fcon)
	end;
prove_goal({float, T0}, Next, Cps, Bs, Vn, Db, Fcon) ->
	case erlog_core:deref(T0, Bs) of
		T when is_float(T) -> erlog_core:prove_body(Next, Cps, Bs, Vn, Db, Fcon);
		_Other -> erlog_errors:fail(Cps, Db, Fcon)
	end;
prove_goal({number, T0}, Next, Cps, Bs, Vn, Db, Fcon) ->
	case erlog_core:deref(T0, Bs) of
		T when is_number(T) -> erlog_core:prove_body(Next, Cps, Bs, Vn, Db, Fcon);
		_Other -> erlog_errors:fail(Cps, Db, Fcon)
	end;
prove_goal({nonvar, T0}, Next, Cps, Bs, Vn, Db, Fcon) ->
	case erlog_core:deref(T0, Bs) of
		{_} -> erlog_errors:fail(Cps, Db, Fcon);
		_Other -> erlog_core:prove_body(Next, Cps, Bs, Vn, Db, Fcon)
	end;
prove_goal({var, T0}, Next, Cps, Bs, Vn, Db, Fcon) ->
	case erlog_core:deref(T0, Bs) of
		{_} -> erlog_core:prove_body(Next, Cps, Bs, Vn, Db, Fcon);
		_Other -> erlog_errors:fail(Cps, Db, Fcon)
	end;
%% Atom processing.
prove_goal({atom_chars, A, L}, Next, Cps, Bs, Vn, Db, Fcon) ->
	prove_atom_chars(A, L, Next, Cps, Bs, Vn, Db, Fcon);
prove_goal({atom_length, A0, L0}, Next, Cps, Bs, Vn, Db, Fcon) ->
	case erlog_core:dderef(A0, Bs) of
		A when is_atom(A) ->
			Alen = length(atom_to_list(A)),  %No of chars in atom
			case erlog_core:dderef(L0, Bs) of
				L when is_integer(L) ->
					erlog_core:unify_prove_body(Alen, L, Next, Cps, Bs, Vn, Db, Fcon);
				{_} = Var ->
					erlog_core:unify_prove_body(Alen, Var, Next, Cps, Bs, Vn, Db, Fcon);
				Other -> erlog_errors:type_error(integer, Other, Db)
			end;
		{_} -> erlog_errors:instantiation_error(Db);
		Other -> erlog_errors:type_error(atom, Other, Db)
	end;
%% Arithmetic evalution and comparison.
prove_goal({is, N, E0}, Next, Cps, Bs, Vn, Db, Fcon) ->
	E = eval_arith(erlog_core:deref(E0, Bs), Bs, Db),
	erlog_core:unify_prove_body(N, E, Next, Cps, Bs, Vn, Db, Fcon);
prove_goal({'>', L, R}, Next, Cps, Bs, Vn, Db, Fcon) ->
	arith_test_prove_body('>', L, R, Next, Cps, Bs, Vn, Db, Fcon);
prove_goal({'>=', L, R}, Next, Cps, Bs, Vn, Db, Fcon) ->
	arith_test_prove_body('>=', L, R, Next, Cps, Bs, Vn, Db, Fcon);
prove_goal({'=:=', L, R}, Next, Cps, Bs, Vn, Db, Fcon) ->
	arith_test_prove_body('==', L, R, Next, Cps, Bs, Vn, Db, Fcon);
prove_goal({'=\\=', L, R}, Next, Cps, Bs, Vn, Db, Fcon) ->
	arith_test_prove_body('/=', L, R, Next, Cps, Bs, Vn, Db, Fcon);
prove_goal({'<', L, R}, Next, Cps, Bs, Vn, Db, Fcon) ->
	arith_test_prove_body('<', L, R, Next, Cps, Bs, Vn, Db, Fcon);
prove_goal({'=<', L, R}, Next, Cps, Bs, Vn, Db, Fcon) ->
	arith_test_prove_body('=<', L, R, Next, Cps, Bs, Vn, Db, Fcon).

%% term_test_prove_body(Test, Left, Right, Next, ChoicePoints, Bindings, Varnum, Database) ->
%%      void.

term_test_prove_body(Test, L, R, Next, Cps, Bs, Vn, Db, Fcon) ->
	case erlang:Test(erlog_core:dderef(L, Bs), erlog_core:dderef(R, Bs)) of
		true -> erlog_core:prove_body(Next, Cps, Bs, Vn, Db, Fcon);
		false -> erlog_errors:fail(Cps, Db, Fcon)
	end.

%% prove_arg(Index, Term, Arg, Next, ChoicePoints, VarNum, Database) -> void.
%%  Prove the goal arg(I, Ct, Arg), Index and Term have been dereferenced.

prove_arg(I, [H | T], A, Next, Cps, Bs, Vn, Db, Fcon) when is_integer(I) ->
	%% He, he, he!
	if I == 1 -> erlog_core:unify_prove_body(H, A, Next, Cps, Bs, Vn, Db, Fcon);
		I == 2 -> erlog_core:unify_prove_body(T, A, Next, Cps, Bs, Vn, Db, Fcon);
		true -> {fail, Db}
	end;
prove_arg(I, Ct, A, Next, Cps, Bs, Vn, Db, Fcon)
	when is_integer(I), tuple_size(Ct) >= 2 ->
	if I > 1, I + 1 =< tuple_size(Ct) ->
		erlog_core:unify_prove_body(element(I + 1, Ct), A, Next, Cps, Bs, Vn, Db, Fcon);
		true -> {fail, Db}
	end;
prove_arg(I, Ct, _, _, _, _, _, Db, _) ->
	%%Type failure just generates an error.
	if not(is_integer(I)) -> erlog_errors:type_error(integer, I, Db);
		true -> erlog_errors:type_error(compound, Ct, Db)
	end.

%% prove_functor(Term, Functor, Arity, Next, ChoicePoints, Bindings, VarNum, Database) -> void.
%%  Prove the call functor(T, F, A), Term has been dereferenced.

prove_functor(T, F, A, Next, Cps, Bs, Vn, Db, Fcon) when tuple_size(T) >= 2 ->
	erlog_core:unify_prove_body(F, element(1, T), A, tuple_size(T) - 1, Next, Cps, Bs, Vn, Db, Fcon);
prove_functor(T, F, A, Next, Cps, Bs, Vn, Db, Fcon) when ?IS_ATOMIC(T) ->
	erlog_core:unify_prove_body(F, T, A, 0, Next, Cps, Bs, Vn, Db, Fcon);
prove_functor([_ | _], F, A, Next, Cps, Bs, Vn, Db, Fcon) ->
	%% Just the top level here.
	erlog_core:unify_prove_body(F, '.', A, 2, Next, Cps, Bs, Vn, Db, Fcon);
prove_functor({_} = Var, F0, A0, Next, Cps, Bs0, Vn0, Db, Fcon) ->
	case {erlog_core:dderef(F0, Bs0), erlog_core:dderef(A0, Bs0)} of
		{'.', 2} ->        %He, he, he!
			Bs1 = erlog_core:add_binding(Var, [{Vn0} | {Vn0 + 1}], Bs0),
			erlog_core:prove_body(Next, Cps, Bs1, Vn0 + 2, Db, Fcon);
		{F1, 0} when ?IS_ATOMIC(F1) ->
			Bs1 = erlog_core:add_binding(Var, F1, Bs0),
			erlog_core:prove_body(Next, Cps, Bs1, Vn0, Db, Fcon);
		{F1, A1} when is_atom(F1), is_integer(A1), A1 > 0 ->
			As = erlog_core:make_vars(A1, Vn0),
			Bs1 = erlog_core:add_binding(Var, list_to_tuple([F1 | As]), Bs0),
			erlog_core:prove_body(Next, Cps, Bs1, Vn0 + A1, Db, Fcon); %!!!
	%% Now the error cases.
		{{_}, _} -> erlog_errors:instantiation_error(Db);
		{F1, A1} when is_atom(F1) -> erlog_errors:type_error(integer, A1, Db);
		{F1, _} -> erlog_errors:type_error(atom, F1, Db)
	end.

%% prove_univ(Term, List, Next, ChoicePoints, Bindings, VarNum, Database) -> void.
%%  Prove the goal Term =.. List, Term has already been dereferenced.

prove_univ(T, L, Next, Cps, Bs, Vn, Db, Fcon) when tuple_size(T) >= 2 ->
	Es = tuple_to_list(T),
	erlog_core:unify_prove_body(Es, L, Next, Cps, Bs, Vn, Db, Fcon);
prove_univ(T, L, Next, Cps, Bs, Vn, Db, Fcon) when ?IS_ATOMIC(T) ->
	erlog_core:unify_prove_body([T], L, Next, Cps, Bs, Vn, Db, Fcon);
prove_univ([Lh | Lt], L, Next, Cps, Bs, Vn, Db, Fcon) ->
	%% He, he, he!
	erlog_core:unify_prove_body(['.', Lh, Lt], L, Next, Cps, Bs, Vn, Db, Fcon);
prove_univ({_} = Var, L, Next, Cps, Bs0, Vn, Db, Fcon) ->
	case erlog_core:dderef(L, Bs0) of
		['.', Lh, Lt] ->        %He, he, he!
			Bs1 = erlog_core:add_binding(Var, [Lh | Lt], Bs0),
			erlog_core:prove_body(Next, Cps, Bs1, Vn, Db, Fcon);
		[A] when ?IS_ATOMIC(A) ->
			Bs1 = erlog_core:add_binding(Var, A, Bs0),
			erlog_core:prove_body(Next, Cps, Bs1, Vn, Db, Fcon);
		[F | As] when is_atom(F), length(As) > 0 ->
			Bs1 = erlog_core:add_binding(Var, list_to_tuple([F | As]), Bs0),
			erlog_core:prove_body(Next, Cps, Bs1, Vn, Db, Fcon);
	%% Now the error cases.
		[{_} | _] -> erlog_errors:instantiation_error(Db);
		{_} -> erlog_errors:instantiation_error(Db);
		Other -> erlog_errors:type_error(list, Other, Db)
	end.

%% prove_atom_chars(Atom, List, Next, ChoicePoints, Bindings, VarNum, Database) ->
%%	void.
%%  Prove the atom_chars(Atom, List).

prove_atom_chars(A, L, Next, Cps, Bs, Vn, Db, Fcon) ->
	%% After a suggestion by Sean Cribbs.
	case erlog_core:dderef(A, Bs) of
		Atom when is_atom(Atom) ->
			AtomList = [list_to_atom([C]) || C <- atom_to_list(Atom)],
			erlog_core:unify_prove_body(L, AtomList, Next, Cps, Bs, Vn, Db, Fcon);
		{_} = Var ->
			%% Error #3: List is neither a list nor a partial list.
			%% Handled in dderef_list/2.
			List = erlog_core:dderef_list(L, Bs),
			%% Error #1, #4: List is a list or partial list with an
			%% element which is a variable or not one char atom.
			Fun = fun({_}) -> erlog_errors:instantiation_error(Db);
				(Atom) ->
					case is_atom(Atom) andalso atom_to_list(Atom) of
						[C] -> C;
						_ -> erlog_errors:type_error(character, Atom, Db)
					end
			end,
			Chars = lists:map(Fun, List),
			Atom = list_to_atom(Chars),
			erlog_core:unify_prove_body(Var, Atom, Next, Cps, Bs, Vn, Db, Fcon);
		Other ->
			%% Error #2: Atom is neither a variable nor an atom
			erlog_errors:type_error(atom, Other, Db)
	end.

%% arith_test_prove_body(Test, Left, Right, Next, ChoicePoints, Bindings, VarNum, Database) ->
%%	void.

arith_test_prove_body(Test, L, R, Next, Cps, Bs, Vn, Db, Fcon) ->
	case erlang:Test(eval_arith(erlog_core:deref(L, Bs), Bs, Db),
		eval_arith(erlog_core:deref(R, Bs), Bs, Db)) of
		true -> erlog_core:prove_body(Next, Cps, Bs, Vn, Db, Fcon);
		false -> erlog_errors:fail(Cps, Db, Fcon)
	end.

%% eval_arith(ArithExpr, Bindings, Database) -> Number.
%%  Evaluate an arithmetic expression, include the database for
%%  errors.  Dereference each level as we go, might fail so save some
%%  work.  Must be called deferenced.

eval_arith({'+', A, B}, Bs, Db) ->
	eval_arith(erlog_core:deref(A, Bs), Bs, Db) + eval_arith(erlog_core:deref(B, Bs), Bs, Db);
eval_arith({'-', A, B}, Bs, Db) ->
	eval_arith(erlog_core:deref(A, Bs), Bs, Db) - eval_arith(erlog_core:deref(B, Bs), Bs, Db);
eval_arith({'*', A, B}, Bs, Db) ->
	eval_arith(erlog_core:deref(A, Bs), Bs, Db) * eval_arith(erlog_core:deref(B, Bs), Bs, Db);
eval_arith({'/', A, B}, Bs, Db) ->
	eval_arith(erlog_core:deref(A, Bs), Bs, Db) / eval_arith(erlog_core:deref(B, Bs), Bs, Db);
eval_arith({'**', A, B}, Bs, Db) ->
	math:pow(eval_arith(erlog_core:deref(A, Bs), Bs, Db),
		eval_arith(erlog_core:deref(B, Bs), Bs, Db));
eval_arith({'//', A, B}, Bs, Db) ->
	eval_int(erlog_core:deref(A, Bs), Bs, Db) div eval_int(erlog_core:deref(B, Bs), Bs, Db);
eval_arith({'mod', A, B}, Bs, Db) ->
	eval_int(erlog_core:deref(A, Bs), Bs, Db) rem eval_int(erlog_core:deref(B, Bs), Bs, Db);
eval_arith({'/\\', A, B}, Bs, Db) ->
	eval_int(erlog_core:deref(A, Bs), Bs, Db) band eval_int(erlog_core:deref(B, Bs), Bs, Db);
eval_arith({'\\/', A, B}, Bs, Db) ->
	eval_int(erlog_core:deref(A, Bs), Bs, Db) bor eval_int(erlog_core:deref(B, Bs), Bs, Db);
eval_arith({'<<', A, B}, Bs, Db) ->
	eval_int(erlog_core:deref(A, Bs), Bs, Db) bsl eval_int(erlog_core:deref(B, Bs), Bs, Db);
eval_arith({'>>', A, B}, Bs, Db) ->
	eval_int(erlog_core:deref(A, Bs), Bs, Db) bsr eval_int(erlog_core:deref(B, Bs), Bs, Db);
eval_arith({'\\', A}, Bs, Db) ->
	bnot eval_int(erlog_core:deref(A, Bs), Bs, Db);
eval_arith({'+', A}, Bs, Db) ->
	+ eval_arith(erlog_core:deref(A, Bs), Bs, Db);
eval_arith({'-', A}, Bs, Db) ->
	- eval_arith(erlog_core:deref(A, Bs), Bs, Db);
eval_arith({'abs', A}, Bs, Db) ->
	abs(eval_arith(erlog_core:deref(A, Bs), Bs, Db));
eval_arith({'float', A}, Bs, Db) ->
	float(eval_arith(erlog_core:deref(A, Bs), Bs, Db));
eval_arith({'truncate', A}, Bs, Db) ->
	trunc(eval_arith(erlog_core:deref(A, Bs), Bs, Db));
eval_arith(N, _Bs, _Db) when is_number(N) -> N;  %Just a number
%% Error cases.
eval_arith({_}, _Bs, Db) -> erlog_errors:instantiation_error(Db);
eval_arith(N, _Bs, Db) when is_tuple(N) ->
	Pi = pred_ind(element(1, N), tuple_size(N) - 1),
	erlog_errors:type_error(evaluable, Pi, Db);
eval_arith([_ | _], _Bs, Db) ->
	erlog_errors:type_error(evaluable, pred_ind('.', 2), Db);
eval_arith(O, _Bs, Db) -> erlog_errors:type_error(evaluable, O, Db).

%% eval_int(IntegerExpr, Bindings, Database) -> Integer.
%% Evaluate an integer expression, include the database for errors.

eval_int(E0, Bs, Db) ->
	E = eval_arith(E0, Bs, Db),
	if is_integer(E) -> E;
		true -> erlog_errors:type_error(integer, E, Db)
	end.

pred_ind(N, A) -> {'/', N, A}.