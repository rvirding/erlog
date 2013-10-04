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
-export([prove_goal/6]).

%%-compile(export_all).

-import(lists, [map/2,foldl/3]).

%% We use these a lot so we import them for cleaner code.
-import(erlog_int, [prove_body/5,unify_prove_body/7,unify_prove_body/9,fail/2,
		    add_binding/3,make_vars/2,
		    deref/2,dderef/2,dderef_list/2,unify/3,
		    term_instance/2,
		    add_built_in/2,add_compiled_proc/4,
		    asserta_clause/2,assertz_clause/2]).

%% load(Database) -> Database.
%%  Assert predicates into the database.

load(Db0) ->
    foldl(fun (Head, Db) -> add_built_in(Head, Db) end, Db0,
	  [
	   %% Term unification and comparison
	   {'=',2},
	   {'\\=',2},
	   {'@>',2},
	   {'@>=',2},
	   {'==',2},
	   {'\\==',2},
	   {'@<',2},
	   {'@=<',2},
	   %% Term creation and decomposition.
	   {arg,3},
	   {copy_term,2},
	   {functor,3},
	   {'=..',2},
	   %% Type testing.
	   {atom,1},
	   {atomic,1},
	   {compound,1},
	   {integer,1},
	   {float,1},
	   {number,1},
	   {nonvar,1},
	   {var,1},
	   %% Atom processing.
	   {atom_chars,2},
	   {atom_length,2},
	   %% Arithmetic evaluation and comparison
	   {'is',2},
	   {'>',2},
	   {'>=',2},
	   {'=:=',2},
	   {'=\\=',2},
	   {'<',2},
	   {'=<',2}
	   ]).

%% prove_goal(Goal, NextGoal, ChoicePoints, Bindings, VarNum, Database) ->
%%	{succeed,ChoicePoints,NewBindings,NewVarNum,NewDatabase} |
%%      {fail,NewDatabase}.
%% Prove one goal. We seldom return succeed here but usually go directly to
%% to NextGoal.

%% Term unification and comparison
prove_goal({'=',L,R}, Next, Cps, Bs, Vn, Db) ->
    unify_prove_body(L, R, Next, Cps, Bs, Vn, Db);
prove_goal({'\\=',L,R}, Next, Cps, Bs0, Vn, Db) ->
    case unify(L, R, Bs0) of
	{succeed,_Bs1} -> fail(Cps, Db);
	fail -> prove_body(Next, Cps, Bs0, Vn, Db)
    end;
prove_goal({'@>',L,R}, Next, Cps, Bs, Vn, Db) ->
    term_test_prove_body('>', L, R, Next, Cps, Bs, Vn, Db);
prove_goal({'@>=',L,R}, Next, Cps, Bs, Vn, Db) ->
    term_test_prove_body('>=', L, R, Next, Cps, Bs, Vn, Db);
prove_goal({'==',L,R}, Next, Cps, Bs, Vn, Db) ->
    term_test_prove_body('==', L, R, Next, Cps, Bs, Vn, Db);
prove_goal({'\\==',L,R}, Next, Cps, Bs, Vn, Db) ->
    term_test_prove_body('/=', L, R, Next, Cps, Bs, Vn, Db);
prove_goal({'@<',L,R}, Next, Cps, Bs, Vn, Db) ->
    term_test_prove_body('<', L, R, Next, Cps, Bs, Vn, Db);
prove_goal({'@=<',L,R}, Next, Cps, Bs, Vn, Db) ->
    term_test_prove_body('=<', L, R, Next, Cps, Bs, Vn, Db);
%% Term creation and decomposition.
prove_goal({arg,I,Ct,A}, Next, Cps, Bs, Vn, Db) ->
    prove_arg(deref(I, Bs), deref(Ct, Bs), A, Next, Cps, Bs, Vn, Db);
prove_goal({copy_term,T0,C}, Next, Cps, Bs, Vn0, Db) ->
    %% Use term_instance to create the copy, can ignore orddict it creates.
    {T,_Nbs,Vn1} = term_instance(dderef(T0, Bs), Vn0),
    unify_prove_body(T, C, Next, Cps, Bs, Vn1, Db);
prove_goal({functor,T,F,A}, Next, Cps, Bs, Vn, Db) ->
    prove_functor(dderef(T, Bs), F, A, Next, Cps, Bs, Vn, Db);
prove_goal({'=..',T,L}, Next, Cps, Bs, Vn, Db) ->
    prove_univ(dderef(T, Bs), L, Next, Cps, Bs, Vn, Db);
%% Type testing.
prove_goal({atom,T0}, Next, Cps, Bs, Vn, Db) ->
    case deref(T0, Bs) of
	T when is_atom(T) -> prove_body(Next, Cps, Bs, Vn, Db);
	_Other -> fail(Cps, Db)
    end;
prove_goal({atomic,T0}, Next, Cps, Bs, Vn, Db) ->
    case deref(T0, Bs) of
	T when ?IS_ATOMIC(T) -> prove_body(Next, Cps, Bs, Vn, Db);
	_Other -> fail(Cps, Db)
    end;
prove_goal({compound,T0}, Next, Cps, Bs, Vn, Db) ->
    case deref(T0, Bs) of
	T when ?IS_ATOMIC(T) -> fail(Cps, Db);
	_Other -> prove_body(Next, Cps, Bs, Vn, Db)
    end;
prove_goal({integer,T0}, Next, Cps, Bs, Vn, Db) ->
    case deref(T0, Bs) of
	T when is_integer(T) -> prove_body(Next, Cps, Bs, Vn, Db);
	_Other -> fail(Cps, Db)
    end;
prove_goal({float,T0}, Next, Cps, Bs, Vn, Db) ->
    case deref(T0, Bs) of
	T when is_float(T) -> prove_body(Next, Cps, Bs, Vn, Db);
	_Other -> fail(Cps, Db)
    end;
prove_goal({number,T0}, Next, Cps, Bs, Vn, Db) ->
    case deref(T0, Bs) of
	T when is_number(T) -> prove_body(Next, Cps, Bs, Vn, Db);
	_Other -> fail(Cps, Db)
    end;
prove_goal({nonvar,T0}, Next, Cps, Bs, Vn, Db) ->
    case deref(T0, Bs) of
	{_} -> fail(Cps, Db);
	_Other -> prove_body(Next, Cps, Bs, Vn, Db)
    end;
prove_goal({var,T0}, Next, Cps, Bs, Vn, Db) ->
    case deref(T0, Bs) of
	{_} -> prove_body(Next, Cps, Bs, Vn, Db);
	_Other -> fail(Cps, Db)
    end;
%% Atom processing.
prove_goal({atom_chars,A,L}, Next, Cps, Bs, Vn, Db) ->
    prove_atom_chars(A, L, Next, Cps, Bs, Vn, Db);
prove_goal({atom_length,A0,L0}, Next, Cps, Bs, Vn, Db) ->
    case dderef(A0, Bs) of
	A when is_atom(A) ->
	    Alen = length(atom_to_list(A)),	%No of chars in atom
	    case dderef(L0, Bs) of
		L when is_integer(L) ->
		    unify_prove_body (Alen, L, Next, Cps, Bs, Vn, Db);
		{_}=Var ->
		    unify_prove_body (Alen, Var, Next, Cps, Bs, Vn, Db);
		Other -> erlog_int:type_error(integer, Other, Db)
	    end;
	{_} -> erlog_int:instantiation_error(Db);
	Other -> erlog_int:type_error(atom, Other, Db)
    end;
%% Arithmetic evalution and comparison.
prove_goal({is,N,E0}, Next, Cps, Bs, Vn, Db) ->
    E = eval_arith(deref(E0, Bs), Bs, Db),
    unify_prove_body(N, E, Next, Cps, Bs, Vn, Db);
prove_goal({'>',L,R}, Next, Cps, Bs, Vn, Db) ->
    arith_test_prove_body('>', L, R, Next, Cps, Bs, Vn, Db);
prove_goal({'>=',L,R}, Next, Cps, Bs, Vn, Db) ->
    arith_test_prove_body('>=', L, R, Next, Cps, Bs, Vn, Db);
prove_goal({'=:=',L,R}, Next, Cps, Bs, Vn, Db) ->
    arith_test_prove_body('==', L, R, Next, Cps, Bs, Vn, Db);
prove_goal({'=\\=',L,R}, Next, Cps, Bs, Vn, Db) ->
    arith_test_prove_body('/=', L, R, Next, Cps, Bs, Vn, Db);
prove_goal({'<',L,R}, Next, Cps, Bs, Vn, Db) ->
    arith_test_prove_body('<', L, R, Next, Cps, Bs, Vn, Db);
prove_goal({'=<',L,R}, Next, Cps, Bs, Vn, Db) ->
    arith_test_prove_body('=<', L, R, Next, Cps, Bs, Vn, Db).

%% term_test_prove_body(Test, Left, Right, Next, ChoicePoints, Bindings, Varnum, Database) ->
%%      void.

term_test_prove_body(Test, L, R, Next, Cps, Bs, Vn, Db) ->
    case erlang:Test(dderef(L, Bs), dderef(R, Bs)) of
	true -> prove_body(Next, Cps, Bs, Vn, Db);
	false -> fail(Cps, Db)
    end.

%% prove_arg(Index, Term, Arg, Next, ChoicePoints, VarNum, Database) -> void.
%%  Prove the goal arg(I, Ct, Arg), Index and Term have been dereferenced.

prove_arg(I, [H|T], A, Next, Cps, Bs, Vn, Db) when is_integer(I) ->
    %% He, he, he!
    if  I == 1 -> unify_prove_body(H, A, Next, Cps, Bs, Vn, Db);
	I == 2 -> unify_prove_body(T, A, Next, Cps, Bs, Vn, Db);
	true -> {fail,Db}
    end;
prove_arg(I, Ct, A, Next, Cps, Bs, Vn, Db)
  when is_integer(I), tuple_size(Ct) >= 2 ->
    if  I > 1, I + 1 =< tuple_size(Ct) ->
	    unify_prove_body(element(I+1, Ct), A, Next, Cps, Bs, Vn, Db);
	true -> {fail,Db}
    end;
prove_arg(I, Ct, _, _, _, _, _, Db) ->
    %%Type failure just generates an error.
    if  not(is_integer(I)) -> erlog_int:type_error(integer, I, Db);
	true -> erlog_int:type_error(compound, Ct, Db)
    end.

%% prove_functor(Term, Functor, Arity, Next, ChoicePoints, Bindings, VarNum, Database) -> void.
%%  Prove the call functor(T, F, A), Term has been dereferenced.

prove_functor(T, F, A, Next, Cps, Bs, Vn, Db) when tuple_size(T) >= 2 ->
    unify_prove_body(F, element(1, T), A, tuple_size(T)-1, Next, Cps, Bs, Vn, Db);
prove_functor(T, F, A, Next, Cps, Bs, Vn, Db) when ?IS_ATOMIC(T) ->
    unify_prove_body(F, T, A, 0, Next, Cps, Bs, Vn, Db);
prove_functor([_|_], F, A, Next, Cps, Bs, Vn, Db) ->
    %% Just the top level here.
    unify_prove_body(F, '.', A, 2, Next, Cps, Bs, Vn, Db);
prove_functor({_}=Var, F0, A0, Next, Cps, Bs0, Vn0, Db) ->
    case {dderef(F0, Bs0),dderef(A0, Bs0)} of
	{'.',2} ->				%He, he, he!
	    Bs1 = add_binding(Var, [{Vn0}|{Vn0+1}], Bs0),
	    prove_body(Next, Cps, Bs1, Vn0+2, Db);
	{F1,0} when ?IS_ATOMIC(F1) ->
	    Bs1 = add_binding(Var, F1, Bs0),
	    prove_body(Next, Cps, Bs1, Vn0, Db);
	{F1,A1} when is_atom(F1), is_integer(A1), A1 > 0 ->
	    As = make_vars(A1, Vn0),
	    Bs1 = add_binding(Var, list_to_tuple([F1|As]), Bs0),
	    prove_body(Next, Cps, Bs1, Vn0+A1, Db); %!!!
	%% Now the error cases.
	{{_},_} -> erlog_int:instantiation_error(Db);
	{F1,A1} when is_atom(F1) -> erlog_int:type_error(integer, A1, Db);
	{F1,_} -> erlog_int:type_error(atom, F1, Db)
    end.

%% prove_univ(Term, List, Next, ChoicePoints, Bindings, VarNum, Database) -> void.
%%  Prove the goal Term =.. List, Term has already been dereferenced.

prove_univ(T, L, Next, Cps, Bs, Vn, Db) when tuple_size(T) >= 2 ->
    Es = tuple_to_list(T),
    unify_prove_body(Es, L, Next, Cps, Bs, Vn, Db);
prove_univ(T, L, Next, Cps, Bs, Vn, Db) when ?IS_ATOMIC(T) ->
    unify_prove_body([T], L, Next, Cps, Bs, Vn, Db);
prove_univ([Lh|Lt], L, Next, Cps, Bs, Vn, Db) ->
    %% He, he, he!
    unify_prove_body(['.',Lh,Lt], L, Next, Cps, Bs, Vn, Db);
prove_univ({_}=Var, L, Next, Cps, Bs0, Vn, Db) ->
    case dderef(L, Bs0) of
	['.',Lh,Lt] ->				%He, he, he!
	    Bs1 = add_binding(Var, [Lh|Lt], Bs0),
	    prove_body(Next, Cps, Bs1, Vn, Db);
	[A] when ?IS_ATOMIC(A) ->
	    Bs1 = add_binding(Var, A, Bs0),
	    prove_body(Next, Cps, Bs1, Vn, Db);
	[F|As] when is_atom(F), length(As) > 0 ->
	    Bs1 = add_binding(Var, list_to_tuple([F|As]), Bs0),
	    prove_body(Next, Cps, Bs1, Vn, Db);
	%% Now the error cases.
	[{_}|_] -> erlog_int:instantiation_error(Db);
	{_} -> erlog_int:instantiation_error(Db);
	Other -> erlog_int:type_error(list, Other, Db)
end.

%% prove_atom_chars(Atom, List, Next, ChoicePoints, Bindings, VarNum, Database) ->
%%	void.
%%  Prove the atom_chars(Atom, List).

prove_atom_chars(A, L, Next, Cps, Bs, Vn, Db) ->
    %% After a suggestion by Sean Cribbs.
    case dderef(A, Bs) of
        Atom when is_atom(Atom) ->
            AtomList = [ list_to_atom([C]) || C <- atom_to_list(Atom) ],
            unify_prove_body(L, AtomList, Next, Cps, Bs, Vn, Db);
        {_}=Var ->
            %% Error #3: List is neither a list nor a partial list.
            %% Handled in dderef_list/2.
            List = dderef_list(L, Bs),
            %% Error #1, #4: List is a list or partial list with an
            %% element which is a variable or not one char atom.
	    Fun = fun ({_}) -> erlog_int:instantiation_error(Db);
		      (Atom) ->
			  case is_atom(Atom) andalso atom_to_list(Atom) of
			      [C] -> C;
			      _ -> erlog_int:type_error(character, Atom, Db)
			  end
		  end,
	    Chars = lists:map(Fun, List),
	    Atom = list_to_atom(Chars),
	    unify_prove_body(Var, Atom, Next, Cps, Bs, Vn, Db);
	Other ->
	    %% Error #2: Atom is neither a variable nor an atom
	    erlog_int:type_error(atom, Other, Db)
    end.

%% arith_test_prove_body(Test, Left, Right, Next, ChoicePoints, Bindings, VarNum, Database) ->
%%	void.

arith_test_prove_body(Test, L, R, Next, Cps, Bs, Vn, Db) ->
    case erlang:Test(eval_arith(deref(L, Bs), Bs, Db),
		     eval_arith(deref(R, Bs), Bs, Db)) of
	true -> prove_body(Next, Cps, Bs, Vn, Db);
	false -> fail(Cps, Db)
    end.

%% eval_arith(ArithExpr, Bindings, Database) -> Number.
%%  Evaluate an arithmetic expression, include the database for
%%  errors.  Dereference each level as we go, might fail so save some
%%  work.  Must be called deferenced.

eval_arith({'+',A,B}, Bs, Db) ->
    eval_arith(deref(A, Bs), Bs, Db) + eval_arith(deref(B, Bs), Bs, Db);
eval_arith({'-',A,B}, Bs, Db) ->
    eval_arith(deref(A, Bs), Bs, Db) - eval_arith(deref(B, Bs), Bs, Db);
eval_arith({'*',A,B}, Bs, Db) ->
    eval_arith(deref(A, Bs), Bs, Db) * eval_arith(deref(B, Bs), Bs, Db);
eval_arith({'/',A,B}, Bs, Db) ->
    eval_arith(deref(A, Bs), Bs, Db) / eval_arith(deref(B, Bs), Bs, Db);
eval_arith({'**',A,B}, Bs, Db) ->
    math:pow(eval_arith(deref(A, Bs), Bs, Db),
	     eval_arith(deref(B, Bs), Bs, Db));
eval_arith({'//',A,B}, Bs, Db) ->
    eval_int(deref(A, Bs), Bs, Db) div eval_int(deref(B, Bs), Bs, Db);
eval_arith({'mod',A,B}, Bs, Db) ->
    eval_int(deref(A, Bs), Bs, Db) rem eval_int(deref(B, Bs), Bs, Db);
eval_arith({'/\\',A,B}, Bs, Db) ->
    eval_int(deref(A, Bs), Bs, Db) band eval_int(deref(B, Bs), Bs, Db);
eval_arith({'\\/',A,B}, Bs, Db) ->
    eval_int(deref(A, Bs), Bs, Db) bor eval_int(deref(B, Bs), Bs, Db);
eval_arith({'<<',A,B}, Bs, Db) ->
    eval_int(deref(A, Bs), Bs, Db) bsl eval_int(deref(B, Bs), Bs, Db);
eval_arith({'>>',A,B}, Bs, Db) ->
    eval_int(deref(A, Bs), Bs, Db) bsr eval_int(deref(B, Bs), Bs, Db);
eval_arith({'\\',A}, Bs, Db) ->
    bnot eval_int(deref(A, Bs), Bs, Db);
eval_arith({'+',A}, Bs, Db) ->
    + eval_arith(deref(A, Bs), Bs, Db);
eval_arith({'-',A}, Bs, Db) ->
    - eval_arith(deref(A, Bs), Bs, Db);
eval_arith({'abs',A}, Bs, Db) ->
    abs(eval_arith(deref(A, Bs), Bs, Db));
eval_arith({'float',A}, Bs, Db) ->
    float(eval_arith(deref(A, Bs), Bs, Db));
eval_arith({'truncate',A}, Bs, Db) ->
    trunc(eval_arith(deref(A, Bs), Bs, Db));
eval_arith(N, _Bs, _Db) when is_number(N) -> N;	%Just a number
%% Error cases.
eval_arith({_}, _Bs, Db) -> erlog_int:instantiation_error(Db);
eval_arith(N, _Bs, Db) when is_tuple(N) ->
    Pi = pred_ind(element(1, N), tuple_size(N)-1),
    erlog_int:type_error(evaluable, Pi, Db);
eval_arith([_|_], _Bs, Db) ->
    erlog_int:type_error(evaluable, pred_ind('.', 2), Db);
eval_arith(O, _Bs, Db) -> erlog_int:type_error(evaluable, O, Db).

%% eval_int(IntegerExpr, Bindings, Database) -> Integer.
%% Evaluate an integer expression, include the database for errors.

eval_int(E0, Bs, Db) ->
    E = eval_arith(E0, Bs, Db),
    if  is_integer(E) -> E;
	true -> erlog_int:type_error(integer, E, Db)
    end.

pred_ind(N, A) -> {'/',N,A}.
