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
-export([prove_goal/3]).

%%-compile(export_all).

-import(lists, [map/2,foldl/3]).

%% We use these a lot so we import them for cleaner code.
-import(erlog_int, [prove_body/2,unify_prove_body/4,unify_prove_body/6,fail/1,
		    get_binding/2,add_binding/3,make_var_list/2,
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
	   {term_variables,2},
	   {term_variables,3},
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
	   {atom_codes,2},
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

%% prove_goal(Goal, NextGoal, State) ->
%%	{succeed,State} |
%%      {fail,State}.
%% Prove one goal. We seldom return succeed here but usually go directly to
%% to NextGoal.

%% Term unification and comparison
prove_goal({'=',L,R}, Next, St) ->
    unify_prove_body(L, R, Next, St);
prove_goal({'\\=',L,R}, Next, #est{bs=Bs}=St) ->
    case unify(L, R, Bs) of
	{succeed,_} -> fail(St);
	fail -> prove_body(Next, St)
    end;
prove_goal({'@>',L,R}, Next, St) ->
    term_test_prove_body('>', L, R, Next, St);
prove_goal({'@>=',L,R}, Next, St) ->
    term_test_prove_body('>=', L, R, Next, St);
prove_goal({'==',L,R}, Next, St) ->
    term_test_prove_body('==', L, R, Next, St);
prove_goal({'\\==',L,R}, Next, St) ->
    term_test_prove_body('/=', L, R, Next, St);
prove_goal({'@<',L,R}, Next, St) ->
    term_test_prove_body('<', L, R, Next, St);
prove_goal({'@=<',L,R}, Next, St) ->
    term_test_prove_body('=<', L, R, Next, St);
%% Term creation and decomposition.
prove_goal({arg,I,Term,A}, Next, #est{bs=Bs}=St) ->
    prove_arg(deref(I, Bs), deref(Term, Bs), A, Next, St);
prove_goal({copy_term,T0,C}, Next, #est{bs=Bs,vn=Vn0}=St) ->
    %% Use term_instance to create the copy, can ignore orddict it creates.
    {T,_Nbs,Vn1} = term_instance(dderef(T0, Bs), Vn0),
    unify_prove_body(T, C, Next, St#est{vn=Vn1});
prove_goal({functor,T,F,A}, Next, #est{bs=Bs}=St) ->
    prove_functor(deref(T, Bs), F, A, Next, St);
prove_goal({term_variables,Term,List}, Next, St) ->
    prove_body([{term_variables,Term,List,[]}|Next], St);
prove_goal({term_variables,Term,List,Tail}, Next, St) ->
    prove_term_variables(Term, List, Tail, Next, St);
prove_goal({'=..',T,L}, Next, #est{bs=Bs}=St) ->
    prove_univ(dderef(T, Bs), L, Next, St);
%% Type testing.
prove_goal({atom,T0}, Next, #est{bs=Bs}=St) ->
    case deref(T0, Bs) of
	T when is_atom(T) -> prove_body(Next, St);
	_Other -> fail(St)
    end;
prove_goal({atomic,T0}, Next, #est{bs=Bs}=St) ->
    case deref(T0, Bs) of
	T when ?IS_ATOMIC(T) -> prove_body(Next, St);
	_Other -> fail(St)
    end;
prove_goal({compound,T0}, Next, #est{bs=Bs}=St) ->
    case deref(T0, Bs) of
	T when ?IS_ATOMIC(T) -> fail(St);
	_Other -> prove_body(Next, St)
    end;
prove_goal({integer,T0}, Next, #est{bs=Bs}=St) ->
    case deref(T0, Bs) of
	T when is_integer(T) -> prove_body(Next, St);
	_Other -> fail(St)
    end;
prove_goal({float,T0}, Next, #est{bs=Bs}=St) ->
    case deref(T0, Bs) of
	T when is_float(T) -> prove_body(Next, St);
	_Other -> fail(St)
    end;
prove_goal({number,T0}, Next, #est{bs=Bs}=St) ->
    case deref(T0, Bs) of
	T when is_number(T) -> prove_body(Next, St);
	_Other -> fail(St)
    end;
prove_goal({nonvar,T0}, Next, #est{bs=Bs}=St) ->
    case deref(T0, Bs) of
	{_} -> fail(St);
	_Other -> prove_body(Next, St)
    end;
prove_goal({var,T0}, Next, #est{bs=Bs}=St) ->
    case deref(T0, Bs) of
	{_} -> prove_body(Next, St);
	_Other -> fail(St)
    end;
%% Atom processing.
prove_goal({atom_chars,A,L}, Next, St) ->
    prove_atom_chars(A, L, Next, St);
prove_goal({atom_codes,A,L}, Next, St) ->
    prove_atom_codes(A, L, Next, St);
prove_goal({atom_length,A0,L0}, Next, #est{bs=Bs}=St) ->
    case deref(A0, Bs) of
	A when is_atom(A) ->
	    Alen = length(atom_to_list(A)),	%No of chars in atom
	    case dderef(L0, Bs) of
		L when is_integer(L) ->
		    unify_prove_body (Alen, L, Next, St);
		{_}=Var ->
		    unify_prove_body (Alen, Var, Next, St);
		Other -> erlog_int:type_error(integer, Other, St)
	    end;
	{_} -> erlog_int:instantiation_error(St);
	Other -> erlog_int:type_error(atom, Other, St)
    end;
%% Arithmetic evalution and comparison.
prove_goal({is,N,E0}, Next, #est{bs=Bs}=St) ->
    E = eval_arith(deref(E0, Bs), Bs, St),
    unify_prove_body(N, E, Next, St);
prove_goal({'>',L,R}, Next, St) ->
    arith_test_prove_body('>', L, R, Next, St);
prove_goal({'>=',L,R}, Next, St) ->
    arith_test_prove_body('>=', L, R, Next, St);
prove_goal({'=:=',L,R}, Next, St) ->
    arith_test_prove_body('==', L, R, Next, St);
prove_goal({'=\\=',L,R}, Next, St) ->
    arith_test_prove_body('/=', L, R, Next, St);
prove_goal({'<',L,R}, Next, St) ->
    arith_test_prove_body('<', L, R, Next, St);
prove_goal({'=<',L,R}, Next, St) ->
    arith_test_prove_body('=<', L, R, Next, St).

%% term_test_prove_body(Test, Left, Right, Next, State) ->
%%     void.

term_test_prove_body(Test, L, R, Next, #est{bs=Bs}=St) ->
    case erlang:Test(dderef(L, Bs), dderef(R, Bs)) of
	true -> prove_body(Next, St);
	false -> fail(St)
    end.

%% prove_arg(Index, Term, Arg, Next, St) -> void.
%%  Prove the goal arg(I, Ct, Arg). Index and Term have been dereferenced.

prove_arg(I, T, A, Next, St) when is_integer(I) ->
    prove_arg_int(I, T, A, Next, St);
prove_arg({_}=I, T, A, Next, St) ->
    prove_arg_var(I, T, A, Next, St);
prove_arg(I, _, _, _, St) ->
    erlog_int:type_error(integer, I, St).

prove_arg_int(I, [H|T], A, Next, St) ->
    %% He, he, he!
    if  I == 1 -> unify_prove_body(A, H, Next, St);
	I == 2 -> unify_prove_body(A, T, Next, St);
	true -> fail(St)
    end;
prove_arg_int(I, Ct, A, Next, St) when tuple_size(Ct) >= 2 ->
    if I >= 1, I =< tuple_size(Ct) - 1 ->
	    Arg = element(I+1, Ct),
	    unify_prove_body(A, Arg, Next, St);
       true -> fail(St)
    end;
prove_arg_int(_, Ct, _, _, St) ->
    erlog_int:type_error(compound, Ct, St).

prove_arg_var(I, [H|T], A, Next, St) ->
    %% He, he, he!
    prove_arg_list(I, 1, [H,T], A, Next, St);
prove_arg_var(I, Ct, A, Next, St) when tuple_size(Ct) >= 2 ->
    Args = tl(tuple_to_list(Ct)),
    prove_arg_list(I, 1, Args, A, Next, St);
prove_arg_var(_, Ct, _, _, St) ->
    erlog_int:type_error(compound, Ct, St).

prove_arg_list(V, I, [H], A, Next, #est{bs=Bs0}=St) ->
    Bs1 = add_binding(V, I, Bs0),
    unify_prove_body(A, H, Next, St#est{bs=Bs1});
prove_arg_list(V, I, [H|T], A, Next, #est{cps=Cps,bs=Bs0,vn=Vn}=St) ->
    FailFun = fun (Lcp, Lcps, Lst) ->
		      fail_arg_3(Lcp, Lcps, Lst, V, I+1, T, A)
	      end,
    Cp = #cp{type=compiled,data=FailFun,next=Next,bs=Bs0,vn=Vn},
    Bs1 = add_binding(V, I, Bs0),
    unify_prove_body(A, H, Next, St#est{cps=[Cp|Cps],bs=Bs1}).

fail_arg_3(#cp{next=Next,bs=Bs,vn=Vn}, Cps, St, V, I, List, A) ->
    prove_arg_list(V, I, List, A, Next, St#est{cps=Cps,bs=Bs,vn=Vn}).

%% prove_functor(Term, Functor, Arity, Next, State) -> void.
%%  Prove the call functor(T, F, A), Term has been dereferenced.

prove_functor(T, F, A, Next, St) when tuple_size(T) >= 2 ->
    unify_prove_body(F, element(1, T), A, tuple_size(T)-1, Next, St);
prove_functor(T, F, A, Next, St) when ?IS_ATOMIC(T) ->
    unify_prove_body(F, T, A, 0, Next, St);
prove_functor([_|_], F, A, Next, St) ->
    %% Just the top level here.
    unify_prove_body(F, '.', A, 2, Next, St);
prove_functor({_}=Var, F0, A0, Next, #est{bs=Bs0,vn=Vn0}=St) ->
    case {deref(F0, Bs0),deref(A0, Bs0)} of
	{'.',2} ->				%He, he, he!
	    Bs1 = add_binding(Var, [{Vn0}|{Vn0+1}], Bs0),
	    prove_body(Next, St#est{bs=Bs1,vn=Vn0+2});
	{F1,0} when ?IS_ATOMIC(F1) ->
	    Bs1 = add_binding(Var, F1, Bs0),
	    prove_body(Next, St#est{bs=Bs1});
	{F1,A1} when is_atom(F1), is_integer(A1), A1 > 0 ->
	    As = make_var_list(A1, Vn0),
	    Bs1 = add_binding(Var, list_to_tuple([F1|As]), Bs0),
	    prove_body(Next, St#est{bs=Bs1,vn=Vn0+A1}); %!!!
	%% Now the error cases.
	{{_},_} -> erlog_int:instantiation_error(St);
	{F1,A1} when is_atom(F1) -> erlog_int:type_error(integer, A1, St);
	{F1,_} -> erlog_int:type_error(atom, F1, St)
    end.

%% prove_term_variables(Term, List, Tail, Next, State) -> void.
%%  Unify List with a list of all the variables in Term with tail
%%  Tail. The variables are in depth-first and left-to-right of how
%%  they occur in Term.

prove_term_variables(T, L, Tail, Next, #est{bs=Bs}=St) ->
    Tvs = term_vars(T, Tail, Bs),
    unify_prove_body(Tvs, L, Next, St).

%% term_vars(Term, Tail, Bindings) -> TermVariables.
%%  This is like dderef but we never rebuild Term just get the variables.

term_vars(A, Vars, _) when ?IS_CONSTANT(A) -> Vars;
term_vars([], Vars, _) -> Vars;
term_vars([H|T], Vars0, Bs) ->
    Vars1 = term_vars(H, Vars0, Bs),
    term_vars(T, Vars1, Bs);
term_vars({_}=Var, Vars, Bs) ->
    case get_binding(Var, Bs) of
	{ok,T} -> term_vars(T, Vars, Bs);
	error ->
	    case lists:member(Var, Vars) of	%Add to the end if not there
		true -> Vars;
		false -> Vars ++ [Var]
	    end
    end;
term_vars(T, Vars, Bs) ->
    foldl(fun (E, Vs) -> term_vars(E, Vs, Bs) end, Vars, tl(tuple_to_list(T))).

%% prove_univ(Term, List, Next, State) -> void.
%%  Prove the goal Term =.. List, Term has already been dereferenced.

prove_univ(T, L, Next, St) when tuple_size(T) >= 2 ->
    Es = tuple_to_list(T),
    unify_prove_body(Es, L, Next, St);
prove_univ(T, L, Next, St) when ?IS_ATOMIC(T) ->
    unify_prove_body([T], L, Next, St);
prove_univ([Lh|Lt], L, Next, St) ->
    %% He, he, he!
    unify_prove_body(['.',Lh,Lt], L, Next, St);
prove_univ({_}=Var, L, Next, #est{bs=Bs0}=St) ->
    case dderef(L, Bs0) of
	['.',Lh,Lt] ->				%He, he, he!
	    Bs1 = add_binding(Var, [Lh|Lt], Bs0),
	    prove_body(Next, St#est{bs=Bs1});
	[A] when ?IS_ATOMIC(A) ->
	    Bs1 = add_binding(Var, A, Bs0),
	    prove_body(Next, St#est{bs=Bs1});
	[F|As] when is_atom(F), length(As) > 0 ->
	    Bs1 = add_binding(Var, list_to_tuple([F|As]), Bs0),
	    prove_body(Next, St#est{bs=Bs1});
	%% Now the error cases.
	[{_}|_] -> erlog_int:instantiation_error(St);
	{_} -> erlog_int:instantiation_error(St);
	Other -> erlog_int:type_error(list, Other, St)
end.

%% prove_atom_chars(Atom, List, Next, State) ->
%%     void.
%%  Prove the atom_chars(Atom, List).

prove_atom_chars(A, L, Next, #est{bs=Bs}=St) ->
    %% After a suggestion by Sean Cribbs.
    case deref(A, Bs) of
        Atom when is_atom(Atom) ->
            AtomList = [ list_to_atom([C]) || C <- atom_to_list(Atom) ],
            unify_prove_body(L, AtomList, Next, St);
        {_}=Var ->
            %% Error #3: List is neither a list nor a partial list.
            %% Handled in dderef_list/2.
            List = dderef_list(L, Bs),
            %% Error #1, #4: List is a list or partial list with an
            %% element which is a variable or not one char atom.
	    Fun = fun ({_}) -> erlog_int:instantiation_error(St);
		      (Atom) ->
			  case is_atom(Atom) andalso atom_to_list(Atom) of
			      [C] -> C;
			      _ -> erlog_int:type_error(character, Atom, St)
			  end
		  end,
	    Chars = lists:map(Fun, List),
	    Atom = list_to_atom(Chars),		%This should not crash
	    unify_prove_body(Var, Atom, Next, St);
	Other ->
	    %% Error #2: Atom is neither a variable nor an atom
	    erlog_int:type_error(atom, Other, St)
    end.

%% prove_atom_codes(Atom, List, Next, State) ->
%%     void.
%%  Prove the atom_codes(Atom, List).

prove_atom_codes(A, L, Next, #est{bs=Bs}=St) ->
    case deref(A, Bs) of
        Atom when is_atom(Atom) ->
            AtomList = atom_to_list(Atom),
            unify_prove_body(L, AtomList, Next, St);
        {_}=Var ->
            %% Error #3: List is neither a list nor a partial list.
            %% Handled in dderef_list/2.
            List = dderef_list(L, Bs),
            %% Error #1, #4: List is a list or partial list with an
            %% element which is a variable or not one char atom.
	    Fun = fun ({_}) -> erlog_int:instantiation_error(St);
		      (C) when is_integer(C), C >= 0, C < 255 -> C;
		      (C) -> erlog_int:type_error(character_code, C, St)
		  end,
	    Chars = lists:map(Fun, List),
	    Atom = list_to_atom(Chars),		%This should not crash
	    unify_prove_body(Var, Atom, Next, St);
	Other ->
	    %% Error #2: Atom is neither a variable nor an atom
	    erlog_int:type_error(atom, Other, St)
    end.

%% arith_test_prove_body(Test, Left, Right, Next, State) ->
%%     void.

arith_test_prove_body(Test, L, R, Next, #est{bs=Bs}=St) ->
    case erlang:Test(eval_arith(deref(L, Bs), Bs, St),
		     eval_arith(deref(R, Bs), Bs, St)) of
	true -> prove_body(Next, St);
	false -> fail(St)
    end.

%% eval_arith(ArithExpr, Bindings, State) -> Number.
%%  Evaluate an arithmetic expression, include the state for errors.
%%  Dereference each level as we go, might fail so save some work.
%%  Must be called deferenced.

eval_arith({'+',A,B}, Bs, St) ->
    eval_arith(deref(A, Bs), Bs, St) + eval_arith(deref(B, Bs), Bs, St);
eval_arith({'-',A,B}, Bs, St) ->
    eval_arith(deref(A, Bs), Bs, St) - eval_arith(deref(B, Bs), Bs, St);
eval_arith({'*',A,B}, Bs, St) ->
    eval_arith(deref(A, Bs), Bs, St) * eval_arith(deref(B, Bs), Bs, St);
eval_arith({'/',A,B}, Bs, St) ->
    eval_arith(deref(A, Bs), Bs, St) / eval_arith(deref(B, Bs), Bs, St);
eval_arith({'**',A,B}, Bs, St) ->
    math:pow(eval_arith(deref(A, Bs), Bs, St),
	     eval_arith(deref(B, Bs), Bs, St));
eval_arith({'//',A,B}, Bs, St) ->
    eval_int(deref(A, Bs), Bs, St) div eval_int(deref(B, Bs), Bs, St);
eval_arith({'mod',A,B}, Bs, St) ->
    eval_int(deref(A, Bs), Bs, St) rem eval_int(deref(B, Bs), Bs, St);
eval_arith({'/\\',A,B}, Bs, St) ->
    eval_int(deref(A, Bs), Bs, St) band eval_int(deref(B, Bs), Bs, St);
eval_arith({'\\/',A,B}, Bs, St) ->
    eval_int(deref(A, Bs), Bs, St) bor eval_int(deref(B, Bs), Bs, St);
eval_arith({'<<',A,B}, Bs, St) ->
    eval_int(deref(A, Bs), Bs, St) bsl eval_int(deref(B, Bs), Bs, St);
eval_arith({'>>',A,B}, Bs, St) ->
    eval_int(deref(A, Bs), Bs, St) bsr eval_int(deref(B, Bs), Bs, St);
eval_arith({'\\',A}, Bs, St) ->
    bnot eval_int(deref(A, Bs), Bs, St);
eval_arith({'+',A}, Bs, St) ->
    + eval_arith(deref(A, Bs), Bs, St);
eval_arith({'-',A}, Bs, St) ->
    - eval_arith(deref(A, Bs), Bs, St);
eval_arith({'abs',A}, Bs, St) ->
    abs(eval_arith(deref(A, Bs), Bs, St));
eval_arith({'float',A}, Bs, St) ->
    float(eval_arith(deref(A, Bs), Bs, St));
eval_arith({'truncate',A}, Bs, St) ->
    trunc(eval_arith(deref(A, Bs), Bs, St));
eval_arith(N, _Bs, _Db) when is_number(N) -> N;	%Just a number
%% Error cases.
eval_arith({_}, _Bs, St) ->
    erlog_int:instantiation_error(St);
eval_arith(N, _Bs, Db) when is_tuple(N) ->
    Pi = pred_ind(element(1, N), tuple_size(N)-1),
    erlog_int:type_error(evaluable, Pi, Db);
eval_arith([_|_], _Bs, Db) ->
    erlog_int:type_error(evaluable, pred_ind('.', 2), Db);
eval_arith(O, _Bs, Db) ->
    erlog_int:type_error(evaluable, O, Db).

%% eval_int(IntegerExpr, Bindings, State) -> Integer.
%% Evaluate an integer expression, include the state for errors.

eval_int(E0, Bs, St) ->
    E = eval_arith(E0, Bs, St),
    if  is_integer(E) -> E;
	true -> erlog_int:type_error(integer, E, St)
    end.

pred_ind(N, A) -> {'/',N,A}.
