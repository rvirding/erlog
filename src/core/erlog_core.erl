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

%% File    : erlog_int.erl
%% Author  : Robert Virding
%% Purpose : Basic interpreter of a Prolog sub-set.
%% 
%% This is the basic Prolog interpreter. 
%% The internal data structures used are very direct and basic:
%%
%% Structures	- {Functor,arg1, Arg2,...} where Functor is an atom
%% Variables	- {Name} where Name is an atom or integer
%% Lists	- Erlang lists
%% Atomic	- Erlang constants
%%
%% There is no problem with the representation of variables as Prolog
%% functors of arity 0 are atoms. This representation is much easier
%% to test for, and create new variables with than using funny atom
%% names like '$1' (yuch!), and we need LOTS of variables.
%%
%% All information about the state of an evaluation is held in the
%% variables:
%%
%% [CurrentGoal,] NextGoal, ChoicePoints, Bindings, VarNum, Database
%%
%% Proving a goal succeeds when we have reached the end of the goal
%% list, i.e. NextGoal is empty (true). Proving goal fails when there
%% are no more choice points left to backtrack into. The evaluation
%% is completely flat as all back track information is held in
%% ChoicePoints. Choice points are added going forwards and removed
%% by backtracking and cuts.
%%
%% Internal goals all have the format {{Name},...} as this is an
%% illegal Erlog structure which can never be generated in (legal)
%% code.
%%
%% Proving a top-level goal will return:
%%
%% {succeed,ChoicePoints,Bindings,VarNum,Database} - the
%%     goal succeeded and these are the
%%     choicepoints/bindings/varnum/database to continue with.
%%
%% {fail,Database} - the goal failed and this is the current database.
%%
%% When a goal has succeeded back tracking is initiated by calling
%% fail(ChoicePoints, Database) which has the same return values as
%% proving the goal.
%%
%% When the interpreter detects an error it builds an error term
%%
%%	{erlog_error,ErrorDescriptor,Database}
%%
%% and throws it. The ErrorDescriptor is a valid Erlog term.
%%
%% Database
%%
%% We use a dictionary for the database. All data for a procedure are
%% kept in the database with the functor as key. Interpreted clauses
%% are kept in a list, each clause has a unique (for that functor)
%% tag. Functions which traverse clauses, clause/retract/goals, get
%% the whole list to use. Any database operations can they be done
%% directly on the database. Retract uses the tag to remove the
%% correct clause. This preserves the logical database view. It is
%% possible to use ETS instead if a dictionary, define macro ETS, but
%% the logical database view makes it difficult to directly use ETS
%% efficiently.
%%
%% Interpreted Code
%%
%% Code, interpreted clause bodies, are not stored directly as Erlog
%% terms. Before being added to the database they are checked that
%% they are well-formed, control structures are recognised, cuts
%% augmented with status and sequences of conjunctions are converted
%% to lists. When code is used a new instance is made with fresh
%% variables, correct cut labels, and bodies directly linked to
%% following code to remove the need of later appending.
%%
%% The following functions convert code:
%%
%% well_form_body/4 - converts an Erlog term to database code body
%%     format checking that it is well formed.
%% well_form_goal/4 - converts an Erlog term directly to a code body
%%     checking that it is well formed.
%% unify_head/4 - unify a goal directly with head without creating a
%%     new instance of the head. Saves creating local variables and
%%     MANY bindings. This is a BIG WIN!
%% body_instance/5 - creates a new code body instance from the
%%     database format.
%% term_instance/2/3 - creates a new instance of a term with new
%%     variables.
%% body_term/3 - creates a copy of a body as a legal Erlog term.
%%
%% Choicepoints/Cuts
%%
%% Choicepoints and cuts are kept on the same stack/list. There are
%% different types of cps depending on their context. Failure pops
%% the first cp off the stack, passing over cuts and resumes
%% execution from that cp. A cut has a label and a flag indicating if
%% this is the last cut with this label. Cut steps over cps/cuts
%% until a cut the same label is reached and execution is resumed
%% with that stack. Unless this is the last cut with a label a new
%% cut is pushed on the stack. For efficiency some cps also act as
%% cuts.
%%
%% It is possible to reuse cut labels for different markers as long
%% the areas the cuts are valid don't overlap, though one may be
%% contained within the other, and the cuts correctly indicate when
%% they are the last cut. This is used for ->; and once/1 where we
%% KNOW the last cut of the internal section.
%%
%% It would be better if the cut marker was the actual cps/cut stack
%% to go back to but this would entail a more interactive
%% body_instance.

-module(erlog_core).

-include("erlog_int.hrl").

%%-define(BIND, orddict).  %TODO ets and others?
-define(BIND, dict).

%% Main execution functions.
-export([
	unify/3,
	dderef_list/2,
	make_vars/2,
	prove_goal/2,
	unify_prove_body/9,
	prove_body/5,
	unify_clauses/8,
	retract_clauses/8,
	prove_predicates/7,
	prove_goal_clauses/7,
	pred_ind/1,
	well_form_body/3,
	deref_list/2, unify_prove_body/7, dderef/2, deref/2, add_binding/3]).
%% Bindings, unification and dereferncing.
-export([functor/1]).
%% Creating term and body instances.
-export([term_instance/2]).
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
prove_goal(Goal0, Db) ->
	%% put(erlog_cut, orddict:new()),
	%% put(erlog_cps, orddict:new()),
	%% put(erlog_var, orddict:new()),
	%% Check term and build new instance of term with bindings.
	{Goal1, Bs, Vn} = initial_goal(Goal0),
	prove_body([{call, Goal1}], [], Bs, Vn, Db).

%% prove_body(Body, ChoicePoints, Bindings, VarNum, Database) ->
%%      {succeed,ChoicePoints,NewBindings,NewVarNum,NewDatabase}.
%% Prove the goals in a body. Remove the first goal and try to prove
%% it. Return when there are no more goals. This is how proving a
%% goal/body succeeds.
prove_body([G | Gs], Cps, Bs0, Vn0, Db) ->
	%%io:fwrite("PB: ~p\n", [{G,Gs,Cps}]),
	prove_goal(G, Gs, Cps, Bs0, Vn0, Db);
prove_body([], Cps, Bs, Vn, Db) ->
	%%io:fwrite("Cps: ~p\nCut: ~p\nVar: ~p\nVar: ~p\n",
	%%      [get(erlog_cps),get(erlog_cut),get(erlog_var),dict:size(Bs)]),
	%%io:fwrite("PB: ~p\n", [Cps]),
	{succeed, Cps, Bs, Vn, Db}.      %No more body

%% unify_prove_body(Term1, Term2, Next, ChoicePoints, Bindings, VarNum, Database) ->
%%	void.
%% Unify Term1 = Term2, on success prove body Next else fail.
unify_prove_body(T1, T2, Next, Cps, Bs0, Vn, Db) ->
	case unify(T1, T2, Bs0) of
		{succeed, Bs1} -> prove_body(Next, Cps, Bs1, Vn, Db);
		fail -> ?FAIL(Bs0, Cps, Db)
	end.

%% unify_prove_body(A1, B1, A2, B2, Next, ChoicePoints, Bindings, VarNum, Database) ->
%%	void.
%% Unify A1 = B1, A2 = B2, on success prove body Next else fail.
unify_prove_body(A1, B1, A2, B2, Next, Cps, Bs0, Vn, Db) ->
	case unify(A1, B1, Bs0) of
		{succeed, Bs1} -> unify_prove_body(A2, B2, Next, Cps, Bs1, Vn, Db);
		fail -> ?FAIL(Bs0, Cps, Db)
	end.

%% deref(Term, Bindings) -> Term.
%% Dereference a variable, else just return the term.
deref({V} = T0, Bs) ->
	case ?BIND:find(V, Bs) of
		{ok, T1} -> deref(T1, Bs);
		error -> T0
	end;
deref(T, _) -> T.        %Not a variable, return it.

%% deref_list(List, Bindings) -> List.
%%  Dereference the top-level checking that it is a list.
deref_list([], _) -> [];      %It already is a list %TODO where it is used?
deref_list([_ | _] = L, _) -> L;
deref_list({V}, Bs) ->
	case dict:find(V, Bs) of
		{ok, L} -> deref_list(L, Bs);
		error -> erlog_errors:instantiation_error()
	end;
deref_list(Other, _) -> erlog_errors:type_error(list, Other).

%% dderef(Term, Bindings) -> Term.
%% Do a deep dereference. Completely dereference all the variables
%% occuring in a term, even those occuring in a variables value.
dderef(A, _) when ?IS_CONSTANT(A) -> A;
dderef([], _) -> [];
dderef([H0 | T0], Bs) ->
	[dderef(H0, Bs) | dderef(T0, Bs)];
dderef({V} = Var, Bs) ->
	case ?BIND:find(V, Bs) of %TODO check, why dict instead erlog_storage
		{ok, T} -> dderef(T, Bs);
		error -> Var
	end;
dderef(T, Bs) when is_tuple(T) ->
	Es0 = tuple_to_list(T),
	Es1 = dderef(Es0, Bs),
	list_to_tuple(Es1).

%% dderef_list(List, Bindings) -> List.
%%  Dereference all variables to any depth but check that the
%%  top-level is a list.
dderef_list([], _Bs) -> [];
dderef_list([H | T], Bs) ->
	[dderef(H, Bs) | dderef_list(T, Bs)];
dderef_list({V}, Bs) ->
	case ?BIND:find(V, Bs) of
		{ok, L} -> dderef_list(L, Bs);
		error -> erlog_errors:instantiation_error()
	end;
dderef_list(Other, _Bs) -> erlog_errors:type_error(list, Other).

%% make_vars(Count, VarNum) -> [Var].
%% Make a list of new variables starting at VarNum.
make_vars(0, _) -> [];
make_vars(I, Vn) ->
	[{Vn} | make_vars(I - 1, Vn + 1)].

%% prove_goal(Goal, NextGoal, ChoicePoints, Bindings, VarNum, Database) ->
%%	{succeed,ChoicePoints,NewBindings,NewVarNum,NewDatabase} |
%%      {fail,NewDatabase}.
%% Prove one goal. We seldom return succeed here but usually go directly to
%% to NextGoal.
%% Handle built-in predicates here. RTFM for a description of the
%% built-ins. Hopefully we do the same.

%% Logic and control. Conjunctions are handled in prove_body and true
%% has been compiled away.
prove_goal({call, G}, Next0, Cps, Bs, Vn, Db) ->
	%% Only add cut CP to Cps if goal contains a cut.
	Label = Vn,
	case check_goal(G, Next0, Bs, Db, false, Label) of
		{Next1, true} ->
			%% Must increment Vn to avoid clashes!!!
			Cut = #cut{label = Label},
			prove_body(Next1, [Cut | Cps], Bs, Vn + 1, Db);
		{Next1, false} -> prove_body(Next1, Cps, Bs, Vn + 1, Db)
	end;
prove_goal({{cut}, Label, Last}, Next, Cps, Bs, Vn, Db) ->
	%% Cut succeeds and trims back to cut ancestor.
	cut(Label, Last, Next, Cps, Bs, Vn, Db);
prove_goal({{disj}, R}, Next, Cps, Bs, Vn, Db) ->
	%% There is no L here, it has already been prepended to Next.
	Cp = #cp{type = disjunction, next = R, bs = Bs, vn = Vn},
	prove_body(Next, [Cp | Cps], Bs, Vn, Db);
prove_goal(fail, _Next, Cps, _Bs, _Vn, Db) -> ?FAIL(_Bs, Cps, Db);
prove_goal({{if_then}, Label}, Next, Cps, Bs, Vn, Db) ->
	%% We effetively implement ( C -> T ) with ( C, !, T ) but cuts in
	%% C are local to C.
	%% There is no ( C, !, T ) here, it has already been prepended to Next.
	%%io:fwrite("PG(->): ~p\n", [{Next}]),
	Cut = #cut{label = Label},
	prove_body(Next, [Cut | Cps], Bs, Vn, Db);
prove_goal({{if_then_else}, Else, Label}, Next, Cps, Bs, Vn, Db) ->
	%% Need to push a choicepoint to fail back to inside Cond and a cut
	%% to cut back to before Then when Cond succeeds. #cp{type=if_then_else}
	%% functions as both as is always removed whatever the outcome.
	%% There is no ( C, !, T ) here, it has already been prepended to Next.
	Cp = #cp{type = if_then_else, label = Label, next = Else, bs = Bs, vn = Vn},
	%%io:fwrite("PG(->;): ~p\n", [{Next,Else,[Cp|Cps]}]),
	prove_body(Next, [Cp | Cps], Bs, Vn, Db);
prove_goal({'\\+', G}, Next0, Cps, Bs, Vn, Db) ->
	%% We effectively implementing \+ G with ( G -> fail ; true ).
	Label = Vn,
	{Next1, _} = check_goal(G, [{{cut}, Label, true}, fail], Bs, Db, true, Label),
	Cp = #cp{type = if_then_else, label = Label, next = Next0, bs = Bs, vn = Vn},
	%%io:fwrite("PG(\\+): ~p\n", [{G1,[Cp|Cps]]),
	%% Must increment Vn to avoid clashes!!!
	prove_body(Next1, [Cp | Cps], Bs, Vn + 1, Db);
prove_goal({{once}, Label}, Next, Cps, Bs, Vn, Db) ->
	%% We effetively implement once(G) with ( G, ! ) but cuts in
	%% G are local to G.
	%% There is no ( G, ! ) here, it has already been prepended to Next.
	Cut = #cut{label = Label},
	prove_body(Next, [Cut | Cps], Bs, Vn, Db);
prove_goal(repeat, Next, Cps, Bs, Vn, Db) ->
	Cp = #cp{type = disjunction, next = [repeat | Next], bs = Bs, vn = Vn},
	prove_body(Next, [Cp | Cps], Bs, Vn, Db);
%% Clause creation and destruction.
prove_goal({abolish, Pi0}, Next, Cps, Bs, Vn, Db) ->
	case dderef(Pi0, Bs) of
		{'/', N, A} when is_atom(N), is_integer(A), A > 0 ->
			erlog_memory:abolish_clauses(Db, {N, A}),
			prove_body(Next, Cps, Bs, Vn, Db);
		Pi -> erlog_errors:type_error(predicate_indicator, Pi, Db)
	end;
prove_goal({assert, C0}, Next, Cps, Bs, Vn, Db) ->
	C = dderef(C0, Bs),
	erlog_memory:assertz_clause(Db, C),
	prove_body(Next, Cps, Bs, Vn, Db);
prove_goal({asserta, C0}, Next, Cps, Bs, Vn, Db) ->
	C = dderef(C0, Bs),
	erlog_memory:asserta_clause(Db, C),
	prove_body(Next, Cps, Bs, Vn, Db);
prove_goal({assertz, C0}, Next, Cps, Bs, Vn, Db) ->
	C = dderef(C0, Bs),
	erlog_memory:assertz_clause(Db, C),
	prove_body(Next, Cps, Bs, Vn, Db);
prove_goal({retract, C0}, Next, Cps, Bs, Vn, Db) ->
	C = dderef(C0, Bs),
	prove_retract(C, Next, Cps, Bs, Vn, Db);
%% Clause retrieval and information
prove_goal({clause, H0, B}, Next, Cps, Bs, Vn, Db) ->
	H1 = dderef(H0, Bs),
	prove_clause(H1, B, Next, Cps, Bs, Vn, Db);
prove_goal({current_predicate, Pi0}, Next, Cps, Bs, Vn, Db) ->
	Pi = dderef(Pi0, Bs),
	prove_current_predicate(Pi, Next, Cps, Bs, Vn, Db);
prove_goal({predicate_property, H0, P}, Next, Cps, Bs, Vn, Db) ->
	H = dderef(H0, Bs),
	case catch erlog_memory:get_procedure_type(Db, functor(H)) of
		built_in -> unify_prove_body(P, built_in, Next, Cps, Bs, Vn, Db);
		compiled -> unify_prove_body(P, compiled, Next, Cps, Bs, Vn, Db);
		interpreted -> unify_prove_body(P, interpreted, Next, Cps, Bs, Vn, Db);
		undefined -> ?FAIL(Bs, Cps, Db);
		{erlog_error, E} -> erlog_errors:erlog_error(E, Db)
	end;
%% External interface
prove_goal({ecall, C0, Val}, Next, Cps, Bs, Vn, Db) ->
	%% Build the initial call.
	%%io:fwrite("PG(ecall): ~p\n   ~p\n   ~p\n", [dderef(C0, Bs),Next,Cps]),
	Efun = case dderef(C0, Bs) of
		       {':', M, F} when is_atom(M), is_atom(F) ->
			       fun() -> M:F() end;
		       {':', M, {F, A}} when is_atom(M), is_atom(F) ->
			       fun() -> M:F(A) end;
		       {':', M, {F, A1, A2}} when is_atom(M), is_atom(F) ->
			       fun() -> M:F(A1, A2) end;
		       {':', M, T} when is_atom(M), ?IS_FUNCTOR(T) ->
			       L = tuple_to_list(T),
			       fun() -> apply(M, hd(L), tl(L)) end;
		       Fun when is_function(Fun) -> Fun;
		       Other -> erlog_errors:type_error(callable, Other, Db)
	       end,
	prove_ecall(Efun, Val, Next, Cps, Bs, Vn, Db);
%% Non-standard but useful.
prove_goal({display, T}, Next, Cps, Bs, Vn, Db) ->
	%% A very simple display procedure.
	io:fwrite("~p\n", [dderef(T, Bs)]),
	prove_body(Next, Cps, Bs, Vn, Db);
%% Now look up the database.
prove_goal(G, Next, Cps, Bs, Vn, Db) ->
%% 	io:fwrite("PG: ~p\n    ~p\n    ~p\n", [dderef(G, Bs),Next,Cps]),
	case catch erlog_memory:get_procedure(Db, functor(G)) of
		built_in -> erlog_bips:prove_goal(G, Next, Cps, Bs, Vn, Db);
		{code, {Mod, Func}} -> Mod:Func(G, Next, Cps, Bs, Vn, Db);
		{clauses, Cs} -> prove_goal_clauses(G, Cs, Next, Cps, Bs, Vn, Db);
		undefined -> ?FAIL(Bs, Cps, Db);
	%% Getting built_in here is an error!
		{erlog_error, E} -> erlog_errors:erlog_error(E, Db)  %Fill in more error data
	end.


cut(Label, Last, Next, [#cut{label = Label} | Cps] = Cps0, Bs, Vn, Db) ->
	if Last -> prove_body(Next, Cps, Bs, Vn, Db);
		true -> prove_body(Next, Cps0, Bs, Vn, Db)
	end;
cut(Label, Last, Next, [#cp{type = if_then_else, label = Label} | Cps] = Cps0, Bs, Vn, Db) ->
	if Last -> prove_body(Next, Cps, Bs, Vn, Db);
		true -> prove_body(Next, Cps0, Bs, Vn, Db)
	end;
cut(Label, Last, Next, [#cp{type = goal_clauses, label = Label} = Cp | Cps], Bs, Vn, Db) ->
	cut_goal_clauses(Last, Next, Cp, Cps, Bs, Vn, Db);
cut(Label, Last, Next, [_Cp | Cps], Bs, Vn, Db) ->
	cut(Label, Last, Next, Cps, Bs, Vn, Db).

%% cut(Label, Last, Next, Cps, Bs, Vn, Db) ->
%%     cut(Label, Last, Next, Cps, Bs, Vn, Db, 1).

%% cut(Label, Last, Next, [#cut{label=Label}|Cps]=Cps0, Bs, Vn, Db, Cn) ->
%%     put(erlog_cut, orddict:update_counter(Cn, 1, get(erlog_cut))),
%%     if  Last -> prove_body(Next, Cps, Bs, Vn, Db);
%% 	true -> prove_body(Next, Cps0, Bs, Vn, Db)
%%     end;
%% cut(Label, Last, Next, [#cp{type=if_then_else,label=Label}|Cps]=Cps0, Bs, Vn, Db, Cn) ->
%%     put(erlog_cut, orddict:update_counter(Cn, 1, get(erlog_cut))),
%%     if  Last -> prove_body(Next, Cps, Bs, Vn, Db);
%% 	true -> prove_body(Next, Cps0, Bs, Vn, Db)
%%     end;
%% cut(Label, Last, Next, [#cp{type=goal_clauses,label=Label}=Cp|Cps], Bs, Vn, Db, Cn) ->
%%     put(erlog_cut, orddict:update_counter(Cn, 1, get(erlog_cut))),
%%     cut_goal_clauses(Last, Next, Cp, Cps, Bs, Vn, Db);
%% cut(Label, Last, Next, [_Cp|Cps], Bs, Vn, Db, Cn) ->
%%     cut(Label, Last, Next, Cps, Bs, Vn, Db, Cn+1).

%% check_goal(Goal, Next, Bindings, Database, CutAfter, CutLabel) -> 
%%      {WellFormedBody,HasCut}.
%% Check to see that Goal is bound and ensure that it is well-formed.
check_goal(G0, Next, Bs, Db, Cut, Label) ->
	case dderef(G0, Bs) of
		{_} -> erlog_errors:instantiation_error(Db);    %Must have something to call
		G1 ->
			case catch {ok, well_form_goal(G1, Next, Cut, Label)} of
				{erlog_error, E} -> erlog_errors:erlog_error(E, Db);
				{ok, GC} -> GC      %Body and cut
			end
	end.

%% prove_ecall(Generator, Value, Next, ChoicePoints, Bindings, VarNum, Database) ->
%%	void.
%% Call an external (Erlang) generator and handle return value, either
%% succeed or fail.

prove_ecall(Efun, Val, Next, Cps, Bs, Vn, Db) ->
	case Efun() of
		{succeed, Ret, Cont} ->      %Succeed and more choices
			Cp = #cp{type = ecall, data = {Cont, Val}, next = Next, bs = Bs, vn = Vn},
			unify_prove_body(Val, Ret, Next, [Cp | Cps], Bs, Vn, Db);
		{succeed_last, Ret} ->      %Succeed but last choice
			unify_prove_body(Val, Ret, Next, Cps, Bs, Vn, Db);
		fail -> ?FAIL(Bs, Cps, Db)      %No more
	end.

%% prove_clause(Head, Body, Next, ChoicePoints, Bindings, VarNum, DataBase) ->
%%      void.
%% Unify clauses matching with functor from Head with both Head and Body.

prove_clause(H, B, Next, Cps, Bs, Vn, Db) ->
	Functor = functor(H),
	case erlog_memory:get_procedure(Db, Functor) of
		{clauses, Cs} -> unify_clauses(H, B, Cs, Next, Cps, Bs, Vn, Db);
		{code, _} ->
			erlog_errors:permission_error(access, private_procedure, pred_ind(Functor), Db);
		built_in ->
			erlog_errors:permission_error(access, private_procedure, pred_ind(Functor), Db);
		undefined -> ?FAIL(Bs, Cps, Db)
	end.

%% unify_clauses(Head, Body, Clauses, Next, ChoicePoints, Bindings, VarNum, Database) ->
%%      void.
%% Try to unify Head and Body using Clauses which all have the same functor.

unify_clauses(Ch, Cb, [C], Next, Cps, Bs0, Vn0, Db) ->
	%% No choice point on last clause
	case unify_clause(Ch, Cb, C, Bs0, Vn0) of
		{succeed, Bs1, Vn1} -> prove_body(Next, Cps, Bs1, Vn1, Db);
		fail -> ?FAIL(Bs0, Cps, Db)
	end;
unify_clauses(Ch, Cb, [C | Cs], Next, Cps, Bs0, Vn0, Db) ->
	case unify_clause(Ch, Cb, C, Bs0, Vn0) of
		{succeed, Bs1, Vn1} ->
			Cp = #cp{type = clause, data = {Ch, Cb, Cs}, next = Next, bs = Bs0, vn = Vn0},
			prove_body(Next, [Cp | Cps], Bs1, Vn1, Db);
		fail -> unify_clauses(Ch, Cb, Cs, Next, Cps, Bs0, Vn0, Db)
	end;
unify_clauses(_Ch, _Cb, [], _Next, Cps, _Bs, _Vn, Db) -> ?FAIL(_Bs, Cps, Db).

unify_clause(Ch, Cb, {_Tag, H0, {B0, _}}, Bs0, Vn0) ->
	{H1, Rs1, Vn1} = term_instance(H0, Vn0),  %Unique vars on head first
	case unify(Ch, H1, Bs0) of
		{succeed, Bs1} ->
			{B1, _Rs2, Vn2} = body_term(B0, Rs1, Vn1),  %Now we need the rest
			case unify(Cb, B1, Bs1) of
				{succeed, Bs2} -> {succeed, Bs2, Vn2};
				fail -> fail
			end;
		fail -> fail
	end.

%% prove_current_predicate(PredInd, Next, ChoicePoints, Bindings, VarNum, DataBase) ->
%%      void.
%% Match functors of existing user (interpreted) predicate with PredInd.

prove_current_predicate(Pi, Next, Cps, Bs, Vn, Db) ->
	case Pi of
		{'/', _, _} -> ok;
		{_} -> ok;
		Other -> erlog_errors:type_error(predicate_indicator, Other)
	end,
	Fs = erlog_memory:get_interp_functors(Db),
	prove_predicates(Pi, Fs, Next, Cps, Bs, Vn, Db).

prove_predicates(Pi, [F | Fs], Next, Cps, Bs, Vn, Db) ->
	Cp = #cp{type = current_predicate, data = {Pi, Fs}, next = Next, bs = Bs, vn = Vn},
	unify_prove_body(Pi, pred_ind(F), Next, [Cp | Cps], Bs, Vn, Db);
prove_predicates(_Pi, [], _Next, Cps, _Bs, _Vn, Db) -> ?FAIL(_Bs, Cps, Db).

%% prove_goal_clauses(Goal, Clauses, Next, ChoicePoints, Bindings, VarNum, Database) ->
%%      void.
%% Try to prove Goal using Clauses which all have the same functor.
prove_goal_clauses(G, [C], Next, Cps, Bs, Vn, Db) ->
	%% Must be smart here and test whether we need to add a cut point.
	%% C has the structure {Tag,Head,{Body,BodyHasCut}}.
	case element(2, element(3, C)) of
		true ->
			Cut = #cut{label = Vn},
			prove_goal_clause(G, C, Next, [Cut | Cps], Bs, Vn, Db);
		false ->
			prove_goal_clause(G, C, Next, Cps, Bs, Vn, Db)
	end;
%% prove_goal_clause(G, C, Next, Cps, Bs, Vn, Db);
prove_goal_clauses(G, [C | Cs], Next, Cps, Bs, Vn, Db) ->
	Cp = #cp{type = goal_clauses, label = Vn, data = {G, Cs}, next = Next, bs = Bs, vn = Vn},
	prove_goal_clause(G, C, Next, [Cp | Cps], Bs, Vn, Db);
prove_goal_clauses(_G, [], _Next, Cps, _Bs, _Vn, Db) -> ?FAIL(_Bs, Cps, Db).

prove_goal_clause(G, {_Tag, H0, {B0, _}}, Next, Cps, Bs0, Vn0, Db) ->
	%% io:fwrite("PGC1: ~p\n", [{G,H0,B0}]),
	Label = Vn0,
	case unify_head(G, H0, Bs0, Vn0 + 1) of
		{succeed, Rs0, Bs1, Vn1} ->
			%% io:fwrite("PGC2: ~p\n", [{Rs0}]),
			{B1, _Rs2, Vn2} = body_instance(B0, Next, Rs0, Vn1, Label),
			%% io:fwrite("PGC3: ~p\n", [{B1,Next,Cps}]),
			prove_body(B1, Cps, Bs1, Vn2, Db);
		fail -> ?FAIL(Bs0, Cps, Db)
	end.

%% cut_goal_clauses(Last, Next, Cp, Cps, Bs, Vn, Db).
cut_goal_clauses(true, Next, #cp{label = _}, Cps, Bs, Vn, Db) ->
	%% Just remove the choice point completely and continue.
	prove_body(Next, Cps, Bs, Vn, Db);
cut_goal_clauses(false, Next, #cp{label = L}, Cps, Bs, Vn, Db) ->
	%% Replace choice point with cut point then continue.
	Cut = #cut{label = L},
	prove_body(Next, [Cut | Cps], Bs, Vn, Db).

%% prove_retract(Clause, Next, ChoicePoints, Bindings, VarNum, Database) ->
%%      void.
%% Retract clauses in database matching Clause.

prove_retract({':-', H, B}, Next, Cps, Bs, Vn, Db) ->
	prove_retract(H, B, Next, Cps, Bs, Vn, Db);
prove_retract(H, Next, Cps, Bs, Vn, Db) ->
	prove_retract(H, true, Next, Cps, Bs, Vn, Db).

prove_retract(H, B, Next, Cps, Bs, Vn, Db) ->
	Functor = functor(H),
	case erlog_memory:get_procedure(Db, Functor) of
		{clauses, Cs} -> retract_clauses(H, B, Cs, Next, Cps, Bs, Vn, Db);
		{code, _} ->
			erlog_errors:permission_error(modify, static_procedure, pred_ind(Functor), Db);
		built_in ->
			erlog_errors:permission_error(modify, static_procedure, pred_ind(Functor), Db);
		undefined -> ?FAIL(Bs, Cps, Db)
	end.

%% retract_clauses(Head, Body, Clauses, Next, ChoicePoints, Bindings, VarNum, Database) ->
%%      void.
%% Try to retract Head and Body using Clauses which all have the same functor.

retract_clauses(Ch, Cb, [C | Cs], Next, Cps, Bs0, Vn0, Db) -> %TODO foreach vs handmaid recursion?
	case unify_clause(Ch, Cb, C, Bs0, Vn0) of
		{succeed, Bs1, Vn1} ->
			%% We have found a right clause so now retract it.
			erlog_memory:retract_clause(Db, functor(Ch), element(1, C)),
			Cp = #cp{type = retract, data = {Ch, Cb, Cs}, next = Next, bs = Bs0, vn = Vn0},
			prove_body(Next, [Cp | Cps], Bs1, Vn1, Db);
		fail -> retract_clauses(Ch, Cb, Cs, Next, Cps, Bs0, Vn0, Db)
	end;
retract_clauses(_Ch, _Cb, [], _Next, Cps, _Bs, _Vn, Db) -> ?FAIL(_Bs, Cps, Db).

unify_args(_, _, Bs, I, S) when I > S -> {succeed, Bs};
unify_args(S1, S2, Bs0, I, S) ->
	case unify(element(I, S1), element(I, S2), Bs0) of
		{succeed, Bs1} -> unify_args(S1, S2, Bs1, I + 1, S);
		fail -> fail
	end.

%% unify(Term, Term, Bindings) -> {succeed,NewBindings} | fail.
%% Unify two terms with a set of bindings.
unify(T10, T20, Bs0) ->
	case {deref(T10, Bs0), deref(T20, Bs0)} of
		{T1, T2} when ?IS_CONSTANT(T1), T1 == T2 ->
			{succeed, Bs0};
		{{V}, {V}} -> {succeed, Bs0};
		{{_} = Var, T2} -> {succeed, add_binding(Var, T2, Bs0)};
		{T1, {_} = Var} -> {succeed, add_binding(Var, T1, Bs0)};
		{[H1 | T1], [H2 | T2]} ->
			case unify(H1, H2, Bs0) of
				{succeed, Bs1} -> unify(T1, T2, Bs1);
				fail -> fail
			end;
		{[], []} -> {succeed, Bs0};
		{T1, T2} when tuple_size(T1) == tuple_size(T2),
			element(1, T1) == element(1, T2) ->
			unify_args(T1, T2, Bs0, 2, tuple_size(T1));
		_Other -> fail
	end.

%% functor(Goal) -> {Name,Arity}.
functor(T) when ?IS_FUNCTOR(T) ->
	{element(1, T), tuple_size(T) - 1};
functor(T) when is_atom(T) -> {T, 0};
functor(T) -> erlog_errors:type_error(callable, T).

%% well_form_body(Body, HasCutAfter, CutLabel) -> {Body,HasCut}.
%% well_form_body(Body, Tail, HasCutAfter, CutLabel) -> {Body,HasCut}.
%%  Check that Body is well-formed, flatten conjunctions, fix cuts and
%%  add explicit call to top-level variables.
well_form_body(Body, Cut, Label) -> well_form_body(Body, [], Cut, Label).

well_form_body({',', L, R}, Tail0, Cut0, Label) ->
	{Tail1, Cut1} = well_form_body(R, Tail0, Cut0, Label),
	well_form_body(L, Tail1, Cut1, Label);
well_form_body({';', {'->', C0, T0}, E0}, Tail, Cut0, Label) ->
	{T1, Tc} = well_form_body(T0, Cut0, Label),
	{E1, Ec} = well_form_body(E0, Cut0, Label),
	%% N.B. an extra cut will be added at run-time!
	{C1, _} = well_form_body(C0, true, Label),
	{[{{if_then_else}, C1, T1, E1, Label} | Tail], Tc or Ec};
well_form_body({';', L0, R0}, Tail, Cut0, Label) ->
	{L1, Lc} = well_form_body(L0, Cut0, Label),
	{R1, Rc} = well_form_body(R0, Cut0, Label),
	{[{{disj}, L1, R1} | Tail], Lc or Rc};
well_form_body({'->', C0, T0}, Tail, Cut0, Label) ->
	{T1, Cut1} = well_form_body(T0, Cut0, Label),
	%% N.B. an extra cut will be added at run-time!
	{C1, _} = well_form_body(C0, true, Label),
	{[{{if_then}, C1, T1, Label} | Tail], Cut1};
well_form_body({once, G}, Tail, Cut, Label) ->
	%% N.B. an extra cut is added at run-time!
	{G1, _} = well_form_body(G, true, Label),
	{[{{once}, G1, Label} | Tail], Cut};
well_form_body({V}, Tail, Cut, _Label) ->
	{[{call, {V}} | Tail], Cut};
well_form_body(true, Tail, Cut, _Label) -> {Tail, Cut}; %No-op
well_form_body(fail, _Tail, _Cut, _Label) -> {[fail], false};  %No further
well_form_body('!', Tail, Cut, Label) ->
	{[{{cut}, Label, not Cut} | Tail], true};
well_form_body(Goal, Tail, Cut, _Label) ->
	functor(Goal),        %Check goal
	{[Goal | Tail], Cut}.

%% well_form_goal(Goal, Tail, HasCutAfter, CutLabel) -> {Body,HasCut}.
%%  Check that Goal is well-formed, flatten conjunctions, fix cuts and
%%  add explicit call to top-level variables.
well_form_goal({',', L, R}, Tail0, Cut0, Label) ->
	{Tail1, Cut1} = well_form_goal(R, Tail0, Cut0, Label),
	well_form_goal(L, Tail1, Cut1, Label);
well_form_goal({';', {'->', C0, T0}, E0}, Tail, Cut0, Label) ->
	{T1, Tc} = well_form_goal(T0, Tail, Cut0, Label),
	{C1, _} = well_form_goal(C0, [{{cut}, Label, true} | T1], true, Label),
	{E1, Ec} = well_form_goal(E0, Tail, Cut0, Label),
	{[{{if_then_else}, E1, Label} | C1], Tc or Ec};
well_form_goal({';', L0, R0}, Tail, Cut0, Label) ->
	{L1, Lc} = well_form_goal(L0, Tail, Cut0, Label),
	{R1, Rc} = well_form_goal(R0, Tail, Cut0, Label),
	{[{{disj}, R1} | L1], Lc or Rc};
well_form_goal({'->', C0, T0}, Tail, Cut0, Label) ->
	{T1, Cut1} = well_form_goal(T0, Tail, Cut0, Label),
	%% N.B. an extra cut will be added at run-time!
	{C1, _} = well_form_goal(C0, [{{cut}, Label, true} | T1], true, Label),
	{[{{if_then}, Label} | C1], Cut1};
well_form_goal({once, G}, Tail, Cut, Label) ->
	{G1, _} = well_form_goal(G, [{{cut}, Label, true} | Tail], true, Label),
	{[{{once}, Label} | G1], Cut};
well_form_goal({V}, Tail, Cut, _Label) ->
	{[{call, {V}} | Tail], Cut};
well_form_goal(true, Tail, Cut, _Label) -> {Tail, Cut}; %No-op
well_form_goal(fail, _Tail, _Cut, _Label) -> {[fail], false};  %No further
well_form_goal('!', Tail, Cut, Label) ->
	{[{{cut}, Label, not Cut} | Tail], true};
well_form_goal(Goal, Tail, Cut, _Label) ->
	functor(Goal),        %Check goal
	{[Goal | Tail], Cut}.

%% term_instance(Term, VarNum) -> {Term,NewRepls,NewVarNum}.
%% term_instance(Term, Repls, VarNum) -> {Term,NewRepls,NewVarNum}.
%%  Generate a copy of a term with new, fresh unused variables. No
%%  bindings from original variables to new variables. It can handle
%%  replacing integer variables with overlapping integer ranges. Don't
%%  check Term as it should already be checked. Use orddict as there
%%  will seldom be many variables and it it fast to setup.
term_instance(A, Vn) -> term_instance(A, orddict:new(), Vn).

term_instance([], Rs, Vn) -> {[], Rs, Vn};
term_instance([H0 | T0], Rs0, Vn0) ->
	{H, Rs1, Vn1} = term_instance(H0, Rs0, Vn0),
	{T, Rs2, Vn2} = term_instance(T0, Rs1, Vn1),
	{[H | T], Rs2, Vn2};
term_instance({'_'}, Rs, Vn) -> {{Vn}, Rs, Vn + 1};  %Unique variable
term_instance({V0}, Rs0, Vn0) ->    %Other variables
	case orddict:find(V0, Rs0) of
		{ok, V1} -> {V1, Rs0, Vn0};
		error ->
			V1 = {Vn0},
			{V1, orddict:store(V0, V1, Rs0), Vn0 + 1}
	end;
%% Special case some smaller structures.
term_instance({Atom, Arg}, Rs0, Vn0) ->
	{CopyArg, Rs1, Vn1} = term_instance(Arg, Rs0, Vn0),
	{{Atom, CopyArg}, Rs1, Vn1};
term_instance({Atom, A1, A2}, Rs0, Vn0) ->
	{CopyA1, Rs1, Vn1} = term_instance(A1, Rs0, Vn0),
	{CopyA2, Rs2, Vn2} = term_instance(A2, Rs1, Vn1),
	{{Atom, CopyA1, CopyA2}, Rs2, Vn2};
term_instance(T, Rs0, Vn0) when is_tuple(T) ->
	As0 = tl(tuple_to_list(T)),
	{As1, Rs1, Vn1} = term_instance(As0, Rs0, Vn0),
	{list_to_tuple([element(1, T) | As1]), Rs1, Vn1};
term_instance(A, Rs, Vn) -> {A, Rs, Vn}.    %Constant

%% unify_head(Goal, Head, Bindings, VarNum) ->
%%      {succeed,Repls,NewBindings,NewVarNum} | fail
%%  Unify a goal with a head without creating an instance of the
%%  head. This saves us creating many variables which are local to the
%%  clause and saves many variable bindings.

unify_head(Goal, Head, Bs, Vn) ->
	unify_head(deref(Goal, Bs), Head, orddict:new(), Bs, Vn).

unify_head(G, H, Rs, Bs, Vn) when ?IS_CONSTANT(G), G == H ->
	{succeed, Rs, Bs, Vn};
unify_head(_T, {'_'}, Rs, Bs, Vn) -> {succeed, Rs, Bs, Vn};
unify_head(T, {V0}, Rs, Bs0, Vn) ->
	%% Now for the tricky bit!
	case orddict:find(V0, Rs) of
		{ok, V1} ->        %Already have a replacement
			case unify(T, V1, Bs0) of
				{succeed, Bs1} -> {succeed, Rs, Bs1, Vn};
				fail -> fail
			end;
		error ->        %Add a replacement
			{succeed, orddict:store(V0, T, Rs), Bs0, Vn}
	end;
unify_head({_} = Var, H0, Rs0, Bs, Vn0) ->
	%% Must have an instance here.
	{H1, Rs1, Vn1} = term_instance(H0, Rs0, Vn0),
	{succeed, Rs1, add_binding(Var, H1, Bs), Vn1};
unify_head([GH | GT], [HH | HT], Rs0, Bs0, Vn0) ->
	case unify_head(deref(GH, Bs0), HH, Rs0, Bs0, Vn0) of
		{succeed, Rs1, Bs1, Vn1} -> unify_head(deref(GT, Bs1), HT, Rs1, Bs1, Vn1);
		fail -> fail
	end;
unify_head([], [], Rs, Bs, Vn) -> {succeed, Rs, Bs, Vn};
unify_head(G, H, Rs, Bs, Vn) when tuple_size(G) == tuple_size(H),
	element(1, G) == element(1, H) ->
	unify_head_args(G, H, Rs, Bs, Vn, 2, tuple_size(G));
unify_head(_G, _H, _Rs, _Bs, _Vn) -> fail.

unify_head_args(_G, _H, Rs, Bs, Vn, I, S) when I > S ->
	{succeed, Rs, Bs, Vn};
unify_head_args(G, H, Rs0, Bs0, Vn0, I, S) ->
	case unify_head(deref(element(I, G), Bs0), element(I, H), Rs0, Bs0, Vn0) of
		{succeed, Rs1, Bs1, Vn1} -> unify_head_args(G, H, Rs1, Bs1, Vn1, I + 1, S);
		fail -> fail
	end.

%% body_instance(Body, Tail, Repls, VarNum, Label) ->
%%      {Body,NewRepls,NewVarNum}.
%%  Generate a copy of a body in a form ready to be interpreted. No
%%  bindings from original variables to new variables. It can handle
%%  replacing integer variables with overlapping integer ranges. Don't
%%  check Term as it should already be checked. Use term_instance to
%%  handle goals. N.B. We have to be VERY careful never to go into the
%%  original tail as this will cause havoc.

body_instance([{{cut} = Cut, _, Last} | Gs0], Tail, Rs0, Vn0, Label) ->
	{Gs1, Rs1, Vn1} = body_instance(Gs0, Tail, Rs0, Vn0, Label),
	{[{Cut, Label, Last} | Gs1], Rs1, Vn1};
body_instance([{{disj} = Disj, L0, R0} | Gs0], Tail, Rs0, Vn0, Label) ->
	{Gs1, Rs1, Vn1} = body_instance(Gs0, Tail, Rs0, Vn0, Label),
	%% Append Gs1 directly to L and R.
	{L1, Rs2, Vn2} = body_instance(L0, Gs1, Rs1, Vn1, Label),
	{R1, Rs3, Vn3} = body_instance(R0, Gs1, Rs2, Vn2, Label),
	{[{Disj, R1} | L1], Rs3, Vn3};
body_instance([{{if_then} = IT, C0, T0, _} | Gs0], Tail, Rs0, Vn0, Label) ->
	{Gs1, Rs1, Vn1} = body_instance(Gs0, Tail, Rs0, Vn0, Label),
	{T1, Rs2, Vn2} = body_instance(T0, Gs1, Rs1, Vn1, Label),
	{C1, Rs3, Vn3} = body_instance(C0, [{{cut}, Label, true} | T1], Rs2, Vn2, Label),
	%% Append Gs1 directly to T1 to C1.
	{[{IT, Label} | C1], Rs3, Vn3};
body_instance([{{if_then_else} = ITE, C0, T0, E0, _} | Gs0], Tail, Rs0, Vn0, Label) ->
	{Gs1, Rs1, Vn1} = body_instance(Gs0, Tail, Rs0, Vn0, Label),
	{T1, Rs2, Vn2} = body_instance(T0, Gs1, Rs1, Vn1, Label),
	{C1, Rs3, Vn3} = body_instance(C0, [{{cut}, Label, true} | T1], Rs2, Vn2, Label),
	{E1, Rs4, Vn4} = body_instance(E0, Gs1, Rs3, Vn3, Label),
	{[{ITE, E1, Label} | C1], Rs4, Vn4};
body_instance([{{once} = Once, G0, _} | Gs0], Tail, Rs0, Vn0, Label) ->
	{Gs1, Rs1, Vn1} = body_instance(Gs0, Tail, Rs0, Vn0, Label),
	{G1, Rs2, Vn2} = body_instance(G0, [{{cut}, Label, true} | Gs1], Rs1, Vn1, Label),
	{[{Once, Label} | G1], Rs2, Vn2};
body_instance([G0 | Gs0], Tail, Rs0, Vn0, Label) ->
	{Gs1, Rs1, Vn1} = body_instance(Gs0, Tail, Rs0, Vn0, Label),
	{G1, Rs2, Vn2} = term_instance(G0, Rs1, Vn1),
	{[G1 | Gs1], Rs2, Vn2};
body_instance([], Tail, Rs, Vn, _Label) -> {Tail, Rs, Vn}.

%% body_term(Body, Repls, VarNum) -> {Term,NewRepls,NewVarNum}.
%%  Generate a copy of a body as a term with new, fresh unused
%%  variables. No bindings from original variables to new
%%  variables. It can handle replacing integer variables with
%%  overlapping integer ranges. Don't check Term as it should already
%%  be checked. Use orddict as there will seldom be many variables and
%%  it it fast to setup.

body_term([{{cut}, _, _} | Gs0], Rs0, Vn0) ->
	{Gs1, Rs1, Vn1} = body_term(Gs0, Rs0, Vn0),
	{body_conj('!', Gs1), Rs1, Vn1};
body_term([{{disj}, L0, R0} | Gs0], Rs0, Vn0) ->
	{Gs1, Rs1, Vn1} = body_term(Gs0, Rs0, Vn0),
	{L1, Rs2, Vn2} = body_term(L0, Rs1, Vn1),
	{R1, Rs3, Vn3} = body_term(R0, Rs2, Vn2),
	{body_conj({';', L1, R1}, Gs1), Rs3, Vn3};
body_term([{{if_then}, C0, T0, _} | Gs0], Rs0, Vn0) ->
	{Gs1, Rs1, Vn1} = body_term(Gs0, Rs0, Vn0),
	{C1, Rs2, Vn2} = body_term(C0, Rs1, Vn1),
	{T1, Rs3, Vn3} = body_term(T0, Rs2, Vn2),
	{body_conj({'->', C1, T1}, Gs1), Rs3, Vn3};
body_term([{{if_then_else}, C0, T0, E0, _} | Gs0], Rs0, Vn0) ->
	{Gs1, Rs1, Vn1} = body_term(Gs0, Rs0, Vn0),
	{C1, Rs2, Vn2} = body_term(C0, Rs1, Vn1),
	{T1, Rs3, Vn3} = body_term(T0, Rs2, Vn2),
	{E1, Rs4, Vn4} = body_term(E0, Rs3, Vn3),
	{body_conj({';', {'->', C1, T1}, E1}, Gs1), Rs4, Vn4};
body_term([{{once}, G0, _} | Gs0], Rs0, Vn0) ->
	{Gs1, Rs1, Vn1} = body_term(Gs0, Rs0, Vn0),
	{G1, Rs2, Vn2} = body_term(G0, Rs1, Vn1),
	{body_conj({once, G1}, Gs1), Rs2, Vn2};
body_term([G0 | Gs0], Rs0, Vn0) ->
	{Gs1, Rs1, Vn1} = body_term(Gs0, Rs0, Vn0),
	{G1, Rs2, Vn2} = term_instance(G0, Rs1, Vn1),
	{body_conj(G1, Gs1), Rs2, Vn2};
body_term([], Rs, Vn) -> {true, Rs, Vn}.

body_conj(L, true) -> L;
body_conj(L, R) -> {',', L, R}.

pred_ind({N, A}) -> {'/', N, A}.

%% pred_ind(N, A) -> {'/',N,A}.

%% Bindings
%% Bindings are kept in a dict where the key is the variable name.
new_bindings() -> ?BIND:new().

add_binding({V}, Val, Bs0) ->
	?BIND:store(V, Val, Bs0).

get_binding({V}, Bs) ->
	?BIND:find(V, Bs).

%% initial_goal(Goal) -> {Goal,Bindings,NewVarNum}.
%% initial_goal(Goal, Bindings, VarNum) -> {Goal,NewBindings,NewVarNum}.
%% Check term for well-formedness as an Erlog term and replace '_'
%% variables with unique numbered variables. Error on non-well-formed
%% goals.
initial_goal(Goal) -> initial_goal(Goal, new_bindings(), 0).

initial_goal({'_'}, Bs, Vn) -> {{Vn}, Bs, Vn + 1};  %Anonymous variable
initial_goal({Name} = Var0, Bs, Vn) when is_atom(Name) ->
	case get_binding(Var0, Bs) of
		{ok, Var1} -> {Var1, Bs, Vn};
		error ->
			Var1 = {Vn},
			{Var1, add_binding(Var0, Var1, Bs), Vn + 1}
	end;
initial_goal([H0 | T0], Bs0, Vn0) ->
	{H1, Bs1, Vn1} = initial_goal(H0, Bs0, Vn0),
	{T1, Bs2, Vn2} = initial_goal(T0, Bs1, Vn1),
	{[H1 | T1], Bs2, Vn2};
initial_goal([], Bs, Vn) -> {[], Bs, Vn};
initial_goal(S, Bs0, Vn0) when ?IS_FUNCTOR(S) ->
	As0 = tl(tuple_to_list(S)),
	{As1, Bs1, Vn1} = initial_goal(As0, Bs0, Vn0),
	{list_to_tuple([element(1, S) | As1]), Bs1, Vn1};
initial_goal(T, Bs, Vn) when ?IS_ATOMIC(T) -> {T, Bs, Vn};
initial_goal(T, _Bs, _Vn) -> erlog_errors:type_error(callable, T).