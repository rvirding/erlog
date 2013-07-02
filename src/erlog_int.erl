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

-module(erlog_int).

%% Main interface functions.
-export([prove_goal/2,prove_body/5,fail/2]).
-export([built_in_db/0]).
%% Utilities.
-export([new_bindings/0,deref/2,dderef/2,dderef_list/2,functor/1]).
-export([add_binding/3,unify/3,unify_prove_body/7]).
-export([asserta_clause/2,assertz_clause/2,abolish_clauses/2]).
-export([add_compiled_proc/4]).
-export([expand_term/1]).

%%-compile(export_all).

-import(lists, [map/2,foldl/3]).

%% Some standard type macros.

%% The old is_constant/1 ?
-define(IS_CONSTANT(T), (not (is_tuple(T) orelse is_list(T)))).

%% -define(IS_ATOMIC(T), (is_atom(T) orelse is_number(T) orelse (T == []))).
%% -define(IS_ATOMIC(T), (not (is_tuple(T) orelse (is_list(T) andalso T /= [])))).

%% Define the database to use. ONE of the follwing must be defined.

%%-define(ETS,true).
%%-define(DB, orddict).
-define(DB, dict).

%% built_in_db() -> Database.
%% Create an initial clause database containing the built-in
%% predicates and predefined library predicates.

built_in_db() ->
    Db0 = new_db(),
    %% First add the Erlang built-ins.
    Db1 = foldl(fun (Head, Db) -> add_built_in(Head, Db) end, Db0,
		[
		 %% Logic and control.
		 {call,{1}},
		 {',',{1},{2}},
		 '!',
		 {';',{1},{2}},
		 fail,
		 {'->',{1},{2}},
		 {'\\+',{1}},
		 {once,{1}},
		 repeat,
		 true,
		 %% Term creation and decomposition.
		 {arg,{1},{2},{3}},
		 {copy_term,{1},{2}},
		 {functor,{1},{2},{3}},
		 {'=..',{1},{2}},
		 %% Arithmetic evaluation and comparison
		 {'>',{1},{2}},
		 {'>=',{1},{2}},
		 {'=:=',{1},{2}},
		 {'=\\=',{1},{2}},
		 {'<',{1},{2}},
		 {'=<',{1},{2}},
		 {'is',{1},{2}},
		 %% Atom processing.
		 {atom_chars,{1},{2}},
		 {atom_length,{1},{2}},
		 %% Clause creation and destruction.
		 {abolish,{1}},
		 {assert,{1}},
		 {asserta,{1}},
		 {assertz,{1}},
		 {retract,{1}},
		 {retractall,{1}},
		 %% Clause retrieval and information.
		 {clause,{1},{2}},
		 {current_predicate,{1}},
		 {predicate_property,{1},{2}},
		 %% Type testing.
		 {atom,{1}},
		 {atomic,{1}},
		 {compound,{1}},
		 {integer,{1}},
		 {float,{1}},
		 {number,{1}},
		 {nonvar,{1}},
		 {var,{1}},
		 %% All solutions
		 %% Term unification and comparison
		 {'@>',{1},{2}},
		 {'@>=',{1},{2}},
		 {'==',{1},{2}},
		 {'\\==',{1},{2}},
		 {'@<',{1},{2}},
		 {'@=<',{1},{2}},
		 {'=',{1},{2}},
		 {'\\=',{1},{2}},
		 %% External interface
		 {ecall,{1},{2}},
		 %% Non-standard but useful
		 {sort,{1},{2}},
		 {expand_term,{1},{2}},
		 {display,{1}}
		]),
    %% Next the interpreted built-ins.
    Db2 = foldl(fun (Clause, Db) -> assertz_clause(Clause, Db) end, Db1,
 		[
		 {'C',[{1}|{2}],{1},{2}},		%For DCGs
		 {':-',{phrase,{1},{2}},{phrase,{1},{2},[]}},
		 {':-',{phrase,{1},{2},{3}},
		  {',',{'=..',{1},{4}},{',',{append,{4},[{2},{3}],{5}},
					{',',{'=..',{6},{5}},{6}}}}}
 		]),
    %% Compiled built-ins and common libray.
    Db3 = foldl(fun ({Head,M,F}, Db) ->
			add_compiled_proc(Head, M, F, Db) end, Db2,
		[
		 %%{{app,{1},{2},{3}},user_pl,app_3},
		 %%{{rev,{1},{2}},user_pl,rev_2},
		 %%{{mem,{1},{2}},user_pl,mem_2}
		]),
    %% Finally some interpreted common library.
    foldl(fun (Clause, Db) -> assertz_clause(Clause, Db) end, Db3,
	  [
	   {append,[],{1},{1}},
	   {':-',{append,[{1}|{2}],{3},[{1}|{4}]},{append,{2},{3},{4}}},
	   {insert,{1},{2},[{2}|{1}]},
	   {':-',{insert,[{1}|{2}],{3},[{1}|{4}]},{insert,{2},{3},{4}}},
	   {member,{1},[{1}|{2}]},
	   {':-',{member,{1},[{2}|{3}]},{member,{1},{3}}}
	  ]).

%% Define the choice point record
-record(cp, {type,label,data,next,bs,vn}).
-record(cut, {label,next}).

%% prove_goal(Goal, Database) -> Succeed | Fail.
%% This is the main entry point into the interpreter. Check that
%% everything is consistent then prove the goal as a call.

prove_goal(Goal0, Db) ->
    %% put(erlog_cut, orddict:new()),
    %% put(erlog_cps, orddict:new()),
    %% put(erlog_var, orddict:new()),
    %% Check term and build new instance of term with bindings.
    {Goal1,Bs,Vn} = initial_goal(Goal0),
    prove_body([{call,Goal1}], [], Bs, Vn, Db).

-define(FAIL(Bs, Cps, Db),
	begin
	    put(erlog_cps, orddict:update_counter(length(Cps), 1, get(erlog_cps))),
	    put(erlog_var, orddict:update_counter(dict:size(Bs), 1, get(erlog_var))),
	    fail(Cps, Db)
	end).
-undef(FAIL).
-define(FAIL(Bs, Cps, Db), fail(Cps, Db)).

%% prove_goal(Goal, NextGoal, ChoicePoints, Bindings, VarNum, Database) ->
%%	{succeed,ChoicePoints,NewBindings,NewVarNum,NewDatabase} |
%%      {fail,NewDatabase}.
%% Prove one goal. We seldom return succeed here but usually go directly to
%% to NextGoal.
%% Handle built-in predicates here. RTFM for a description of the
%% built-ins. Hopefully we do the same.

%% Logic and control. Conjunctions are handled in prove_body and true
%% has been compiled away.
prove_goal({call,G}, Next0, Cps, Bs, Vn, Db) ->
    %% Only add cut CP to Cps if goal contains a cut.
    Label = Vn,
    case check_goal(G, Next0, Bs, Db, false, Label) of
	{Next1,true} ->
	    %% Must increment Vn to avoid clashes!!!
	    Cut = #cut{label=Label},
	    prove_body(Next1, [Cut|Cps], Bs, Vn+1, Db);
	{Next1,false} -> prove_body(Next1, Cps, Bs, Vn+1, Db)
    end;
prove_goal({{cut},Label,Last}, Next, Cps, Bs, Vn, Db) ->
    %% Cut succeeds and trims back to cut ancestor.
    cut(Label, Last, Next, Cps, Bs, Vn, Db);
prove_goal({{disj},R}, Next, Cps, Bs, Vn, Db) ->
    %% There is no L here, it has already been prepended to Next.
    Cp = #cp{type=disjunction,next=R,bs=Bs,vn=Vn},
    prove_body(Next, [Cp|Cps], Bs, Vn, Db);
prove_goal(fail, _Next, Cps, _Bs, _Vn, Db) -> ?FAIL(_Bs, Cps, Db);
prove_goal({{if_then},Label}, Next, Cps, Bs, Vn, Db) ->
    %% We effetively implement ( C -> T ) with ( C, !, T ) but cuts in
    %% C are local to C.
    %% There is no ( C, !, T ) here, it has already been prepended to Next.
    %%io:fwrite("PG(->): ~p\n", [{Next}]),
    Cut = #cut{label=Label},
    prove_body(Next, [Cut|Cps], Bs, Vn, Db);
prove_goal({{if_then_else},Else,Label}, Next, Cps, Bs, Vn, Db) ->
    %% Need to push a choicepoint to fail back to inside Cond and a cut
    %% to cut back to before Then when Cond succeeds. #cp{type=if_then_else}
    %% functions as both as is always removed whatever the outcome.
    %% There is no ( C, !, T ) here, it has already been prepended to Next.
    Cp = #cp{type=if_then_else,label=Label,next=Else,bs=Bs,vn=Vn},
    %%io:fwrite("PG(->;): ~p\n", [{Next,Else,[Cp|Cps]}]),
    prove_body(Next, [Cp|Cps], Bs, Vn, Db);
prove_goal({'\\+',G}, Next0, Cps, Bs, Vn, Db) ->
    %% We effectively implementing \+ G with ( G -> fail ; true ).
    Label = Vn,
    {Next1,_} = check_goal(G, [{{cut},Label,true},fail], Bs, Db, true, Label),
    Cp = #cp{type=if_then_else,label=Label,next=Next0,bs=Bs,vn=Vn},
    %%io:fwrite("PG(\\+): ~p\n", [{G1,[Cp|Cps]]),
    %% Must increment Vn to avoid clashes!!!
    prove_body(Next1, [Cp|Cps], Bs, Vn+1, Db);
prove_goal({{once},Label}, Next, Cps, Bs, Vn, Db) ->
    %% We effetively implement once(G) with ( G, ! ) but cuts in
    %% G are local to G.
    %% There is no ( G, ! ) here, it has already been prepended to Next.
    Cut = #cut{label=Label},
    prove_body(Next, [Cut|Cps], Bs, Vn, Db);
prove_goal(repeat, Next, Cps, Bs, Vn, Db) ->
    Cp = #cp{type=disjunction,next=[repeat|Next],bs=Bs,vn=Vn},
    prove_body(Next, [Cp|Cps], Bs, Vn, Db);
%% Term unification and comparison
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
prove_goal({'=',L,R}, Next, Cps, Bs, Vn, Db) ->
    unify_prove_body(L, R, Next, Cps, Bs, Vn, Db);
prove_goal({'\\=',L,R}, Next, Cps, Bs0, Vn, Db) ->
    case unify(L, R, Bs0) of
	{succeed,_Bs1} -> ?FAIL(_Bs1, Cps, Db);
	fail -> prove_body(Next, Cps, Bs0, Vn, Db)
    end;
%% Type testing.
prove_goal({atom,T0}, Next, Cps, Bs, Vn, Db) ->
    case deref(T0, Bs) of
	T when is_atom(T) -> prove_body(Next, Cps, Bs, Vn, Db);
	_Other -> ?FAIL(Bs, Cps, Db)
    end;
prove_goal({atomic,T0}, Next, Cps, Bs, Vn, Db) ->
    case deref(T0, Bs) of
	T when ?IS_CONSTANT(T) ; T == [] -> prove_body(Next, Cps, Bs, Vn, Db);
	_Other -> ?FAIL(Bs, Cps, Db)
    end;
prove_goal({compound,T0}, Next, Cps, Bs, Vn, Db) ->
    case deref(T0, Bs) of
	T when ?IS_CONSTANT(T) ; T == [] -> ?FAIL(Bs, Cps, Db);
	_Other -> prove_body(Next, Cps, Bs, Vn, Db)
    end;
prove_goal({integer,T0}, Next, Cps, Bs, Vn, Db) ->
    case deref(T0, Bs) of
	T when is_integer(T) -> prove_body(Next, Cps, Bs, Vn, Db);
	_Other -> ?FAIL(Bs, Cps, Db)
    end;
prove_goal({float,T0}, Next, Cps, Bs, Vn, Db) ->
    case deref(T0, Bs) of
	T when is_float(T) -> prove_body(Next, Cps, Bs, Vn, Db);
	_Other -> ?FAIL(Bs, Cps, Db)
    end;
prove_goal({number,T0}, Next, Cps, Bs, Vn, Db) ->
    case deref(T0, Bs) of
	T when is_number(T) -> prove_body(Next, Cps, Bs, Vn, Db);
	_Other -> ?FAIL(Bs, Cps, Db)
    end;
prove_goal({nonvar,T0}, Next, Cps, Bs, Vn, Db) ->
    case deref(T0, Bs) of
	{_} -> ?FAIL(Bs, Cps, Db);
	_Other -> prove_body(Next, Cps, Bs, Vn, Db)
    end;
prove_goal({var,T0}, Next, Cps, Bs, Vn, Db) ->
    case deref(T0, Bs) of
	{_} -> prove_body(Next, Cps, Bs, Vn, Db);
	_Other -> ?FAIL(Bs, Cps, Db)
    end;
%% Arithmetic evalution and comparison.
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
    arith_test_prove_body('=<', L, R, Next, Cps, Bs, Vn, Db);
prove_goal({is,N,E0}, Next, Cps, Bs, Vn, Db) ->
    E = eval_arith(deref(E0, Bs), Bs, Db),
    unify_prove_body(N, E, Next, Cps, Bs, Vn, Db);
%% Term creation and decomposition.
prove_goal({copy_term,T0,C}, Next, Cps, Bs, Vn0, Db) ->
    %% Use term_instance to create the copy, can ignore orddict it creates.
    {T,_Nbs,Vn1} = term_instance(dderef(T0, Bs), Vn0),
    unify_prove_body(T, C, Next, Cps, Bs, Vn1, Db);
prove_goal({arg,I,Ct,A}, Next, Cps, Bs, Vn, Db) ->
    prove_arg(deref(I, Bs), deref(Ct, Bs), A, Next, Cps, Bs, Vn, Db);
prove_goal({functor,T,F,A}, Next, Cps, Bs, Vn, Db) ->
    prove_functor(dderef(T, Bs), F, A, Next, Cps, Bs, Vn, Db);
prove_goal({'=..',T,L}, Next, Cps, Bs, Vn, Db) ->
    prove_univ(dderef(T, Bs), L, Next, Cps, Bs, Vn, Db);
%% Atom processing.
prove_goal({atom_chars,A,L}, Next, Cps, Bs, Vn, Db) ->
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
	    Fun = fun ({_}) -> instantiation_error(Db);
		      (Atom) ->
			  case is_atom(Atom) andalso atom_to_list(Atom) of
			      [C] -> C;
			      _ -> type_error(character, Atom)
			  end
		  end,
	    Chars = lists:map(Fun, List),
	    Atom = list_to_atom(Chars),
	    unify_prove_body(Var, Atom, Next, Cps, Bs, Vn, Db);
	Other ->
	    %% Error #2: Atom is neither a variable nor an atom
	    type_error(atom, Other)
    end;
prove_goal({atom_length,A0,L0}, Next, Cps, Bs, Vn, Db) ->
    case dderef(A0, Bs) of
	A when is_atom(A) ->
	    Alen = length(atom_to_list(A)),	%No of chars in atom
	    case dderef(L0, Bs) of
		L when is_integer(L) ->
		    unify_prove_body (Alen, L, Next, Cps, Bs, Vn, Db);
		{_}=Var ->
		    unify_prove_body (Alen, Var, Next, Cps, Bs, Vn, Db);
		Other -> type_error(integer, Other)
	    end;
	{_} -> instantiation_error(Db);
	Other -> type_error(atom, Other)
    end;
%% Clause creation and destruction.
prove_goal({abolish,Pi0}, Next, Cps, Bs, Vn, Db) ->
    case dderef(Pi0, Bs) of
	{'/',N,A} when is_atom(N), is_integer(A), A > 0 ->
	    prove_body(Next, Cps, Bs, Vn, abolish_clauses({N,A}, Db));
	Pi -> type_error(predicate_indicator, Pi, Db)
    end;
prove_goal({assert,C0}, Next, Cps, Bs, Vn, Db) ->
    C = dderef(C0, Bs),
    prove_body(Next, Cps, Bs, Vn, assertz_clause(C, Db));
prove_goal({asserta,C0}, Next, Cps, Bs, Vn, Db) ->
    C = dderef(C0, Bs),
    prove_body(Next, Cps, Bs, Vn, asserta_clause(C, Db));
prove_goal({assertz,C0}, Next, Cps, Bs, Vn, Db) ->
    C = dderef(C0, Bs),
    prove_body(Next, Cps, Bs, Vn, assertz_clause(C, Db));
prove_goal({retract,C0}, Next, Cps, Bs, Vn, Db) ->
    C = dderef(C0, Bs),
    prove_retract(C, Next, Cps, Bs, Vn, Db);
%% Clause retrieval and information
prove_goal({clause,H0,B}, Next, Cps, Bs, Vn, Db) ->
    H1 = dderef(H0, Bs),
    prove_clause(H1, B, Next, Cps, Bs, Vn, Db);
prove_goal({current_predicate,Pi0}, Next, Cps, Bs, Vn, Db) ->
    Pi = dderef(Pi0, Bs),
    prove_current_predicate(Pi, Next, Cps, Bs, Vn, Db);
prove_goal({predicate_property,H0,P}, Next, Cps, Bs, Vn, Db) ->
    H = dderef(H0, Bs),
    case catch get_procedure_type(functor(H), Db) of
	built_in -> unify_prove_body(P, built_in, Next, Cps, Bs, Vn, Db);
	compiled -> unify_prove_body(P, compiled, Next, Cps, Bs, Vn, Db);
	interpreted -> unify_prove_body(P, interpreted, Next, Cps, Bs, Vn, Db);
	undefined -> ?FAIL(Bs, Cps, Db);
	{erlog_error,E} -> erlog_error(E, Db)
    end;	
%% External interface
prove_goal({ecall,C0,Val}, Next, Cps, Bs, Vn, Db) ->
    %% Build the initial call.
    %%io:fwrite("PG(ecall): ~p\n   ~p\n   ~p\n", [dderef(C0, Bs),Next,Cps]),
    Efun = case dderef(C0, Bs) of
	       {':',M,F} when is_atom(M), is_atom(F) ->
		   fun () -> M:F() end;
	       {':',M,{F,A}} when is_atom(M), is_atom(F) ->
		   fun () -> M:F(A) end;
	       {':',M,{F,A1,A2}} when is_atom(M), is_atom(F) ->
		   fun () -> M:F(A1,A2) end;
	       {':',M,T} when is_atom(M), is_tuple(T), size(T) >= 2,
			      is_atom(element(1, T)) ->
			  L = tuple_to_list(T),
		   fun () -> apply(M, hd(L), tl(L)) end;
	       Fun when is_function(Fun) -> Fun;
	       Other -> type_error(callable, Other, Db)
	   end,
    prove_ecall(Efun, Val, Next, Cps, Bs, Vn, Db);
%% Non-standard but useful.
prove_goal({sort,L0,S}, Next, Cps, Bs, Vn, Db) ->
    case catch lists:usort(dderef_list(L0, Bs)) of
	{erlog_error,E} -> erlog_error(E, Db);
	L1 -> unify_prove_body(S, L1, Next, Cps, Bs, Vn, Db)
    end;
prove_goal({expand_term,T0,Exp}, Next, Cps, Bs, Vn0, Db) ->
    T1 = dderef(T0, Bs),
    {E,Vn1} = expand_term(T1, Vn0),
    unify_prove_body(E, Exp, Next, Cps, Bs, Vn1, Db);
prove_goal({display,T}, Next, Cps, Bs, Vn, Db) ->
    %% A very simple display procedure.
    io:fwrite("~p\n", [dderef(T, Bs)]),
    prove_body(Next, Cps, Bs, Vn, Db);
%% Now look up the database.
prove_goal(G, Next, Cps, Bs, Vn, Db) ->
    %%io:fwrite("PG: ~p\n    ~p\n    ~p\n", [dderef(G, Bs),Next,Cps]),
    case catch get_procedure(functor(G), Db) of
	{code,{Mod,Func}} -> Mod:Func(G, Next, Cps, Bs, Vn, Db);
	{clauses,Cs} -> prove_goal_clauses(G, Cs, Next, Cps, Bs, Vn, Db);
	undefined -> ?FAIL(Bs, Cps, Db);
	%% Getting built_in here is an error!
	{erlog_error,E} -> erlog_error(E, Db)	%Fill in more error data
    end.

fail_disjunction(#cp{next=Next,bs=Bs,vn=Vn}, Cps, Db) ->
    prove_body(Next, Cps, Bs, Vn, Db).

fail_if_then_else(#cp{next=Next,bs=Bs,vn=Vn}, Cps, Db) ->
    prove_body(Next, Cps, Bs, Vn, Db).

%% fail(ChoicePoints, Database) -> {fail,Database}.
%% cut(Label, Last, Next, ChoicePoints, Bindings, VarNum, Database) -> void.
%%
%% The functions which manipulate the choice point stack.
%% fail backtracks to next choicepoint skipping cut labels
%% cut steps backwards over choice points until matching cut.

fail([#cp{type=goal_clauses}=Cp|Cps], Db) ->
    fail_goal_clauses(Cp, Cps, Db);
fail([#cp{type=disjunction}=Cp|Cps], Db) ->
    fail_disjunction(Cp, Cps, Db);
fail([#cp{type=if_then_else}=Cp|Cps], Db) ->
    fail_if_then_else(Cp, Cps, Db);
fail([#cp{type=clause}=Cp|Cps], Db) ->
    fail_clause(Cp, Cps, Db);
fail([#cp{type=retract}=Cp|Cps], Db) ->
    fail_retract(Cp, Cps, Db);
fail([#cp{type=current_predicate}=Cp|Cps], Db) ->
    fail_current_predicate(Cp, Cps, Db);
fail([#cp{type=ecall}=Cp|Cps], Db) ->
    fail_ecall(Cp, Cps, Db);
fail([#cp{type=compiled,data=F}=Cp|Cps], Db) ->
    F(Cp, Cps, Db);
fail([#cut{}|Cps], Db) ->
    fail(Cps, Db);				%Fail over cut points.
fail([], Db) -> {fail,Db}.    

cut(Label, Last, Next, [#cut{label=Label}|Cps]=Cps0, Bs, Vn, Db) ->
    if  Last -> prove_body(Next, Cps, Bs, Vn, Db);
	true -> prove_body(Next, Cps0, Bs, Vn, Db)
    end;
cut(Label, Last, Next, [#cp{type=if_then_else,label=Label}|Cps]=Cps0, Bs, Vn, Db) ->
    if  Last -> prove_body(Next, Cps, Bs, Vn, Db);
	true -> prove_body(Next, Cps0, Bs, Vn, Db)
    end;
cut(Label, Last, Next, [#cp{type=goal_clauses,label=Label}=Cp|Cps], Bs, Vn, Db) ->
    cut_goal_clauses(Last, Next, Cp, Cps, Bs, Vn, Db);
cut(Label, Last, Next, [_Cp|Cps], Bs, Vn, Db) ->
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
	{_} -> instantiation_error(Db);		%Must have something to call
	G1 ->
	    case catch {ok,well_form_goal(G1, Next, Cut, Label)} of
		{erlog_error,E} -> erlog_error(E, Db);
		{ok,GC} -> GC			%Body and cut
	    end
    end.

%% unify_prove_body(Term1, Term2, Next, ChoicePoints, Bindings, VarNum, Database) ->
%%	void.
%% Unify Term1 = Term2, on success prove body Next else fail.

unify_prove_body(T1, T2, Next, Cps, Bs0, Vn, Db) ->
    case unify(T1, T2, Bs0) of
	{succeed,Bs1} -> prove_body(Next, Cps, Bs1, Vn, Db);
	fail -> ?FAIL(Bs0, Cps, Db)
    end.

%% unify_prove_body(A1, B1, A2, B2, Next, ChoicePoints, Bindings, VarNum, Database) ->
%%	void.
%% Unify A1 = B1, A2 = B2, on success prove body Next else fail.

unify_prove_body(A1, B1, A2, B2, Next, Cps, Bs0, Vn, Db) ->
    case unify(A1, B1, Bs0) of
	{succeed,Bs1} -> unify_prove_body(A2, B2, Next, Cps, Bs1, Vn, Db);
	fail -> ?FAIL(Bs0, Cps, Db)
    end.

%% term_test_prove_body(Test, Left, Right, Next, ChoicePoints, Bindings, Varnum, Database) ->
%%      void.

term_test_prove_body(Test, L, R, Next, Cps, Bs, Vn, Db) ->
    case erlang:Test(dderef(L, Bs), dderef(R, Bs)) of
	true -> prove_body(Next, Cps, Bs, Vn, Db);
	false -> ?FAIL(Bs, Cps, Db)
    end.

%% arith_test_prove_body(Test, Left, Right, Next, ChoicePoints, Bindings, VarNum, Database) ->
%%	void.

arith_test_prove_body(Test, L, R, Next, Cps, Bs, Vn, Db) ->
    case erlang:Test(eval_arith(deref(L, Bs), Bs, Db),
		     eval_arith(deref(R, Bs), Bs, Db)) of
	true -> prove_body(Next, Cps, Bs, Vn, Db);
	false -> ?FAIL(Bs, Cps, Db)
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
  when is_integer(I), is_tuple(Ct), size(Ct) >= 2 ->
    if  I > 1, I + 1 =< size(Ct) ->
	    unify_prove_body(element(I+1, Ct), A, Next, Cps, Bs, Vn, Db);
	true -> {fail,Db}
    end;
prove_arg(I, Ct, _, _, _, _, _, Db) ->
    %%Type failure just generates an error.
    if  not(is_integer(I)) -> type_error(integer, I, Db);
	true -> type_error(compound, Ct, Db)
    end.

%% prove_functor(Term, Functor, Arity, Next, ChoicePoints, Bindings, VarNum, Database) -> void.
%%  Prove the call functor(T, F, A), Term has been dereferenced.

prove_functor(T, F, A, Next, Cps, Bs, Vn, Db)
  when is_tuple(T), size(T) >= 2 ->
    unify_prove_body(F, element(1, T), A, size(T)-1, Next, Cps, Bs, Vn, Db);
prove_functor(T, F, A, Next, Cps, Bs, Vn, Db)
  when ?IS_CONSTANT(T) ; T == [] ->
    unify_prove_body(F, T, A, 0, Next, Cps, Bs, Vn, Db);
prove_functor([_|_], F, A, Next, Cps, Bs, Vn, Db) ->
    %% Just the top level here.
    unify_prove_body(F, '.', A, 2, Next, Cps, Bs, Vn, Db);
prove_functor({_}=Var, F0, A0, Next, Cps, Bs0, Vn0, Db) ->
    case {dderef(F0, Bs0),dderef(A0, Bs0)} of
	{'.',2} ->				%He, he, he!
	    Bs1 = add_binding(Var, [{Vn0}|{Vn0+1}], Bs0),
	    prove_body(Next, Cps, Bs1, Vn0+2, Db);
	{F1,0} when ?IS_CONSTANT(F1) ->
	    Bs1 = add_binding(Var, F1, Bs0),
	    prove_body(Next, Cps, Bs1, Vn0, Db);
	{F1,A1} when is_atom(F1), is_integer(A1), A1 > 0 ->
	    As = make_vars(A1, Vn0),
	    Bs1 = add_binding(Var, list_to_tuple([F1|As]), Bs0),
	    prove_body(Next, Cps, Bs1, Vn0+A1, Db); %!!!
	%% Now the error cases.
	{{_},_} -> instantiation_error(Db);
	{F1,A1} when is_atom(F1) -> type_error(integer, A1, Db);
	{F1,_} -> type_error(atom, F1, Db)
    end.

%% prove_univ(Term, List, Next, ChoicePoints, Bindings, VarNum, Database) -> void.
%%  Prove the goal Term =.. List, Term has already been dereferenced.

prove_univ(T, L, Next, Cps, Bs, Vn, Db) when is_tuple(T), size(T) >= 2 ->
    Es = tuple_to_list(T),
    unify_prove_body(Es, L, Next, Cps, Bs, Vn, Db);
prove_univ(T, L, Next, Cps, Bs, Vn, Db) when ?IS_CONSTANT(T) ; T == [] ->
    unify_prove_body([T], L, Next, Cps, Bs, Vn, Db);
prove_univ([Lh|Lt], L, Next, Cps, Bs, Vn, Db) ->
    %% He, he, he!
    unify_prove_body(['.',Lh,Lt], L, Next, Cps, Bs, Vn, Db);
prove_univ({_}=Var, L, Next, Cps, Bs0, Vn, Db) ->
    case dderef(L, Bs0) of
	['.',Lh,Lt] ->				%He, he, he!
	    Bs1 = add_binding(Var, [Lh|Lt], Bs0),
	    prove_body(Next, Cps, Bs1, Vn, Db);
	[A] when ?IS_CONSTANT(A) ->
	    Bs1 = add_binding(Var, A, Bs0),
	    prove_body(Next, Cps, Bs1, Vn, Db);
	[F|As] when is_atom(F), length(As) > 0 ->
	    Bs1 = add_binding(Var, list_to_tuple([F|As]), Bs0),
	    prove_body(Next, Cps, Bs1, Vn, Db);
	%% Now the error cases.
	[{_}|_] -> instantiation_error(Db);
	Other -> type_error(list, Other, Db)
end.

%% prove_ecall(Generator, Value, Next, ChoicePoints, Bindings, VarNum, Database) ->
%%	void.
%% Call an external (Erlang) generator and handle return value, either
%% succeed or fail.

prove_ecall(Efun, Val, Next, Cps, Bs, Vn, Db) ->
    case Efun() of
	{succeed,Ret,Cont} ->			%Succeed and more choices
	    Cp = #cp{type=ecall,data={Cont,Val},next=Next,bs=Bs,vn=Vn},
	    unify_prove_body(Val, Ret, Next, [Cp|Cps], Bs, Vn, Db);
	{succeed_last,Ret} ->			%Succeed but last choice
	    unify_prove_body(Val, Ret, Next, Cps, Bs, Vn, Db);
	fail -> ?FAIL(Bs, Cps, Db)			%No more
    end.

fail_ecall(#cp{data={Efun,Val},next=Next,bs=Bs,vn=Vn}, Cps, Db) ->
    prove_ecall(Efun, Val, Next, Cps, Bs, Vn, Db).    

%% prove_clause(Head, Body, Next, ChoicePoints, Bindings, VarNum, DataBase) ->
%%      void.
%% Unify clauses matching with functor from Head with both Head and Body.

prove_clause(H, B, Next, Cps, Bs, Vn, Db) ->
    Functor = functor(H),
    case get_procedure(Functor, Db) of
	{clauses,Cs} -> unify_clauses(H, B, Cs, Next, Cps, Bs, Vn, Db);
	{code,_} ->
	    permission_error(access, private_procedure, pred_ind(Functor), Db);
	built_in ->
	    permission_error(access, private_procedure, pred_ind(Functor), Db);
	undefined -> ?FAIL(Bs, Cps, Db)
    end.

%% unify_clauses(Head, Body, Clauses, Next, ChoicePoints, Bindings, VarNum, Database) ->
%%      void.
%% Try to unify Head and Body using Clauses which all have the same functor.

unify_clauses(Ch, Cb, [C], Next, Cps, Bs0, Vn0, Db) ->
    %% No choice point on last clause
    case unify_clause(Ch, Cb, C, Bs0, Vn0) of
	{succeed,Bs1,Vn1} -> prove_body(Next, Cps, Bs1, Vn1, Db);
	fail -> ?FAIL(Bs0, Cps, Db)
    end;
unify_clauses(Ch, Cb, [C|Cs], Next, Cps, Bs0, Vn0, Db) ->
    case unify_clause(Ch, Cb, C, Bs0, Vn0) of
	{succeed,Bs1,Vn1} ->
	    Cp = #cp{type=clause,data={Ch,Cb,Cs},next=Next,bs=Bs0,vn=Vn0},
	    prove_body(Next, [Cp|Cps], Bs1, Vn1, Db);
	fail -> unify_clauses(Ch, Cb, Cs, Next, Cps, Bs0, Vn0, Db)
    end;
unify_clauses(_Ch, _Cb, [], _Next, Cps,_Bs, _Vn, Db) -> ?FAIL(_Bs, Cps, Db).

unify_clause(Ch, Cb, {_Tag,H0,{B0,_}}, Bs0, Vn0) ->
    {H1,Rs1,Vn1} = term_instance(H0, Vn0),	%Unique vars on head first
    case unify(Ch, H1, Bs0) of
	{succeed,Bs1} ->
	    {B1,_Rs2,Vn2} = body_term(B0, Rs1, Vn1),	%Now we need the rest
	    case unify(Cb, B1, Bs1) of
		{succeed,Bs2} -> {succeed,Bs2,Vn2};
		fail -> fail
	    end;
	fail -> fail
    end.

fail_clause(#cp{data={Ch,Cb,Cs},next=Next,bs=Bs,vn=Vn}, Cps, Db) ->
    unify_clauses(Ch, Cb, Cs, Next, Cps, Bs, Vn, Db).

%% prove_current_predicate(PredInd, Next, ChoicePoints, Bindings, VarNum, DataBase) ->
%%      void.
%% Match functors of existing user (interpreted) predicate with PredInd.

prove_current_predicate(Pi, Next, Cps, Bs, Vn, Db) ->
    case Pi of
	{'/',_,_} -> ok;
	{_} -> ok;
	Other -> type_error(predicate_indicator, Other)
    end,
    Fs = get_interp_functors(Db),
    prove_predicates(Pi, Fs, Next, Cps, Bs, Vn, Db).

prove_predicates(Pi, [F|Fs], Next, Cps, Bs, Vn, Db) ->
    Cp = #cp{type=current_predicate,data={Pi,Fs},next=Next,bs=Bs,vn=Vn},
    unify_prove_body(Pi, pred_ind(F), Next, [Cp|Cps], Bs, Vn, Db);
prove_predicates(_Pi, [], _Next, Cps, _Bs, _Vn, Db) -> ?FAIL(_Bs, Cps, Db).

fail_current_predicate(#cp{data={Pi,Fs},next=Next,bs=Bs,vn=Vn}, Cps, Db) ->
    prove_predicates(Pi, Fs, Next, Cps, Bs, Vn, Db).

%% prove_goal_clauses(Goal, Clauses, Next, ChoicePoints, Bindings, VarNum, Database) ->
%%      void.
%% Try to prove Goal using Clauses which all have the same functor.

prove_goal_clauses(G, [C], Next, Cps, Bs, Vn, Db) ->
    %% Must be smart here and test whether we need to add a cut point.
    %% C has the structure {Tag,Head,{Body,BodyHasCut}}.
    case element(2, element(3, C)) of
	true ->
	    Cut = #cut{label=Vn},
	    prove_goal_clause(G, C, Next, [Cut|Cps], Bs, Vn, Db);
	false ->
	    prove_goal_clause(G, C, Next, Cps, Bs, Vn, Db)
    end;
    %% prove_goal_clause(G, C, Next, Cps, Bs, Vn, Db);
prove_goal_clauses(G, [C|Cs], Next, Cps, Bs, Vn, Db) ->
    Cp = #cp{type=goal_clauses,label=Vn,data={G,Cs},next=Next,bs=Bs,vn=Vn},
    prove_goal_clause(G, C, Next, [Cp|Cps], Bs, Vn, Db);
prove_goal_clauses(_G, [], _Next, Cps,_Bs, _Vn, Db) -> ?FAIL(_Bs, Cps ,Db).

prove_goal_clause(G, {_Tag,H0,{B0,_}}, Next, Cps, Bs0, Vn0, Db) ->
    %% io:fwrite("PGC1: ~p\n", [{G,H0,B0}]),
    Label = Vn0,
    case unify_head(G, H0, Bs0, Vn0+1) of
	{succeed,Rs0,Bs1,Vn1} ->
	    %% io:fwrite("PGC2: ~p\n", [{Rs0}]),
	    {B1,_Rs2,Vn2} = body_instance(B0, Next, Rs0, Vn1, Label),
	    %% io:fwrite("PGC3: ~p\n", [{B1,Next,Cps}]),
	    prove_body(B1, Cps, Bs1, Vn2, Db);
	fail -> ?FAIL(Bs0, Cps, Db)
    end.

fail_goal_clauses(#cp{data={G,Cs},next=Next,bs=Bs,vn=Vn}, Cps, Db) ->
    prove_goal_clauses(G, Cs, Next, Cps, Bs, Vn, Db).

%% cut_goal_clauses(Last, Next, Cp, Cps, Bs, Vn, Db).

cut_goal_clauses(true, Next, #cp{label=_}, Cps, Bs, Vn, Db) ->
    %% Just remove the choice point completely and continue.
    prove_body(Next, Cps, Bs, Vn, Db);
cut_goal_clauses(false, Next, #cp{label=L}, Cps, Bs, Vn, Db) ->
    %% Replace choice point with cut point then continue.
    Cut = #cut{label=L},
    prove_body(Next, [Cut|Cps], Bs, Vn, Db).

%% prove_retract(Clause, Next, ChoicePoints, Bindings, VarNum, Database) ->
%%      void.
%% Retract clauses in database matching Clause.

prove_retract({':-',H,B}, Next, Cps, Bs, Vn, Db) ->
    prove_retract(H, B, Next, Cps, Bs, Vn, Db);
prove_retract(H, Next, Cps, Bs, Vn, Db) ->
    prove_retract(H, true, Next, Cps, Bs, Vn, Db).

prove_retract(H, B, Next, Cps, Bs, Vn, Db) ->
    Functor = functor(H),
    case get_procedure(Functor, Db) of
	{clauses,Cs} -> retract_clauses(H, B, Cs, Next, Cps, Bs, Vn, Db);
	{code,_} ->
	    permission_error(modify, static_procedure, pred_ind(Functor), Db);
	built_in ->
	    permission_error(modify, static_procedure, pred_ind(Functor), Db);
	undefined -> ?FAIL(Bs, Cps, Db)
    end.

%% retract_clauses(Head, Body, Clauses, Next, ChoicePoints, Bindings, VarNum, Database) ->
%%      void.
%% Try to retract Head and Body using Clauses which all have the same functor.

retract_clauses(Ch, Cb, [C|Cs], Next, Cps, Bs0, Vn0, Db0) ->
    case unify_clause(Ch, Cb, C, Bs0, Vn0) of
	{succeed,Bs1,Vn1} ->
	    %% We have found a right clause so now retract it.
	    Db1 = retract_clause(functor(Ch), element(1, C), Db0),
	    Cp = #cp{type=retract,data={Ch,Cb,Cs},next=Next,bs=Bs0,vn=Vn0},
	    prove_body(Next, [Cp|Cps], Bs1, Vn1, Db1);
	fail -> retract_clauses(Ch, Cb, Cs, Next, Cps, Bs0, Vn0, Db0)
    end;
retract_clauses(_Ch, _Cb, [], _Next, Cps, _Bs, _Vn, Db) -> ?FAIL(_Bs, Cps, Db).

fail_retract(#cp{data={Ch,Cb,Cs},next=Next,bs=Bs,vn=Vn}, Cps, Db) ->
    retract_clauses(Ch, Cb, Cs, Next, Cps, Bs, Vn, Db).

%% prove_body(Body, ChoicePoints, Bindings, VarNum, Database) ->
%%      {succeed,ChoicePoints,NewBindings,NewVarNum,NewDatabase}.
%% Prove the goals in a body. Remove the first goal and try to prove
%% it. Return when there are no more goals. This is how proving a
%% goal/body succeeds.

prove_body([G|Gs], Cps, Bs0, Vn0, Db0) ->
    %%io:fwrite("PB: ~p\n", [{G,Gs,Cps}]),
    prove_goal(G, Gs, Cps, Bs0, Vn0, Db0);
prove_body([], Cps, Bs, Vn, Db) ->
    %%io:fwrite("Cps: ~p\nCut: ~p\nVar: ~p\nVar: ~p\n",
    %%      [get(erlog_cps),get(erlog_cut),get(erlog_var),dict:size(Bs)]),
    %%io:fwrite("PB: ~p\n", [Cps]),
    {succeed,Cps,Bs,Vn,Db}.			%No more body

%% unify(Term, Term, Bindings) -> {succeed,NewBindings} | fail.
%% Unify two terms with a set of bindings.

unify(T10, T20, Bs0) ->
    case {deref(T10, Bs0),deref(T20, Bs0)} of
	{T1,T2} when ?IS_CONSTANT(T1), T1 == T2 ->
	    {succeed,Bs0};
	{{V},{V}} -> {succeed,Bs0};
	{{_}=Var,T2} -> {succeed,add_binding(Var, T2, Bs0)};
	{T1,{_}=Var} -> {succeed,add_binding(Var, T1, Bs0)};
	{[H1|T1],[H2|T2]} ->
	    case unify(H1, H2, Bs0) of
		{succeed,Bs1} -> unify(T1, T2, Bs1);
		fail -> fail
	    end;
	{[],[]} -> {succeed,Bs0};
	{T1,T2} when is_tuple(T1), is_tuple(T2), size(T1) == size(T2),
		     element(1, T1) == element(1, T2) ->
	    unify_args(T1, T2, Bs0, 2, size(T1));
	_Other -> fail
    end.

unify_args(_, _, Bs, I, S) when I > S -> {succeed,Bs};
unify_args(S1, S2, Bs0, I, S) ->
    case unify(element(I, S1), element(I, S2), Bs0) of
	{succeed,Bs1} -> unify_args(S1, S2, Bs1, I+1, S);
	fail -> fail
    end.

%% eval_arith(ArithExpr, Bindings, Database) -> Number.
%% Evaluate an arithmetic expression, include the database for errors.
%% Dereference each level as we go, might fail so save some work.
%% Must be called deferenced.

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
eval_arith({_}, _Bs, Db) -> instantiation_error(Db);
eval_arith(N, _Bs, Db) when is_tuple(N) ->
    type_error(evaluable, pred_ind(element(1, N), size(N)-1), Db);
eval_arith([_|_], _Bs, Db) -> type_error(evaluable, pred_ind('.', 2), Db);
eval_arith(O, _Bs, Db) -> type_error(evaluable, O, Db).

%% eval_int(IntegerExpr, Bindings, Database) -> Integer.
%% Evaluate an integer expression, include the database for errors.

eval_int(E0, Bs, Db) ->
    E = eval_arith(E0, Bs, Db),
    if  is_integer(E) -> E;
	true -> type_error(integer, E, Db)
    end.

%% make_vars(Count, VarNum) -> [Var].
%% Make a list of new variables starting at VarNum.

make_vars(0, _) -> [];
make_vars(I, Vn) ->
    [{Vn}|make_vars(I-1, Vn+1)].

%% Errors

type_error(Type, Value, Db) -> erlog_error({type_error,Type,Value}, Db).
type_error(Type, Value) -> erlog_error({type_error,Type,Value}).

instantiation_error(Db) -> erlog_error(instantiation_error, Db).
instantiation_error() -> erlog_error(instantiation_error).

permission_error(Op, Type, Value, Db) ->
    erlog_error({permission_error,Op,Type,Value}, Db).

erlog_error(E, Db) -> throw({erlog_error,E,Db}).
erlog_error(E) -> throw({erlog_error,E}).

-ifdef(DB).
%% Database
%% The database is a dict where the key is the functor pair {Name,Arity}.
%% The value is: built_in |
%%		 {clauses,NextTag,[{Tag,Head,Body}]} |
%%		 {code,{Module,Function}}.
%% Built-ins are defined by the system and cannot manipulated by user
%% code.
%% We are a little paranoid here and do our best to ensure consistency
%% in the database by checking input arguments even if we know they
%% come from "good" code.

new_db() -> ?DB:new().

%% add_built_in(Head, Database) -> NewDatabase.
%% Add Head as a built-in in the database.

add_built_in(H, Db) ->
    Functor = functor(H),
    ?DB:store(Functor, built_in, Db).

%% add_compiled_proc(Head, Module, Function, Database) -> NewDatabase.
%% Add Head as a compiled procedure with code in Module:Function. No
%% checking.

add_compiled_proc(H, M, F, Db) ->
    Functor = functor(H),
    ?DB:update(Functor,
	       fun (built_in) ->
		       permission_error(modify, static_procedure, pred_ind(Functor), Db);
		   (_) -> {code,{M,F}}
	       end, {code,{M,F}}, Db).

%% assertz_clause(Clause, Database) -> NewDatabase.
%% assertz_clause(Head, Body, Database) -> NewDatabase.
%% Assert a clause into the database first checking that it is well
%% formed.

assertz_clause({':-',H,B}, Db) -> assertz_clause(H, B, Db);
assertz_clause(H, Db) -> assertz_clause(H, true, Db).

assertz_clause(Head, Body0, Db) ->
    {Functor,Body} = case catch {ok,functor(Head),
				 well_form_body(Body0, false, sture)} of
			 {erlog_error,E} -> erlog_error(E, Db);
			 {ok,F,B} -> {F,B}
		      end,
    ?DB:update(Functor,
	       fun (built_in) ->
		       permission_error(modify, static_procedure, pred_ind(Functor), Db);
		   ({code,_}) ->
		       permission_error(modify, static_procedure, pred_ind(Functor), Db);
		   ({clauses,T,Cs}) -> {clauses,T+1,Cs ++ [{T,Head,Body}]}
	       end, {clauses,1,[{0,Head,Body}]}, Db).

%% asserta_clause(Clause, Database) -> NewDatabase.
%% asserta_clause(Head, Body, Database) -> NewDatabase.
%% Assert a clause into the database first checking that it is well
%% formed.

asserta_clause({':-',H,B}, Db) -> asserta_clause(H, B, Db);
asserta_clause(H, Db) -> asserta_clause(H, true, Db).

asserta_clause(Head, Body0, Db) ->
    {Functor,Body} = case catch {ok,functor(Head),
				 well_form_body(Body0, false, sture)} of
			 {erlog_error,E} -> erlog_error(E, Db);
			 {ok,F,B} -> {F,B}
		      end,
    ?DB:update(Functor,
	       fun (built_in) ->
		       permission_error(modify, static_procedure, pred_ind(Functor), Db);
		   ({code,_}) ->
		       permission_error(modify, static_procedure, pred_ind(Functor), Db);
		   ({clauses,T,Cs}) -> {clauses,T+1,[{T,Head,Body}|Cs]}
	       end, {clauses,1,[{0,Head,Body}]}, Db).

%% retract_clause(Functor, ClauseTag, Database) -> NewDatabase.
%% Retract (remove) the clause with tag ClauseTag from the list of
%% clauses of Functor.

retract_clause(F, Ct, Db) ->
    case ?DB:find(F, Db) of
	{ok,built_in} ->
	    permission_error(modify, static_procedure, pred_ind(F), Db);
	{ok,{code,_}} ->
	    permission_error(modify, static_procedure, pred_ind(F), Db);
	{ok,{clauses,Nt,Cs}} ->
	    ?DB:store(F, {clauses,Nt,lists:keydelete(Ct, 1, Cs)}, Db);
	error -> Db				%Do nothing
    end.

%% abolish_clauses(Functor, Database) -> NewDatabase.

abolish_clauses(Func, Db) ->
    case ?DB:find(Func, Db) of
	{ok,built_in} ->
	    permission_error(modify, static_procedure, pred_ind(Func), Db);
	{ok,{code,_}} -> ?DB:erase(Func, Db);
	{ok,{clauses,_,_}} -> ?DB:erase(Func, Db);
	error -> Db				%Do nothing
    end.

%% get_procedure(Functor, Database) ->
%%	built_in | {code,{Mod,Func}} | {clauses,[Clause]} | undefined.
%% Return the procedure type and data for a functor.

get_procedure(Func, Db) ->
    case ?DB:find(Func, Db) of
	{ok,built_in} -> built_in;		%A built-in
	{ok,{code,{_M,_F}}=P} -> P;		%Compiled (perhaps someday)
	{ok,{clauses,_T,Cs}} -> {clauses,Cs};	%Interpreted clauses
	error -> undefined			%Undefined
    end.

%% get_procedure_type(Functor, Database) ->
%%	built_in | compiled | interpreted | undefined.
%% Return the procedure type for a functor.

get_procedure_type(Func, Db) ->
    case ?DB:find(Func, Db) of
	{ok,built_in} -> built_in;		%A built-in
	{ok,{code,_}} -> compiled;		%Compiled (perhaps someday)
	{ok,{clauses,_,_}} -> interpreted;	%Interpreted clauses
	error -> undefined			%Undefined
    end.

%% get_interp_functors(Database) -> [Functor].

get_interp_functors(Db) ->
    ?DB:fold(fun (_Func, built_in, Fs) -> Fs;
		 (_Func, {code,_}, Fs) -> Fs;
		 (Func, {clauses,_,_}, Fs) -> [Func|Fs]
	     end, [], Db).
-endif.

-ifdef(ETS).
%% The database is an ets table where the key is the functor pair {Name,Arity}.
%% The value is: {Functor,built_in} |
%%		 {Functor,clauses,NextTag,[{Tag,Head,Body}]} |
%%		 {Functor,code,{Module,Function}}.
%% Built-ins are defined by the system and cannot manipulated by user
%% code.
%% We are a little paranoid here and do our best to ensure consistency
%% in the database by checking input arguments even if we know they
%% come from "good" code.

new_db() -> ets:new(erlog_database, [set,protected,{keypos,1}]).

%% add_built_in(Head, Database) -> NewDatabase.
%% Add Head as a built-in in the database.

add_built_in(H, Db) ->
    Functor = functor(H),
    ets:insert(Db, {Functor,built_in}),
    Db.

%% add_compiled_proc(Head, Module, Function, Database) -> NewDatabase.
%% Add Head as a compiled procedure with code in Module:Function. No
%% checking.

add_compiled_proc(H, M, F, Db) ->
    Functor = functor(H),
    case ets:lookup(Db, Functor) of
	[{_,built_in}] ->
	    permission_error(modify, static_procedure, pred_ind(Functor), Db);
	[_] -> ets:insert(Db, {Functor,code,{M,F}});
	[] -> ets:insert(Db, {Functor,code,{M,F}})
    end,
    Db.

%% assertz_clause(Clause, Database) -> NewDatabase.
%% assertz_clause(Head, Body, Database) -> NewDatabase.
%% Assert a clause into the database first checking that it is well
%% formed.

assertz_clause({':-',H,B}, Db) -> assertz_clause(H, B, Db);
assertz_clause(H, Db) -> assertz_clause(H, true, Db).

assertz_clause(Head, Body0, Db) ->
    {Functor,Body} = case catch {ok,functor(Head),
				 well_form_body(Body0, false, sture)} of
			 {erlog_error,E} -> erlog_error(E, Db);
			 {ok,F,B} -> {F,B}
		      end,
    case ets:lookup(Db, Functor) of
	[{_,built_in}] -> permission_error(pred_ind(Functor), Db);
	[{_,code,_}] -> permission_error(pred_ind(Functor), Db);
	[{_,clauses,Tag,Cs}] ->
	    ets:insert(Db, {Functor,clauses,Tag+1,Cs ++ [{Tag,Head,Body}]});
	[] -> ets:insert(Db, {Functor,clauses,1,[{0,Head,Body}]})
    end,
    Db.

%% asserta_clause(Clause, Database) -> NewDatabase.
%% asserta_clause(Head, Body, Database) -> NewDatabase.
%% Assert a clause into the database first checking that it is well
%% formed.

asserta_clause({':-',H,B}, Db) -> asserta_clause(H, B, Db);
asserta_clause(H, Db) -> asserta_clause(H, true, Db).

asserta_clause(Head, Body0, Db) ->
    {Functor,Body} = case catch {ok,functor(Head),
				 well_form_body(Body0, false, sture)} of
			 {erlog_error,E} -> erlog_error(E, Db);
			 {ok,F,B} -> {F,B}
		      end,
    case ets:lookup(Db, Functor) of
	[{_,built_in}] -> permission_error(pred_ind(Functor), Db);
	[{_,code,_}] -> permission_error(pred_ind(Functor), Db);
	[{_,clauses,Tag,Cs}] ->
	    ets:insert(Db, {Functor,clauses,Tag+1,[{Tag,Head,Body}|Cs]});
	[] -> ets:insert(Db, {Functor,clauses,1,[{0,Head,Body}]})
    end,
    Db.

%% retract_clause(Functor, ClauseTag, Database) -> NewDatabase.
%% Retract (remove) the clause with tag ClauseTag from the list of
%% clauses of Functor.

retract_clause(F, Ct, Db) ->
    case ets:lookup(Db, F) of
	[{_,built_in}] ->
	    permission_error(modify, static_procedure, pred_ind(F), Db);
	[{_,code,_}] ->
	    permission_error(modify, static_procedure, pred_ind(F), Db);
	[{_,clauses,Nt,Cs}] ->
	    ets:insert(Db, {F,clauses,Nt,lists:keydelete(Ct, 1, Cs)});
	[] -> ok				%Do nothing
    end,
    Db.

%% abolish_clauses(Functor, Database) -> NewDatabase.

abolish_clauses(Func, Db) ->
    case ets:lookup(Db, Func) of
	[{_,built_in}] ->
	    permission_error(modify, static_procedure, pred_ind(F), Db);
	[{_,code,_}] -> ets:delete(Db, Func);
	[{_,clauses,_,_}] -> ets:delete(Db, Func);
	[] -> ok				%Do nothing
    end,
    Db.

%% get_procedure(Functor, Database) ->
%%	built_in | {code,{Mod,Func}} | {clauses,[Clause]} | undefined.
%% Return the procedure type and data for a functor.

get_procedure(Func, Db) ->
    case ets:lookup(Db, Func) of
	[{_,built_in}] -> built_in;
	[{_,code,C}] -> {code,C};
	[{_,clauses,_,Cs}] -> {clauses,Cs};
	[] -> undefined
    end.

%% get_procedure_type(Functor, Database) ->
%%	built_in | compiled | interpreted | undefined.
%% Return the procedure type for a functor.

get_procedure_type(Func, Db) ->
    case ets:lookup(Db, Func) of
	[{_,built_in}] -> built_in;		%A built-in
	[{_,code,C}] -> compiled;		%Compiled (perhaps someday)
	[{_,clauses,_,Cs}] -> interpreted;	%Interpreted clauses
	[] -> undefined				%Undefined
    end.

%% get_interp_functors(Database) -> [Functor].

get_interp_functors(Db) ->
    ets:foldl(fun ({_,built_in}, Fs) -> Fs;
		  ({_,code,_}, Fs) -> Fs;
		  ({Func,clauses,_,_}, Fs) -> [Func|Fs]
	      end, [], Db).
-endif.

%% functor(Goal) -> {Name,Arity}.

functor(T) when is_tuple(T), size(T) > 1, is_atom(element(1, T)) ->
    {element(1, T),size(T)-1};
functor(T) when is_atom(T) -> {T,0};
functor(T) -> type_error(callable, T).

%% well_form_body(Body, HasCutAfter, CutLabel) -> {Body,HasCut}.
%% well_form_body(Body, Tail, HasCutAfter, CutLabel) -> {Body,HasCut}.
%% Check that Body is well-formed, flatten conjunctions, fix cuts and
%% add explicit call to top-level variables.

well_form_body(Body, Cut, Label) -> well_form_body(Body, [], Cut, Label).

well_form_body({',',L,R}, Tail0, Cut0, Label) ->
    {Tail1,Cut1} = well_form_body(R, Tail0, Cut0, Label),
    well_form_body(L, Tail1, Cut1, Label);
well_form_body({';',{'->',C0,T0},E0}, Tail, Cut0, Label) ->
    {T1,Tc} = well_form_body(T0, Cut0, Label),
    {E1,Ec} = well_form_body(E0, Cut0, Label),
    %% N.B. an extra cut will be added at run-time!
    {C1,_} = well_form_body(C0, true, Label),
    {[{{if_then_else},C1,T1,E1,Label}|Tail],Tc or Ec};
well_form_body({';',L0,R0}, Tail, Cut0, Label) ->
    {L1,Lc} = well_form_body(L0, Cut0, Label),
    {R1,Rc} = well_form_body(R0, Cut0, Label),
    {[{{disj},L1,R1}|Tail],Lc or Rc};
well_form_body({'->',C0,T0}, Tail, Cut0, Label) ->
    {T1,Cut1} = well_form_body(T0, Cut0, Label),
    %% N.B. an extra cut will be added at run-time!
    {C1,_} = well_form_body(C0, true, Label),
    {[{{if_then},C1,T1,Label}|Tail],Cut1};
well_form_body({once,G}, Tail, Cut, Label) ->
    %% N.B. an extra cut is added at run-time!
    {G1,_} = well_form_body(G, true, Label),
    {[{{once},G1,Label}|Tail],Cut};
well_form_body({V}, Tail, Cut, _Label) ->
    {[{call,{V}}|Tail],Cut};
well_form_body(true, Tail, Cut, _Label) -> {Tail,Cut}; %No-op
well_form_body(fail, _Tail, _Cut, _Label) -> {[fail],false};	%No further
well_form_body('!', Tail, Cut, Label) ->
    {[{{cut},Label,not Cut}|Tail],true};
well_form_body(Goal, Tail, Cut, _Label) ->
    functor(Goal),				%Check goal
    {[Goal|Tail],Cut}.

%% well_form_goal(Goal, Tail, HasCutAfter, CutLabel) -> {Body,HasCut}.
%% Check that Goal is well-formed, flatten conjunctions, fix cuts and
%% add explicit call to top-level variables. 

well_form_goal({',',L,R}, Tail0, Cut0, Label) ->
    {Tail1,Cut1} = well_form_goal(R, Tail0, Cut0, Label),
    well_form_goal(L, Tail1, Cut1, Label);
well_form_goal({';',{'->',C0,T0},E0}, Tail, Cut0, Label) ->
    {T1,Tc} = well_form_goal(T0, Tail, Cut0, Label),
    {C1,_} = well_form_goal(C0, [{{cut},Label,true}|T1], true, Label),
    {E1,Ec} = well_form_goal(E0, Tail, Cut0, Label),
    {[{{if_then_else},E1,Label}|C1],Tc or Ec};
well_form_goal({';',L0,R0}, Tail, Cut0, Label) ->
    {L1,Lc} = well_form_goal(L0, Tail, Cut0, Label),
    {R1,Rc} = well_form_goal(R0, Tail, Cut0, Label),
    {[{{disj},R1}|L1],Lc or Rc};
well_form_goal({'->',C0,T0}, Tail, Cut0, Label) ->
    {T1,Cut1} = well_form_goal(T0, Tail, Cut0, Label),
    %% N.B. an extra cut will be added at run-time!
    {C1,_} = well_form_goal(C0, [{{cut},Label,true}|T1], true, Label),
    {[{{if_then},Label}|C1],Cut1};
well_form_goal({once,G}, Tail, Cut, Label) ->
    {G1,_} = well_form_goal(G, [{{cut},Label,true}|Tail], true, Label),
    {[{{once},Label}|G1],Cut};
well_form_goal({V}, Tail, Cut, _Label) ->
    {[{call,{V}}|Tail],Cut};
well_form_goal(true, Tail, Cut, _Label) -> {Tail,Cut}; %No-op
well_form_goal(fail, _Tail, _Cut, _Label) -> {[fail],false};	%No further
well_form_goal('!', Tail, Cut, Label) ->
    {[{{cut},Label,not Cut}|Tail],true};
well_form_goal(Goal, Tail, Cut, _Label) ->
    functor(Goal),				%Check goal
    {[Goal|Tail],Cut}.

%% term_instance(Term, VarNum) -> {Term,NewRepls,NewVarNum}.
%% term_instance(Term, Repls, VarNum) -> {Term,NewRepls,NewVarNum}.
%%  Generate a copy of a term with new, fresh unused variables. No
%%  bindings from original variables to new variables. It can handle
%%  replacing integer variables with overlapping integer ranges. Don't
%%  check Term as it should already be checked. Use orddict as there
%%  will seldom be many variables and it it fast to setup.

term_instance(A, Vn) -> term_instance(A, orddict:new(), Vn).

term_instance([], Rs, Vn) -> {[],Rs,Vn};
term_instance([H0|T0], Rs0, Vn0) ->
    {H,Rs1,Vn1} = term_instance(H0, Rs0, Vn0),
    {T,Rs2,Vn2} = term_instance(T0, Rs1, Vn1),
    {[H|T],Rs2,Vn2};
term_instance({'_'}, Rs, Vn) -> {{Vn},Rs,Vn+1};	%Unique variable
term_instance({V0}, Rs0, Vn0) ->		%Other variables
    case orddict:find(V0, Rs0) of
	{ok,V1} -> {V1,Rs0,Vn0};
	error ->
	    V1 = {Vn0},
	    {V1,orddict:store(V0, V1, Rs0),Vn0+1}
    end;
%% Special case some smaller structures.
term_instance({Atom,Arg}, Rs0, Vn0) ->
    {CopyArg,Rs1,Vn1} = term_instance(Arg, Rs0, Vn0),
    {{Atom,CopyArg},Rs1,Vn1};
term_instance({Atom,A1,A2}, Rs0, Vn0) ->
    {CopyA1,Rs1,Vn1} = term_instance(A1, Rs0, Vn0),
    {CopyA2,Rs2,Vn2} = term_instance(A2, Rs1, Vn1),
    {{Atom,CopyA1,CopyA2},Rs2,Vn2};
term_instance(T, Rs0, Vn0) when is_tuple(T) ->
    As0 = tl(tuple_to_list(T)),
    {As1,Rs1,Vn1} = term_instance(As0, Rs0, Vn0),
    {list_to_tuple([element(1, T)|As1]),Rs1,Vn1};
term_instance(A, Rs, Vn) -> {A,Rs,Vn}.		%Constant

%% unify_head(Goal, Head, Bindings, VarNum) ->
%%      {succeed,Repls,NewBindings,NewVarNum} | fail
%% Unify a goal with a head without creating an instance of the
%% head. This saves us creating many variables which are local to the
%% clause and saves many variable bindings.

unify_head(Goal, Head, Bs, Vn) ->
    unify_head(deref(Goal, Bs), Head, orddict:new(), Bs, Vn).

unify_head(G, H, Rs, Bs, Vn) when ?IS_CONSTANT(G), G == H ->
    {succeed,Rs,Bs,Vn};
unify_head(_T, {'_'}, Rs, Bs, Vn) -> {succeed,Rs,Bs,Vn};
unify_head(T, {V0}, Rs, Bs0, Vn) ->
    %% Now for the tricky bit!
    case orddict:find(V0, Rs) of
	{ok,V1} ->				%Already have a replacement
	    case unify(T, V1, Bs0) of
		{succeed,Bs1} -> {succeed,Rs,Bs1,Vn};
		fail -> fail
	    end;
	error ->				%Add a replacement
	    {succeed,orddict:store(V0, T, Rs),Bs0,Vn}
    end;
unify_head({_}=Var, H0, Rs0, Bs, Vn0) ->
    %% Must have an instance here.
    {H1,Rs1,Vn1} = term_instance(H0, Rs0, Vn0),
    {succeed,Rs1,add_binding(Var, H1, Bs),Vn1};
unify_head([GH|GT], [HH|HT], Rs0, Bs0, Vn0) ->
    case unify_head(deref(GH, Bs0), HH, Rs0, Bs0, Vn0) of
	{succeed,Rs1,Bs1,Vn1} -> unify_head(deref(GT, Bs1), HT, Rs1, Bs1, Vn1);
	fail -> fail
    end;
unify_head([], [], Rs, Bs, Vn) -> {succeed,Rs,Bs,Vn};
unify_head(G, H, Rs, Bs, Vn) when is_tuple(G), is_tuple(H), size(G) == size(H),
				  element(1, G) == element(1, H) ->
    unify_head_args(G, H, Rs, Bs, Vn, 2, size(G));
unify_head(_G, _H, _Rs, _Bs, _Vn) -> fail.

unify_head_args(_G, _H, Rs, Bs, Vn, I, S) when I > S ->
    {succeed,Rs,Bs,Vn};
unify_head_args(G, H, Rs0, Bs0, Vn0, I, S) ->
    case unify_head(deref(element(I, G), Bs0), element(I, H), Rs0, Bs0, Vn0) of
	{succeed,Rs1,Bs1,Vn1} -> unify_head_args(G, H, Rs1, Bs1, Vn1, I+1, S);
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

body_instance([{{cut}=Cut,_,Last}|Gs0], Tail, Rs0, Vn0, Label) ->
    {Gs1,Rs1,Vn1} = body_instance(Gs0, Tail, Rs0, Vn0, Label),
    {[{Cut,Label,Last}|Gs1],Rs1,Vn1};
body_instance([{{disj}=Disj,L0,R0}|Gs0], Tail, Rs0, Vn0, Label) ->
    {Gs1,Rs1,Vn1} = body_instance(Gs0, Tail, Rs0, Vn0, Label),
    %% Append Gs1 directly to L and R.
    {L1,Rs2,Vn2} = body_instance(L0, Gs1, Rs1, Vn1, Label),
    {R1,Rs3,Vn3} = body_instance(R0, Gs1, Rs2, Vn2, Label),
    {[{Disj,R1}|L1],Rs3,Vn3};
body_instance([{{if_then}=IT,C0,T0,_}|Gs0], Tail, Rs0, Vn0, Label) ->
    {Gs1,Rs1,Vn1} = body_instance(Gs0, Tail, Rs0, Vn0, Label),
    {T1,Rs2,Vn2} = body_instance(T0, Gs1, Rs1, Vn1, Label),
    {C1,Rs3,Vn3} = body_instance(C0, [{{cut},Label,true}|T1], Rs2, Vn2, Label),
    %% Append Gs1 directly to T1 to C1.
    {[{IT,Label}|C1],Rs3,Vn3};
body_instance([{{if_then_else}=ITE,C0,T0,E0,_}|Gs0], Tail, Rs0, Vn0, Label) ->
    {Gs1,Rs1,Vn1} = body_instance(Gs0, Tail, Rs0, Vn0, Label),
    {T1,Rs2,Vn2} = body_instance(T0, Gs1, Rs1, Vn1, Label),
    {C1,Rs3,Vn3} = body_instance(C0, [{{cut},Label,true}|T1], Rs2, Vn2, Label),
    {E1,Rs4,Vn4} = body_instance(E0, Gs1, Rs3, Vn3, Label),
    {[{ITE,E1,Label}|C1],Rs4,Vn4};
body_instance([{{once}=Once,G0,_}|Gs0], Tail, Rs0, Vn0, Label) ->
    {Gs1,Rs1,Vn1} = body_instance(Gs0, Tail, Rs0, Vn0, Label),
    {G1,Rs2,Vn2} = body_instance(G0, [{{cut},Label,true}|Gs1], Rs1, Vn1, Label),
    {[{Once,Label}|G1],Rs2,Vn2};
body_instance([G0|Gs0], Tail, Rs0, Vn0, Label) ->
    {Gs1,Rs1,Vn1} = body_instance(Gs0, Tail, Rs0, Vn0, Label),
    {G1,Rs2,Vn2} = term_instance(G0, Rs1, Vn1),
    {[G1|Gs1],Rs2,Vn2};
body_instance([], Tail, Rs, Vn, _Label) -> {Tail,Rs,Vn}.

%% body_term(Body, Repls, VarNum) -> {Term,NewRepls,NewVarNum}.
%%  Generate a copy of a body as a term with new, fresh unused
%%  variables. No bindings from original variables to new
%%  variables. It can handle replacing integer variables with
%%  overlapping integer ranges. Don't check Term as it should already
%%  be checked. Use orddict as there will seldom be many variables and
%%  it it fast to setup.

body_term([{{cut},_,_}|Gs0], Rs0, Vn0) ->
    {Gs1,Rs1,Vn1} = body_term(Gs0, Rs0, Vn0),
    {body_conj('!', Gs1),Rs1,Vn1};
body_term([{{disj},L0,R0}|Gs0], Rs0, Vn0) ->
    {Gs1,Rs1,Vn1} = body_term(Gs0, Rs0, Vn0),
    {L1,Rs2,Vn2} = body_term(L0, Rs1, Vn1),
    {R1,Rs3,Vn3} = body_term(R0, Rs2, Vn2),
    {body_conj({';',L1,R1}, Gs1),Rs3,Vn3};
body_term([{{if_then},C0,T0,_}|Gs0], Rs0, Vn0) ->
    {Gs1,Rs1,Vn1} = body_term(Gs0, Rs0, Vn0),
    {C1,Rs2,Vn2} = body_term(C0, Rs1, Vn1),
    {T1,Rs3,Vn3} = body_term(T0, Rs2, Vn2),
    {body_conj({'->',C1,T1}, Gs1),Rs3,Vn3};
body_term([{{if_then_else},C0,T0,E0,_}|Gs0], Rs0, Vn0) ->
    {Gs1,Rs1,Vn1} = body_term(Gs0, Rs0, Vn0),
    {C1,Rs2,Vn2} = body_term(C0, Rs1, Vn1),
    {T1,Rs3,Vn3} = body_term(T0, Rs2, Vn2),
    {E1,Rs4,Vn4} = body_term(E0, Rs3, Vn3),
    {body_conj({';',{'->',C1,T1},E1}, Gs1),Rs4,Vn4};
body_term([{{once},G0,_}|Gs0], Rs0, Vn0) ->
    {Gs1,Rs1,Vn1} = body_term(Gs0, Rs0, Vn0),
    {G1,Rs2,Vn2} = body_term(G0, Rs1, Vn1),
    {body_conj({once,G1}, Gs1),Rs2,Vn2};
body_term([G0|Gs0], Rs0, Vn0) ->
    {Gs1,Rs1,Vn1} = body_term(Gs0, Rs0, Vn0),
    {G1,Rs2,Vn2} = term_instance(G0, Rs1, Vn1),
    {body_conj(G1, Gs1),Rs2,Vn2};
body_term([], Rs, Vn) -> {true,Rs,Vn}.

body_conj(L, true) -> L;
body_conj(L, R) -> {',',L,R}.

pred_ind({N,A}) -> {'/',N,A}.

pred_ind(N, A) -> {'/',N,A}.

%% Bindings
%% Bindings are kept in a dict where the key is the variable name.
%%-define(BIND, orddict).
-define(BIND, dict).

new_bindings() -> ?BIND:new().

add_binding({V}, Val, Bs0) ->
    ?BIND:store(V, Val, Bs0).

get_binding({V}, Bs) ->
    ?BIND:find(V, Bs).

%% deref(Term, Bindings) -> Term.
%% Dereference a variable, else just return the term.

deref({V}=T0, Bs) ->
    case ?BIND:find(V, Bs) of
	{ok,T1} -> deref(T1, Bs);
	error -> T0
    end;
deref(T, _) -> T.				%Not a variable, return it.

%% dderef(Term, Bindings) -> Term.
%% Do a deep dereference. Completely dereference all the variables
%% occuring in a term, even those occuring in a variables value.

dderef(A, _) when ?IS_CONSTANT(A) -> A;
dderef([], _) -> [];
dderef([H0|T0], Bs) ->
    [dderef(H0, Bs)|dderef(T0, Bs)];
dderef({V}=Var, Bs) ->
    case ?BIND:find(V, Bs) of
	{ok,T} -> dderef(T, Bs);
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
dderef_list([H|T], Bs) ->
    [dderef(H, Bs)|dderef_list(T, Bs)];
dderef_list({V}, Bs) ->
    case ?BIND:find(V, Bs) of
	{ok,L} -> dderef_list(L, Bs);
	error -> instantiation_error()
    end;
dderef_list(Other, _Bs) -> type_error(list, Other).

%% initial_goal(Goal) -> {Goal,Bindings,NewVarNum}.
%% initial_goal(Goal, Bindings, VarNum) -> {Goal,NewBindings,NewVarNum}.
%% Check term for well-formedness as an Erlog term and replace '_'
%% variables with unique numbered variables. Error on non-well-formed
%% goals.

initial_goal(Goal) -> initial_goal(Goal, new_bindings(), 0).

initial_goal({'_'}, Bs, Vn) -> {{Vn},Bs,Vn+1};	%Anonymous variable
initial_goal({Name}=Var0, Bs, Vn) when is_atom(Name) ->
    case get_binding(Var0, Bs) of
	{ok,Var1} -> {Var1,Bs,Vn};
	error ->
	    Var1 = {Vn},
	    {Var1,add_binding(Var0, Var1, Bs),Vn+1}
    end;
initial_goal([H0|T0], Bs0, Vn0) ->
    {H1,Bs1,Vn1} = initial_goal(H0, Bs0, Vn0),
    {T1,Bs2,Vn2} = initial_goal(T0, Bs1, Vn1),
    {[H1|T1],Bs2,Vn2};
initial_goal([], Bs, Vn) -> {[],Bs,Vn};
initial_goal(S, Bs0, Vn0)
  when is_tuple(S), size(S) >= 2, is_atom(element(1, S)) ->
    As0 = tl(tuple_to_list(S)),
    {As1,Bs1,Vn1} = initial_goal(As0, Bs0, Vn0),
    {list_to_tuple([element(1, S)|As1]),Bs1,Vn1};
initial_goal(T, Bs, Vn) when ?IS_CONSTANT(T) -> {T,Bs,Vn};
initial_goal(T, _Bs, _Vn) -> type_error(callable, T).

%% expand_term(Term) -> {ExpTerm}.
%% expand_term(Term, VarNum) -> {ExpTerm,NewVarNum}.
%% expand_head(Head, Var1, Var2) -> ExpHead.
%% expand_body(Body, Var1, VarNum) -> {ExpBody,Var2,NewVarNum}.
%% Handle DCG expansion. We do NOT work backwards.

expand_term(Term) ->
    {Exp,_} = expand_term(Term, 0),
    Exp.

expand_term({'-->',H0,B0}, Vn0) ->
    V1 = {Vn0},					%Top-level chaining variable
    {B1,V2,Vn1} = expand_body(B0, V1, Vn0+1),
    H1 = expand_head(H0, V1, V2),
    {{':-',H1,B1},Vn1+1};
expand_term(T, Vn) -> {T,Vn}.			%Do nothing

expand_head(A, V1, V2) when is_atom(A) -> {A,V1,V2};
expand_head(T, V1, V2) when is_tuple(T), size(T) >= 2, is_atom(element(1, T)) ->
    list_to_tuple(tuple_to_list(T) ++ [V1,V2]);
expand_head(Other, _V1, _V2) -> type_error(callable, Other).

expand_body('!', V1, Vn) -> {'!',V1,Vn};	%No chaining
expand_body({_}=V, V1, Vn) -> V2 = {Vn}, {{phrase,V,V1,V2},V2,Vn+1};
expand_body({'{}',T}, V1, Vn) -> {T,V1,Vn};	%No chaining
expand_body({'\\+',G0}, V1, Vn0) ->		%No chaining
    {G1,_V2,Vn1} = expand_body(G0, V1, Vn0),
    {{'\\+',G1},V1,Vn1};
expand_body({',',L0,R0}, V1, Vn0) ->
    {L1,V2,Vn1} = expand_body(L0, V1, Vn0),
    {R1,V3,Vn2} = expand_body(R0, V2, Vn1),
    {{',',L1,R1},V3,Vn2};
expand_body({'->',L0,R0}, V1, Vn0) ->
    {L1,V2,Vn1} = expand_body(L0, V1, Vn0),
    {R1,V3,Vn2} = expand_body(R0, V2, Vn1),
    {{'->',L1,R1},V3,Vn2};
expand_body({';',L0,R0}, V1, Vn0) ->
    {L1,V2,Vn1} = expand_body(L0, V1, Vn0),
    {R1,V3,Vn2} = expand_body(R0, V1, Vn1),
    {{';',L1,{',',R1,{'=',V3,V2}}},V2,Vn2};
expand_body([], V1, Vn) ->			%Chain but do nothing
    V2 = {Vn},
    {{'=',V1,V2},V2,Vn+1};
expand_body(Lits, V1, Vn) when is_list(Lits) ->
    expand_lits(Lits, V1, Vn);
expand_body(Goal, V1, Vn) ->
    V2 = {Vn},
    {expand_head(Goal, V1, V2),V2,Vn+1}.

expand_lits([Lit], V1, Vn) ->
    V2 = {Vn},
    {{'C',V1,Lit,V2},V2,Vn+1};
expand_lits([Lit|Lits0], V1, Vn0) ->
    V2 = {Vn0},
    {Lits1,V3,Vn1} = expand_lits(Lits0, V2, Vn0+1),
    {{',',{'C',V1,Lit,V2},Lits1},V3,Vn1}.
