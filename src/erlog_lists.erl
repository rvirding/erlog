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
-export([append_3/6,insert_3/6,member_2/6,reverse_2/6]).

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
    %% Compiled common list library.
    Db1 = foldl(fun ({Head,M,F}, Db) ->
			add_compiled_proc(Head, M, F, Db) end, Db0,
		[
		 {{reverse,2},?MODULE,reverse_2},
		 {{member,2},?MODULE,member_2},
		 {{insert,3},?MODULE,insert_3},
		 {{append,3},?MODULE,append_3}
		]),
    %% Finally interpreted common list library.
    foldl(fun (Clause, Db) -> assertz_clause(Clause, Db) end, Db1,
	  [
	   %% insert(L, X, [X|L]). insert([H|L], X, [H|L1]) :- insert(L, X, L1).
	   %% delete([X|L], X, L). delete([H|L], X, [H|L1]) :- delete(L, X, L1).
	   {':-',{delete,{1},{2},{3}},{insert,{3},{2},{1}}},
	   %% perm([], []).
	   %% perm([X|Xs], Ys1) :- perm(Xs, Ys), insert(Ys, X, Ys1).
	   {perm,[],[]},
	   {':-',{perm,[{1}|{2}],{3}},{',',{perm,{2},{4}},{insert,{4},{1},{3}}}}
	  ]).

%% append_3(Head, NextGoal, Choicepoints, Bindings, VarNum, Database) -> void.
%% append([], L, L).
%% append([H|T], L, [H|L1]) :- append(T, L, L1).

append_3(Head, Next0, Cps, Bs0, Vn, Db) ->
    case dderef(Head, Bs0) of
	{append,[],A2,A3} ->
	    unify_prove_body(A2, A3, Next0, Cps, Bs0, Vn, Db);
	{append,[H|T],L,A3} ->
	    L1 = {Vn},
	    Next1 = [{append,T,L,L1}|Next0],
	    unify_prove_body(A3, [H|L1], Next1, Cps, Bs0, Vn+1, Db);
	{append,{_}=A1,A2,A3} ->
	    FailFun = fun (LCp, LCps, LDb) ->
			      fail_append_3(LCp, LCps, LDb, A1, A2, A3)
		      end,
	    Cp = #cp{type=compiled,data=FailFun,next=Next0,bs=Bs0,vn=Vn},
	    Bs1 = add_binding(A1, [], Bs0),
	    unify_prove_body(A2, A3, Next0, [Cp|Cps], Bs1, Vn, Db);
	{append,_A1,_A2,_A3} -> fail(Cps, Db)	%Will fail here!
    end.

fail_append_3(#cp{next=Next0,bs=Bs0,vn=Vn}, Cps, Db, A1, L, A3) ->
    H = {Vn},
    T = {Vn+1},
    L1 = {Vn+2},
    Bs1 = add_binding(A1, [H|T], Bs0),		%A1 always a variable here.
    Next1 = [{append,T,L,L1}|Next0],
    unify_prove_body(A3, [H|L1], Next1, Cps, Bs1, Vn+3, Db).

%% insert_3(Head, NextGoal, Choicepoints, Bindings, VarNum, Database) -> void.
%% insert(L, X, [X|L]).
%% insert([H|L], X, [H|L1]) :- insert(L, X, L1).

insert_3(Head, Next, Cps, Bs, Vn, Db) ->
    {insert,A1,A2,A3} = dderef(Head, Bs),
    FailFun = fun (LCp, LCps, LDb) ->
		      fail_insert_3(LCp, LCps, LDb, A1, A2, A3)
	      end,
    Cp = #cp{type=compiled,data=FailFun,next=Next,bs=Bs,vn=Vn},
    unify_prove_body(A3, [A2|A1], Next, [Cp|Cps], Bs, Vn, Db).

fail_insert_3(#cp{next=Next0,bs=Bs,vn=Vn}, Cps, Db, A1, X, A3) ->
    H = {Vn},
    L = {Vn+1},
    L1 = {Vn+2},
    Next1 = [{insert,L,X,L1}|Next0],
    unify_prove_body(A1, [H|L], A3, [H|L1], Next1, Cps, Bs, Vn+3, Db).

%% member_2(Head, NextGoal, Choicepoints, Bindings, VarNum, Database) -> void.
%% member(X, [X|_]).
%% member(X, [_|T]) :- member(X, T).

member_2(Head, Next, Cps, Bs, Vn, Db) ->
    {member,A1,A2} = dderef(Head, Bs),
    FailFun = fun (LCp, LCps, LDb) ->
		      fail_member_2(LCp, LCps, LDb, A1, A2)
	      end,
    Cp = #cp{type=compiled,data=FailFun,next=Next,bs=Bs,vn=Vn},
    T = {Vn},
    unify_prove_body(A2, [A1|T], Next, [Cp|Cps], Bs, Vn+1, Db).

fail_member_2(#cp{next=Next0,bs=Bs,vn=Vn}, Cps, Db, A1, A2) ->
    H = {Vn},
    T = {Vn+1},
    Next1 = [{member,A1,T}|Next0],
    unify_prove_body(A2, [H|T], Next1, Cps, Bs, Vn+2, Db).

%% reverse_2(Head, NextGoal, Choicepoints, Bindings, VarNum, Database) -> void.
%% reverse([], []).
%% reverse([H|L1], L) :- reverse(L1, L2), append(L2, [H], L).

reverse_2(Head, Next0, Cps, Bs0, Vn, Db) ->
    case dderef(Head, Bs0) of
%% 	{reverse,[],[]} -> prove_body(Next0, Cps, Bs0, Vn, Db);
%% 	{reverse,[],{_}=Var} ->
%% 	    Bs1 = add_binding(Var, [], Bs0),
%% 	    prove_body(Next0, Cps, Bs1, Vn, Db);
%% 	{reverse,[],_} -> fail(Cps, Db);
	{reverse,[],A2} ->
	    unify_prove_body(A2, [], Next0, Cps, Bs0, Vn, Db);
	{reverse,[H|T],A2} ->
	    L = {Vn},
	    L1 = A2,
	    %%Next1 = [{reverse,T,L},{append,L,[H],L1}|Next0],
	    %%prove_body(Next1, Cps, Bs0, Vn+1, Db);
	    Next1 = [{append,L,[H],L1}|Next0],
	    reverse_2({reverse,T,L}, Next1, Cps, Bs0, Vn+1, Db);
	{reverse,{_}=A1,A2} ->
	    FailFun = fun (LCp, LCps, LDb) ->
			      fail_reverse_2(LCp, LCps, LDb, A1, A2)
		      end,
	    Cp = #cp{type=compiled,data=FailFun,next=Next0,bs=Bs0,vn=Vn},
	    Bs1 = add_binding(A1, [], Bs0),
	    unify_prove_body(A2, [], Next0, [Cp|Cps], Bs1, Vn, Db);
	{reverse,_A1,_A2} -> fail(Cps, Db)
    end.

fail_reverse_2(#cp{next=Next,bs=Bs0,vn=Vn}, Cps, Db, A1, A2) ->
    H = {Vn},
    T = {Vn+1},
    L1 = A2,
    L = {Vn+2},
    Bs1 = add_binding(A1, [H|T], Bs0),
    %%Next1 = [{reverse,T,L},{apperse,L,[H],L1}|Next],
    %%prove_body(Next1, Cps, Bs1, Vn+3, Db).
    Next1 = [{append,L,[H],L1}|Next],
    reverse_2({reverse,T,L}, Next1, Cps, Bs1, Vn+3, Db).
