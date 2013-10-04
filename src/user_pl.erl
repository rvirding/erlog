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

%% File    : user_pl.erl
%% Author  : Robert Virding
%% Purpose : Test compilation of Erlog procedures.
%% 
%% Hand compiled app/3 and rev/2 to test speed-up. (~2)
%% Hand compiled mem/2 to show the small benefit when only
%% implementing indexing on first argument.

-module(user_pl).

-include("erlog_int.hrl").

-export([assert/1,app_3/6,rev_2/6,mem_2/6]).

-import(erlog_int, [add_binding/3,
                    %% unify/3,
                    %% deref/2,
                    dderef/2,
		    %% prove_body/5,
                    unify_prove_body/7,
                    fail/2,
		    add_compiled_proc/4]).
-import(lists, [foldl/3]).

%% assert(Database) -> Database.
%%  Assert predicates into the database.

assert(Db) ->
    foldl(fun ({Head,M,F}, LDb) -> 
		  add_compiled_proc(Head, M, F, LDb) end, Db,
	  [
	   {{app,3},?MODULE,app_3},
	   {{rev,3},?MODULE,rev_2},
	   {{mem,2},?MODULE,mem_2}
	  ]).

%% app_3(Head, NextGoal, Choicepoints, Bindings, VarNum, Database) -> void.
%% app([], L, L).
%% app([H|T], L, [H|L1]) :- app(T, L, L1).

app_3(Head, Next0, Cps, Bs0, Vn, Db) ->
    case dderef(Head, Bs0) of
	{app,[],A2,A3} ->
	    unify_prove_body(A2, A3, Next0, Cps, Bs0, Vn, Db);
	{app,[H|T],A2,A3} ->
	    L = A2,
	    L1 = {Vn},
	    Next1 = [{app,T,L,L1}|Next0],
	    unify_prove_body(A3, [H|L1], Next1, Cps, Bs0, Vn+1, Db);
	{app,{_}=A1,A2,A3} ->
	    FailFun = fun (LCp, LCps, LDb) ->
			      fail_app_3(LCp, LCps, LDb, A1, A2, A3)
		      end,
	    Cp = #cp{type=compiled,data=FailFun,next=Next0,bs=Bs0,vn=Vn},
	    Bs1 = add_binding(A1, [], Bs0),
	    unify_prove_body(A2, A3, Next0, [Cp|Cps], Bs1, Vn, Db);
	{app,_A1,_A2,_A3} -> fail(Cps, Db)	%Will fail here!
    end.

fail_app_3(#cp{next=Next,bs=Bs0,vn=Vn}, Cps, Db, A1, A2, A3) ->
    H = {Vn},
    T = {Vn+1},
    L = A2,
    L1 = {Vn+2},
    Bs1 = add_binding(A1, [H|T], Bs0),
    Next1 = [{app,T,L,L1}|Next],
    unify_prove_body(A3, [H|L1], Next1, Cps, Bs1, Vn+3, Db).
	
%% rev_2(Head, NextGoal, Choicepoints, Bindings, VarNum, Database) -> void.
%% rev([], []).
%% rev([H|T], L1) :- rev(T, L), app(L, [H], L1).

rev_2(Head, Next0, Cps, Bs0, Vn, Db) ->
    case dderef(Head, Bs0) of
%% 	{rev,[],[]} -> prove_body(Next0, Cps, Bs0, Vn, Db);
%% 	{rev,[],{_}=Var} ->
%% 	    Bs1 = add_binding(Var, [], Bs0),
%% 	    prove_body(Next0, Cps, Bs1, Vn, Db);
%% 	{rev,[],_} -> fail(Cps, Db);
	{rev,[],A2} ->
	    unify_prove_body(A2, [], Next0, Cps, Bs0, Vn, Db);
	{rev,[H|T],A2} ->
	    L = {Vn},
	    L1 = A2,
	    %%Next1 = [{rev,T,L},{app,L,[H],L1}|Next0],
	    %%prove_body(Next1, Cps, Bs0, Vn+1, Db);
	    Next1 = [{app,L,[H],L1}|Next0],
	    rev_2({rev,T,L}, Next1, Cps, Bs0, Vn+1, Db);
	{rev,{_}=A1,A2} ->
	    FailFun = fun (LCp, LCps, LDb) ->
			      fail_rev_2(LCp, LCps, LDb, A1, A2)
		      end,
	    Cp = #cp{type=compiled,data=FailFun,next=Next0,bs=Bs0,vn=Vn},
	    Bs1 = add_binding(A1, [], Bs0),
	    unify_prove_body(A2, [], Next0, [Cp|Cps], Bs1, Vn, Db);
	{rev,_A1,_A2} -> fail(Cps, Db)
    end.

fail_rev_2(#cp{next=Next,bs=Bs0,vn=Vn}, Cps, Db, A1, A2) ->
    H = {Vn},
    T = {Vn+1},
    L1 = A2,
    L = {Vn+2},
    Bs1 = add_binding(A1, [H|T], Bs0),
    %%Next1 = [{rev,T,L},{app,L,[H],L1}|Next],
    %%prove_body(Next1, Cps, Bs1, Vn+3, Db).
    Next1 = [{app,L,[H],L1}|Next],
    rev_2({rev,T,L}, Next1, Cps, Bs1, Vn+3, Db).

%% mem_2(Head, NextGoal, Choicepoints, Bindings, VarNum, Database) -> void.
%% mem(X, [X|_]).
%% mem(X, [_|T]) :- mem(X, T).

mem_2(Head, Next, Cps, Bs, Vn, Db) ->
    {mem,A1,A2} = dderef(Head, Bs),
    FailFun = fun (LCp, LCps, LDb) ->
		      fail_mem_2(LCp, LCps, LDb, A1, A2)
	      end,
    Cp = #cp{type=compiled,data=FailFun,next=Next,bs=Bs,vn=Vn},
    T = {Vn},
    unify_prove_body(A2, [A1|T], Next, [Cp|Cps], Bs, Vn+1, Db).

fail_mem_2(#cp{next=Next,bs=Bs,vn=Vn}, Cps, Db, A1, A2) ->
    H = {Vn},
    T = {Vn+1},
    unify_prove_body(A2, [H|T], [{mem,A1,T}|Next], Cps, Bs, Vn+2, Db).
