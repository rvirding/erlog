%% Copyright (c) 2008-2014 Robert Virding
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

%% File    : erlog_ets.erl
%% Author  : Robert Virding
%% Purpose : ETS interface for Erlog.

-module(erlog_ets).

-include("erlog_int.hrl").

-compile(export_all).

-export([load/1,all_1/3,keys_2/3,match_2/3]).

-import(lists, [foldl/3]).
-import(erlog_int, [add_compiled_proc/4,dderef/2,unify/3,
		    prove_body/2,unify_prove_body/4,fail/1]).

%% load(Database) -> Database.
%%  Assert predicates into the database.

load(Db0) ->
    Db1 = foldl(fun ({Head,M,F}, LDb) -> 
			add_compiled_proc(Head, M, F, LDb) end, Db0,
		[
		 {{ets_all,1},?MODULE,all_1},
		 {{ets_keys,2},?MODULE,keys_2},
		 {{ets_match,2},?MODULE,match_2}
		]),
    Db1.

%% all_1(Head, NextGoal, State) -> void().
%%      Goal = {ets_all,Tables}.
%% Return all the ETS databases.

all_1({ets_all,Var}, Next, St) ->
    Tabs = ets:all(),
    unify_prove_body(Var, Tabs, Next, St).

%% keys_2(Head, NextGoal, State) -> void().
%%      Goal = {ets_keys,Table,Key}.
%% Return the keys in an ETS database one at a time over backtracking.

keys_2({ets_keys,Tab0,KeyVar}, Next, #est{bs=Bs}=St) ->
    Tab1 = dderef(Tab0, Bs),
    case ets:first(Tab1) of
	'$end_of_table' -> fail(St);
	Key -> keys_loop(Tab1, Key, KeyVar, Next, St)
    end.

keys_loop(Tab, Key, KeyVar, Next, #est{cps=Cps,bs=Bs,vn=Vn}=St) ->
    FailFun = fun(LCp, LCps, Lst) ->
		      keys_fail(LCp, LCps, Lst, Tab, Key, KeyVar)
	      end,
    Cp = #cp{type=compiled,data=FailFun,next=Next,bs=Bs,vn=Vn},
    unify_prove_body(KeyVar, Key, Next, St#est{cps=[Cp|Cps]}).

keys_fail(#cp{next=Next,bs=Bs,vn=Vn}, Cps, St, Tab, PrevKey, KeyVar) ->
    case ets:next(Tab, PrevKey) of
	'$end_of_table' -> fail(St);
	Key -> keys_loop(Tab, Key, KeyVar, Next, St#est{cps=Cps,bs=Bs,vn=Vn})
    end.

%% match_2(Head, Next, State) -> void().
%%      Head = {ets_match,Table,Pattern}.
%% Match objects in an ETS database one at a time over backtracking
%% using Pattern in goal. Variables in Pattern are bound for each
%% object matched.

match_2({ets_match,Tab0,Pat0}, Next, #est{bs=Bs}=St) ->
    Tab1 = dderef(Tab0, Bs),
    Pat1 = dderef(Pat0, Bs),
    {Epat,Vs} = ets_pat(Pat1),
    match_2_loop(ets:match(Tab1, Epat, 10), Next, St, Epat, Vs).

match_2_loop({[M|Ms],Cont}, Next, #est{cps=Cps,bs=Bs,vn=Vn}=St, Epat, Vs) ->
    FailFun = fun (LCp, LCps, Lst) ->
		      match_2_fail(LCp, LCps, Lst, Epat, Vs, {Ms,Cont})
	      end,
    Cp = #cp{type=compiled,data=FailFun,next=Next,bs=Bs,vn=Vn},
    unify_prove_body(Vs, M, Next, St#est{cps=[Cp|Cps]});
match_2_loop({[],Cont}, Next, St, Epat, Vs) ->
    match_2_loop(ets:match(Cont), Next, St, Epat, Vs);
match_2_loop('$end_of_table', _Next, St, _Epat, _Vs) ->
    fail(St).

match_2_fail(#cp{next=Next,bs=Bs,vn=Vn}, Cps, St, Epat, Vs, Ms) ->
    match_2_loop(Ms, Next, St#est{cps=Cps,bs=Bs,vn=Vn}, Epat, Vs).

%% ets_pat(Term) -> {EtsPattern,VarList}.
%% Convert a term into an ETS pattern replacing variables with the ETS
%% pattern variables. Also return a list of variables in the same
%% order as ETS will return the list of values. This is slightly
%% tricky as the order they are in ETS which is not the same as term
%% order so they can not be easily sorted. Sigh!

ets_pat(Pat) ->
    {Epat,_Vn,Vs0} = ets_pat(Pat, 11, []),
    Vs1 = [ V || {V,_Ev} <- Vs0 ],
    {Epat,Vs1}.

ets_pat({_}=V, Vn, Vs) ->
    case find(V, Vs) of
	{yes,Ev} -> {Ev,Vn,Vs};
	no ->
	    Ev = ets_var(Vn),
	    {Ev,Vn-1,[{V,Ev}|Vs]}
    end;
ets_pat([H0|T0], Vn0, Vs0) ->
    {T1,Vn1,Vs1} = ets_pat(T0, Vn0, Vs0),	%Right to left!
    {H1,Vn2,Vs2} = ets_pat(H0, Vn1, Vs1),
    {[H1|T1],Vn2,Vs2};
ets_pat(P, Vn0, Vs0) when is_tuple(P), size(P) >= 2 ->
    {As,Vn1,Vs1} = ets_pat_arg(P, Vn0, Vs0, size(P), []),
    {list_to_tuple([element(1, P)|As]),Vn1,Vs1};
ets_pat(P, Vn, Vs) -> {P,Vn,Vs}.		%Constant

ets_pat_arg(_P, Vn, Vs, 1, As) -> {As,Vn,Vs};
ets_pat_arg(P, Vn0, Vs0, I, As) ->
    {A,Vn1,Vs1} = ets_pat(element(I, P), Vn0, Vs0),
    ets_pat_arg(P, Vn1, Vs1, I-1, [A|As]).

find(V, [{V,Ev}|_Vs]) -> {yes,Ev};
find(V, [_P|Vs]) -> find(V, Vs);
find(_V, []) -> no.

ets_var(1) -> '$1';
ets_var(2) -> '$2';
ets_var(3) -> '$3';
ets_var(4) -> '$4';
ets_var(5) -> '$5';
ets_var(6) -> '$6';
ets_var(7) -> '$7';
ets_var(8) -> '$8';
ets_var(9) -> '$9';
ets_var(10) -> '$10';
ets_var(11) -> '$11';
ets_var(12) -> '$12';
ets_var(13) -> '$13';
ets_var(14) -> '$14';
ets_var(15) -> '$15';
ets_var(16) -> '$16'. 
