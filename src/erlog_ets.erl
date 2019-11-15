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

-export([load/1,all_1/3,key_2/3,match_2/3]).

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
		 {{ets_key,2},?MODULE,key_2},
		 {{ets_match,2},?MODULE,match_2}
		]),
    Db1.

%% all_1(Head, NextGoal, State) -> void().
%%      Goal = {ets_all,Tables}.
%% Return all the ETS databases.

all_1({ets_all,Var}, Next, St) ->
    Tabs = ets:all(),
    unify_prove_body(Var, Tabs, Next, St).

%% key_2(Head, NextGoal, State) -> void().
%%      Goal = {ets_key,Table,Key}.
%% Return the key in an ETS database one at a time over backtracking.

key_2({ets_key,Tab0,KeyVar}, Next, #est{bs=Bs}=St) ->
    Tab1 = dderef(Tab0, Bs),
    %% io:format("kf: ~p ~p\n", [Tab1,ets:first(Tab1)]),
    Key = ets:first(Tab1),
    key_2_loop(Tab1, Key, KeyVar, Next, St).

key_2_loop(_Tab, '$end_of_table', _KeyVar, _Next, St) ->
    fail(St);
key_2_loop(Tab, Key, KeyVar, Next, #est{cps=Cps,bs=Bs,vn=Vn}=St) ->
    FailFun = fun(LCp, LCps, Lst) ->
		      key_2_fail(LCp, LCps, Lst, Tab, Key, KeyVar)
	      end,
    Cp = #cp{type=compiled,data=FailFun,next=Next,bs=Bs,vn=Vn},
    unify_prove_body(KeyVar, Key, Next, St#est{cps=[Cp|Cps]}).

key_2_fail(#cp{next=Next,bs=Bs,vn=Vn}, Cps, St, Tab, PrevKey, KeyVar) ->
    %% io:format("kn: ~p ~p\n", [PrevKey,ets:next(Tab,PrevKey)]),
    NextKey = ets:next(Tab, PrevKey),
    key_2_loop(Tab, NextKey, KeyVar, Next, St#est{cps=Cps,bs=Bs,vn=Vn}).

%% match_2(Head, Next, State) -> void().
%%      Head = {ets_match,Table,Pattern}.
%% Match objects in an ETS database one at a time over backtracking
%% using Pattern in goal. Variables in Pattern are bound for each
%% object matched.

match_2({ets_match,Tab0,Pat0}, Next, #est{bs=Bs}=St) ->
    Tab1 = dderef(Tab0, Bs),
    Pat1 = dderef(Pat0, Bs),
    {Epat,Vs} = ets_pat(Pat1),
    %% io:format("Pat1: ~p\nEpat: ~p\nVs:  ~p\n", [Pat1,Epat,Vs]),
    match_2_loop(ets:match(Tab1, Epat, 10), Next, St, Epat, Vs).

match_2_loop({[M|Ms],Cont}, Next, #est{cps=Cps,bs=Bs,vn=Vn}=St, Epat, Vs) ->
    %% io:format("m2l: ~p\n     ~p\n",[M,Vs]),
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
%% pattern variables. Also return a list of pattern variable/erlog
%% variable in the same order as ETS will return the list of
%% values. We do this by strictly building backwards and adding the
%% '$N' variables form the back incrementing the index. They will then
%% be in reverse order.

ets_pat(Pat) ->
    {Epat,_Vn,Vs0} = ets_pat(Pat, 0, []),
    Evs = [ V || {V,_Ec} <- lists:reverse(Vs0) ],
    {Epat,Evs}.

ets_pat({'_'}, Vn, Vs) ->			%_ variable passed on as is
    {'_',Vn, Vs};
ets_pat({_}=V, Vn, Vs) ->
    case find(V, Vs) of
	{yes,Ev} -> {Ev,Vn,Vs};
	no ->
	    Ev = ets_var(Vn),
	    {Ev,Vn+1,[{V,Ev}|Vs]}
    end;
ets_pat([H|T], Vn0, Vs0) ->
    {Et,Vn1,Vs1} = ets_pat(T, Vn0, Vs0),
    {Eh,Vn2,Vs2} = ets_pat(H, Vn1, Vs1),
    {[Eh|Et],Vn2,Vs2};
ets_pat(T, Vn0, Vs0) when is_tuple(T), tuple_size(T) >= 2 ->
    {Ees,Vn1,Vs1} = ets_pat_elements(T, tuple_size(T), [], Vn0, Vs0),
    {list_to_tuple(Ees),Vn1,Vs1};
ets_pat(C, Vn, Vs) -> {C,Vn,Vs}.		%Constant for erlog

ets_pat_elements(_T, 0, Ees, Vn, Vs) -> {Ees,Vn,Vs};
ets_pat_elements(T, I, Ees, Vn0, Vs0) ->
    {Ee,Vn1,Vs1} = ets_pat(element(I, T), Vn0, Vs0),
    ets_pat_elements(T, I-1, [Ee|Ees], Vn1, Vs1).

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
ets_var(16) -> '$16';
ets_var(N) -> 					%Do the rest less efficiently
    list_to_atom([$$|integer_to_list(N)]).
