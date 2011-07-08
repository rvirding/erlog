%%% File    : erlog_ets.erl
%%% @author  : Robert Virding
%%%
%%% @doc ETS interface for Erlog.
%%%
%%% @copyright Robert Virding. This stuff is mine, distributed without
%%% warranty "as is" and may not be used commercially without written
%%% permission.
%%% @end

-module(erlog_ets).

-compile(export_all).

-export([assert/1,all/6,keys/6,match/6]).

-import(lists, [foldl/3]).
-import(erlog_int, [add_compiled_proc/4,dderef/2,unify/3,
		    prove_body/5,unify_prove_body/7,fail/2]).

-record(cp, {type,label,data,next,bs,vn}).

assert(Db) ->
    foldl(fun ({Head,M,F}, LDb) -> 
		  add_compiled_proc(Head, M, F, LDb) end, Db,
	  [
	   {{ets_all,{l}},?MODULE,all},
	   {{ets_keys,{t},{k}},?MODULE,keys},
	   {{ets_match,{t},{p}},?MODULE,match}
	  ]).


%%@doc Return all the ETS databases.
%%@spec all(Goal, Next, ChoicePoints, Bindings, VarNum, Database) -> void()
%%      Goal = {ets_all,Tables}
all({ets_all,Var}, Next, Cps, Bs, Vn, Db) ->
    Tabs = ets:all(),
    unify_prove_body(Var, Tabs, Next, Cps, Bs, Vn, Db).

%%@doc Return the keys in an ETS database one at a time over backtracking.
%%
%%      Goal = {ets_keys,Table,Key}
%%@end
%%@spec keys(Goal, Next, ChoicePoints, Bindings, VarNum, Database) -> void()
keys({ets_keys,Tab0,KeyVar}, Next, Cps, Bs, Vn, Db) ->
    Tab1 = dderef(Tab0, Bs),
    case ets:first(Tab1) of
	'$end_of_table' -> fail(Cps, Db);
	Key -> keys_loop(Tab1, Key, KeyVar, Next, Cps, Bs, Vn, Db)
    end.

keys_loop(Tab, Key, KeyVar, Next, Cps, Bs, Vn, Db) ->
    FailFun = fun(LCp, LCps, LDb) ->
		      keys_fail(LCp, LCps, LDb, Tab, Key, KeyVar)
	      end,
    C = #cp{type=compiled,data=FailFun,next=Next,bs=Bs,vn=Vn},
    unify_prove_body(KeyVar, Key, Next, [C|Cps], Bs, Vn, Db).

keys_fail(#cp{next=Next,bs=Bs,vn=Vn}, Cps, Db, Tab, PrevKey, KeyVar) ->
    case ets:next(Tab, PrevKey) of
	'$end_of_table' -> fail(Cps, Db);	    
	Key -> keys_loop(Tab, Key, KeyVar, Next, Cps, Bs, Vn, Db)
    end.

%%@doc
%% Match objects in an ETS database one at a time over backtracking
%% using Pattern in goal. Variables in Pattern are bound for each
%% object matched.
%%
%% Goal = {ets_match,Table,Pattern}.
%%@end
%%@spec match(Goal, Next, ChoicePoints, Bindings, VarNum, Database) -> void()
match({ets_match,Tab0,Pat0}, Next, Cps, Bs, Vn, Db) ->
    Tab1 = dderef(Tab0, Bs),
    Pat1 = dderef(Pat0, Bs),
    {Epat,Vs} = ets_pat(Pat1),
    match_loop(ets:match(Tab1, Epat, 10), Next, Cps, Bs, Vn, Db, Epat, Vs).

match_loop({[M|Ms],Cont}, Next, Cps, Bs, Vn, Db, Epat, Vs) ->
    FailFun = fun (LCp, LCps, LDb) ->
		      match_fail(LCp, LCps, LDb, Epat, Vs, {Ms,Cont})
	      end,
    Cp = #cp{type=compiled,data=FailFun,next=Next,bs=Bs,vn=Vn},
    unify_prove_body(Vs, M, Next, [Cp|Cps], Bs, Vn, Db);
match_loop({[],Cont}, Next, Cps, Bs, Vn, Db, Epat, Vs) ->
    match_loop(ets:match(Cont), Next, Cps, Bs, Vn, Db, Epat, Vs);
match_loop('$end_of_table', _Next, Cps, _Bs, _Vn, Db, _Epat, _Vs) ->
    fail(Cps, Db).

match_fail(#cp{next=Next,bs=Bs,vn=Vn}, Cps, Db, Epat, Vs, Ms) ->
    match_loop(Ms, Next, Cps, Bs, Vn, Db, Epat, Vs).

%%@doc Convert a term into an ETS pattern replacing variables with the ETS
%% pattern variables. Also return a list of variables in the same
%% order as ETS will return the list of values. This is slightly
%% tricky as the order they are in ETS which is not the same as term
%% order so they can not be easily sorted. Sigh!
%%@end
%%@spec ets_pat(Term) -> {EtsPattern,VarList}
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
ets_var(11) -> '$11'.
