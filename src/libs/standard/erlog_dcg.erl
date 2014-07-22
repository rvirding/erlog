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

%% File    : erlog_dcg.erl
%% Author  : Robert Virding
%% Purpose : DCG conversion and procedures.

-module(erlog_dcg).

-include("erlog_core.hrl").
-include("erlog_dcg.hrl").

-export([expand_term/1, expand_term/2]).
-export([expand_term_2/1, phrase_3/1]).
-export([load/1]).

load(Db) ->
	%% Compiled DCG predicates.
	lists:foreach(fun(Proc) -> erlog_memory:add_compiled_proc(Db, Proc) end, ?ERLOG_DCG),
	%% Interpreted DCG predicates.
	lists:foreach(fun(Clause) -> erlog_memory:assertz_clause(Db, Clause) end,
		[
			%% 'C'([H|T], H, T).
			%% {'C',[{1}|{2}],{1},{2}},		%For DCGs
			%% phrase(V, L) :- phrase(V, L, []).
			{':-', {phrase, {1}, {2}}, {phrase, {1}, {2}, []}}
			%% phrase(V, L, R) :-
			%%     V =.. Z, append(Z, [L,R], G), C =.. G, C.
			%% {':-',{phrase,{1},{2},{3}},
			%%  {',',{'=..',{1},{4}},{',',{append,{4},[{2},{3}],{5}},
			%% 			{',',{'=..',{6},{5}},{6}}}}}
		]).

%% expand_term_2(Goal, NextGoal, ChoicePoints, Bindings, VarNum, Database) ->
%%     void
%%  Call the expand_term/2 predicate.
expand_term_2(Param = #param{goal = Goal, bindings = Bs, var_num = Vn0}) ->
	{expand_term, DCGRule, A2} = ec_support:dderef(Goal, Bs),
	{Exp, Vn1} = expand_term(DCGRule, Vn0),
	ec_body:unify_prove_body(A2, Exp, Param#param{var_num = Vn1}).

%% phrase_3(Goal, NextGoal, ChoicePoints, Bindings, VarNum, Database) -> void.
%%  Call the phrase/3 preidicate. We could easily do this in prolog
%%  except for that it calls dcg_body/4 which is not exported.
%%
%%  phrase(GRBody, S0, S) -> dcg_body(GRBody, S0, S, Goal), call(Goal).
phrase_3(Param = #param{goal = Goal, next_goal = Next0, bindings = Bs, var_num = Vn0}) ->
	{phrase, GRBody, S0, S} = ec_support:dderef(Goal, Bs),
	{Body, Vn1} = dcg_body(GRBody, S0, S, Vn0),
	%% io:format("~p\n", [Body]),
	Next1 = [{call, Body} | Next0],    %Evaluate body
	ec_body:prove_body(Param#param{goal = Next1, var_num = Vn1}).

%% expand_term(Term) -> {ExpTerm}.
%% expand_term(Term, VarNum) -> {ExpTerm,NewVarNum}.
%%  Handle DCG expansion. We do NOT work backwards.
expand_term(Term) ->
	{Exp, _} = expand_term(Term, 0),
	Exp.

expand_term({'-->', _, _} = Term, Vn) ->
	dcg_rule(Term, Vn);
expand_term(Term, Vn) -> {Term, Vn}.

%% dcg_rule(Term, VarNum) -> {ExpTerm,NewVarNum}.
%% dcg_rule(DCGRule, VarIn, VarOout, VarNum) -> {ExpTerm,NewVarNum}.
%% dcg_non_term(NonTerminal, VarIn, VarOut) -> ExpTerm.
%% dcg_body(BodyTerm, VarIn, VarOut, VarNum) -> {ExpBody,NewVarOut,NewVarNum}.
%% dcg_goal(BodyGoal, VarIn, VarOut, VarNum) -> {ExpGaol,NewVarOut,NewVarNum}.
%% dcg_terminal(Terminals, VarIn, VarOut, VarNum) ->
%%     {ExpTerms,NewVarOut,NewVarNum}.
%%  dcg_body and dcg_goal do smae the thing except the dcg_body
%%  guarantees the output variable is the one we specify. It may
%%  insert an explicit '=' to get this.
dcg_rule(DCGRule, Vn0) ->
	S0 = {Vn0},
	S = {Vn0 + 1},
	dcg_rule(DCGRule, S0, S, Vn0 + 2).

dcg_rule({'-->', {',', H, RHC}, B}, S0, S, Vn0) ->
	S1 = {Vn0},
	Head = dcg_non_term(H, S0, S),
	{Goal1, S2, Vn1} = dcg_goal(B, S0, S1, Vn0 + 1),
	{Goal2, Vn2} = dcg_terminals(RHC, S, S2, Vn1),
	{{':-', Head, {',', Goal1, Goal2}}, Vn2};
dcg_rule({'-->', H, B}, S0, S, Vn0) ->
	Head = dcg_non_term(H, S0, S),
	{Body, Vn1} = dcg_body(B, S0, S, Vn0),
	{{':-', Head, Body}, Vn1}.

dcg_non_term(A, S0, S) when is_atom(A) -> {A, S0, S};
dcg_non_term(T, S0, S) when ?IS_FUNCTOR(T) ->
	list_to_tuple(tuple_to_list(T) ++ [S0, S]);
dcg_non_term(Other, _, _) -> erlog_errors:type_error(callable, Other).

dcg_body({',', G0, B0}, S0, S, Vn0) ->
	S1 = {Vn0},
	{G1, S2, Vn1} = dcg_goal(G0, S0, S1, Vn0 + 1),
	{B1, Vn2} = dcg_body(B0, S2, S, Vn1),
	{{',', G1, B1}, Vn2};
dcg_body(G0, S0, S, Vn0) ->
	case dcg_goal(G0, S0, S, Vn0) of
		{G1, S, Vn1} -> {G1, Vn1};      %Already uses S
		{G1, S1, Vn1} ->        %So we get S!
			%% io:format("~p\n", [{G1,S0,S1,S}]),
			{{',', G1, {'=', S1, S}}, Vn1}
	end.

dcg_goal('!', S0, _, Vn) -> {'!', S0, Vn};
dcg_goal({_} = V, S0, S, Vn) ->
	{{phrase, V, S0, S}, S, Vn};
dcg_goal({'{}', G}, S0, _, Vn) -> {G, S0, Vn};
dcg_goal({',', L0, R0}, S0, S, Vn0) ->
	S1 = {Vn0},
	{L1, S2, Vn1} = dcg_goal(L0, S0, S1, Vn0 + 1),
	{R1, S3, Vn2} = dcg_goal(R0, S2, S, Vn1),
	{{',', L1, R1}, S3, Vn2};
dcg_goal({';', L0, R0}, S0, S, Vn0) ->
	{L1, Vn1} = dcg_body(L0, S0, S, Vn0),
	{R1, Vn2} = dcg_body(R0, S0, S, Vn1),
	{{';', L1, R1}, S, Vn2};
dcg_goal({'->', GRIf, GRThen}, S0, S, Vn0) ->
	S1 = {Vn0},
	{If, S2, Vn1} = dcg_goal(GRIf, S0, S1, Vn0 + 1),
	{Then, S3, Vn2} = dcg_goal(GRThen, S2, S, Vn1),
	{{'->', If, Then}, S3, Vn2};
dcg_goal({'\\+', G0}, S0, S, Vn) ->
	{G1, _, _} = dcg_goal(G0, S0, S, Vn),
	{{'\\+', G1}, S0, Vn};
dcg_goal(Lits, S0, S, Vn0) when is_list(Lits) ->
	{ELits, Vn1} = dcg_terminals(Lits, S0, S, Vn0),
	{ELits, S, Vn1};
dcg_goal(NonT, S0, S, Vn) ->
	Goal = dcg_non_term(NonT, S0, S),
	{Goal, S, Vn}.

dcg_terminals(Lits, S0, S, Vn) ->    %Without 'C'/3
	{{'=', S0, Lits ++ S}, Vn}.