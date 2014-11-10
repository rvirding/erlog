%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Авг. 2014 17:48
%%%-------------------------------------------------------------------
-module(erlog_ed_logic).
-author("tihon").

-include("erlog_core.hrl").

%% API
-export([expand_term/1, phrase/1]).

%% expand_term(Term) -> {ExpTerm}.
%% expand_term(Term, VarNum) -> {ExpTerm,NewVarNum}.
%%  Handle DCG expansion. We do NOT work backwards.
expand_term(Term) ->
	{Exp, _} = expand_term(Term, 0),
	Exp.

expand_term({'-->', _, _} = Term, Vn) ->
	dcg_rule(Term, Vn);
expand_term(Term, Vn) -> {Term, Vn}.

phrase(Params = #param{goal = Goal, next_goal = Next0, bindings = Bs, var_num = Vn0}) ->
	{phrase, GRBody, S0, S} = erlog_ec_support:dderef(Goal, Bs),
	{Body, Vn1} = erlog_ed_logic:dcg_body(GRBody, S0, S, Vn0),
	%% io:format("~p\n", [Body]),
	Next1 = [{call, Body} | Next0],    %Evaluate body
	erlog_ec_core:prove_body(Params#param{goal = Next1, var_num = Vn1}).

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