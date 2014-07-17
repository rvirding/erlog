%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Июль 2014 16:06
%%%-------------------------------------------------------------------
-module(ec_body).
-author("tihon").

-include("erlog_core.hrl").

%% API
-export([body_instance/5, prove_body/1, unify_prove_body/3, unify_prove_body/5, body_term/3, well_form_body/4, well_form_body/3]).

%% prove_body(Body, ChoicePoints, Bindings, VarNum, Database) ->
%%      {succeed,ChoicePoints,NewBindings,NewVarNum,NewDatabase}.
%% Prove the goals in a body. Remove the first goal and try to prove
%% it. Return when there are no more goals. This is how proving a
%% goal/body succeeds.
prove_body(Params = #param{goal = [G | Gs]}) -> %TODO use lists:foldr instead!
	%%io:fwrite("PB: ~p\n", [{G,Gs,Cps}]),
	ec_goals:prove_goal(Params#param{goal = G, next_goal = Gs});
prove_body(#param{goal = [], choice = Cps, bindings = Bs, var_num = Vn, database = Db}) ->
	%%io:fwrite("Cps: ~p\nCut: ~p\nVar: ~p\nVar: ~p\n",
	%%      [get(erlog_cps),get(erlog_cut),get(erlog_var),dict:size(Bs)]),
	%%io:fwrite("PB: ~p\n", [Cps]),
	{succeed, Cps, Bs, Vn, Db}.      %No more body  %TODO why should we return database?

%% unify_prove_body(Term1, Term2, Next, ChoicePoints, Bindings, VarNum, Database) ->
%%	void.
%% Unify Term1 = Term2, on success prove body Next else fail.
unify_prove_body(T1, T2, Params = #param{next_goal = Next, bindings = Bs0}) ->
	case ec_unify:unify(T1, T2, Bs0) of
		{succeed, Bs1} -> prove_body(Params#param{goal = Next, bindings = Bs1});
		fail -> erlog_errors:fail(Params)
	end.

%% unify_prove_body(A1, B1, A2, B2, Next, ChoicePoints, Bindings, VarNum, Database) ->
%%	void.
%% Unify A1 = B1, A2 = B2, on success prove body Next else fail.
unify_prove_body(A1, B1, A2, B2, Params = #param{bindings = Bs0}) ->
	case ec_unify:unify(A1, B1, Bs0) of
		{succeed, Bs1} -> unify_prove_body(A2, B2, Params#param{bindings = Bs1});
		fail -> erlog_errors:fail(Params)
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
	{G1, Rs2, Vn2} = ec_term:term_instance(G0, Rs1, Vn1),
	{[G1 | Gs1], Rs2, Vn2};
body_instance([], Tail, Rs, Vn, _Label) -> {Tail, Rs, Vn}.

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
	ec_support:functor(Goal),        %Check goal
	{[Goal | Tail], Cut}.

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
	{G1, Rs2, Vn2} = ec_term:term_instance(G0, Rs1, Vn1),
	{body_conj(G1, Gs1), Rs2, Vn2};
body_term([], Rs, Vn) -> {true, Rs, Vn}.


body_conj(L, true) -> L;
body_conj(L, R) -> {',', L, R}.