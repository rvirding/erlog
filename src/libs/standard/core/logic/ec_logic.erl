%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc core logic code of erlog_code functions
%%%
%%% @end
%%% Created : 15. Июль 2014 16:02
%%%-------------------------------------------------------------------
-module(ec_logic).
-author("tihon").

-include("erlog_core.hrl").

%% API
-export([initial_goal/1,
	check_goal/6,
	prove_findall/4,
	prove_ecall/3,
	prove_clause/3,
	prove_current_predicate/2,
	prove_predicates/3,
	prove_retract/2,
	prove_retractall/2,
	retract_clauses/4, parse_int/1, to_string/1]).

%% prove_findall(Term, Goal, Bag, Param)
%% Do findall on Goal and return list of each Term in Bag. We are
%% sneaky here and use the database to keep the list using the
%% current VarNum as tag. This is done in the internal goal
%% {findall}. Then when findall finally fails which catch it in
%% fail_findall which cleans up by removing special database entry
%% and unifying Bag.
prove_findall(T, G, B0, Param = #param{bindings = Bs, choice = Cps, next_goal = Next, var_num = Vn, database = Db}) ->
	Label = Vn,
	Tag = Vn + 1,  %Increment to avoid clashes
	{Next1, _} = ec_logic:check_goal(G, [{findall, Tag, T}], Bs, Db, false, Label),
	B1 = partial_list(B0, Bs),
	Cp = #cp{type = findall, data = {Tag, B1}, next = Next, bs = Bs, vn = Vn},
	erlog_memory:raw_store(Db, Tag, []),  %Initialise collection
	%% Catch case where an erlog error occurs when cleanup database.
	try
		ec_core:prove_body(Param#param{goal = Next1, choice = [Cp | Cps], bindings = Bs, var_num = Vn + 1})
	catch
		throw:{erlog_error, E, Dba} ->
			Dbb = erlog_memory:raw_erase(Dba, Tag),  %Clear special entry
			erlog_errors:erlog_error(E, Dbb)
	end.

%% prove_ecall(Generator, Value, Next, ChoicePoints, Bindings, VarNum, Database) ->
%%	void.
%% Call an external (Erlang) generator and handle return value, either
%% succeed or fail.
prove_ecall(Efun, Val, Param = #param{next_goal = Next, choice = Cps, bindings = Bs, var_num = Vn}) ->
	case Efun() of
		{succeed, Ret, Cont} ->      %Succeed and more choices
			Cp = #cp{type = ecall, data = {Cont, Val}, next = Next, bs = Bs, vn = Vn},
			ec_body:unify_prove_body(Val, Ret, Param#param{choice = [Cp | Cps]});
		{succeed_last, Ret} ->      %Succeed but last choice
			ec_body:unify_prove_body(Val, Ret, Param);
		fail -> erlog_errors:fail(Param)      %No more
	end.

%% prove_clause(Head, Body, Next, ChoicePoints, Bindings, VarNum, DataBase) ->
%%      void.
%% Unify clauses matching with functor from Head with both Head and Body.
prove_clause(H, B, Param = #param{database = Db}) ->
	Functor = ec_support:functor(H),
	case erlog_memory:get_procedure(Db, Functor) of
		{clauses, Cs} -> ec_unify:unify_clauses(H, B, Cs, Param);
		{code, _} ->
			erlog_errors:permission_error(access, private_procedure, ec_support:pred_ind(Functor), Db);
		built_in ->
			erlog_errors:permission_error(access, private_procedure, ec_support:pred_ind(Functor), Db);
		undefined -> erlog_errors:fail(Param)
	end.

%% prove_current_predicate(PredInd, Next, ChoicePoints, Bindings, VarNum, DataBase) ->
%%      void.
%% Match functors of existing user (interpreted) predicate with PredInd.
prove_current_predicate(Pi, Param = #param{database = Db}) ->
	case Pi of
		{'/', _, _} -> ok;
		{_} -> ok;
		Other -> erlog_errors:type_error(predicate_indicator, Other)
	end,
	Fs = erlog_memory:get_interp_functors(Db),
	prove_predicates(Pi, Fs, Param).

prove_predicates(Pi, [F | Fs], Param = #param{next_goal = Next, choice = Cps, bindings = Bs, var_num = Vn}) ->
	Cp = #cp{type = current_predicate, data = {Pi, Fs}, next = Next, bs = Bs, vn = Vn},
	ec_body:unify_prove_body(Pi, ec_support:pred_ind(F), Param#param{choice = [Cp | Cps]});
prove_predicates(_Pi, [], Param) -> erlog_errors:fail(Param).

%% prove_retract(Clause, Next, ChoicePoints, Bindings, VarNum, Database) ->
%%      void.
%% Retract clauses in database matching Clause.
prove_retract({':-', H, B}, Params) ->
	prove_retract(H, B, Params);
prove_retract(H, Params) ->
	prove_retract(H, true, Params).

prove_retractall({':-', H, B}, Params) ->
	prove_retractall(H, B, Params);
prove_retractall(H, Params) ->
	prove_retractall(H, true, Params).

%% check_goal(Goal, Next, Bindings, Database, CutAfter, CutLabel) ->
%%      {WellFormedBody,HasCut}.
%% Check to see that Goal is bound and ensure that it is well-formed.
check_goal(G0, Next, Bs, Db, Cut, Label) ->
	case ec_support:dderef(G0, Bs) of
		{_} -> erlog_errors:instantiation_error(Db);    %Must have something to call
		G1 ->
			case catch {ok, well_form_goal(G1, Next, Cut, Label)} of
				{erlog_error, E} -> erlog_errors:erlog_error(E, Db);
				{ok, GC} -> GC      %Body and cut
			end
	end.

%% retract_clauses(Head, Body, Clauses, Next, ChoicePoints, Bindings, VarNum, Database) ->
%%      void.
%% Try to retract Head and Body using Clauses which all have the same functor.
retract_clauses(_Ch, _Cb, [], Param) -> erlog_errors:fail(Param);
retract_clauses(Ch, Cb, [C | Cs], Param = #param{next_goal = Next, choice = Cps, bindings = Bs0, var_num = Vn0, database = Db}) -> %TODO foreach vs handmade recursion?
	case ec_unify:unify_clause(Ch, Cb, C, Bs0, Vn0) of
		{succeed, Bs1, Vn1} ->
			%% We have found a right clause so now retract it.
			erlog_memory:retract_clause(Db, ec_support:functor(Ch), element(1, C)),
			Cp = #cp{type = retract, data = {Ch, Cb, Cs}, next = Next, bs = Bs0, vn = Vn0},
			ec_core:prove_body(Param#param{goal = Next, choice = [Cp | Cps], bindings = Bs1, var_num = Vn1});
		fail -> retract_clauses(Ch, Cb, Cs, Param)
	end.

%% well_form_goal(Goal, Tail, HasCutAfter, CutLabel) -> {Body,HasCut}.
%%  Check that Goal is well-formed, flatten conjunctions, fix cuts and
%%  add explicit call to top-level variables.
well_form_goal({',', L, R}, Tail0, Cut0, Label) ->
	{Tail1, Cut1} = well_form_goal(R, Tail0, Cut0, Label),
	well_form_goal(L, Tail1, Cut1, Label);
well_form_goal({';', {'->', C0, T0}, E0}, Tail, Cut0, Label) ->
	{T1, Tc} = well_form_goal(T0, Tail, Cut0, Label),
	{C1, _} = well_form_goal(C0, [{cut, Label, true} | T1], true, Label),
	{E1, Ec} = well_form_goal(E0, Tail, Cut0, Label),
	{[{if_then_else, E1, Label} | C1], Tc or Ec};
well_form_goal({';', L0, R0}, Tail, Cut0, Label) ->
	{L1, Lc} = well_form_goal(L0, Tail, Cut0, Label),
	{R1, Rc} = well_form_goal(R0, Tail, Cut0, Label),
	{[{disj, R1} | L1], Lc or Rc};
well_form_goal({'->', C0, T0}, Tail, Cut0, Label) ->
	{T1, Cut1} = well_form_goal(T0, Tail, Cut0, Label),
	%% N.B. an extra cut will be added at run-time!
	{C1, _} = well_form_goal(C0, [{cut, Label, true} | T1], true, Label),
	{[{if_then, Label} | C1], Cut1};
well_form_goal({once, G}, Tail, Cut, Label) ->
	{G1, _} = well_form_goal(G, [{cut, Label, true} | Tail], true, Label),
	{[{once, Label} | G1], Cut};
well_form_goal({V}, Tail, Cut, _Label) ->
	{[{call, {V}} | Tail], Cut};
well_form_goal(true, Tail, Cut, _Label) -> {Tail, Cut}; %No-op
well_form_goal(fail, _Tail, _Cut, _Label) -> {[fail], false};  %No further
well_form_goal('!', Tail, Cut, Label) ->
	{[{cut, Label, not Cut} | Tail], true};
well_form_goal(Goal, Tail, Cut, _Label) ->
	ec_support:functor(Goal),        %Check goal
	{[Goal | Tail], Cut}.

parse_int(Float) when is_float(Float) -> round(Float);
parse_int(String) when is_list(String) ->
	case string:to_integer(String) of
		{error, E} -> throw(E);
		{Res, _} -> Res
	end;
parse_int(Atom) when is_atom(Atom) ->
	parse_int(atom_to_list(Atom)).

to_string(Int) when is_integer(Int) -> integer_to_list(Int);
to_string(Value) -> lists:flatten(io_lib:format("~p", [Value])).

%% initial_goal(Goal) -> {Goal,Bindings,NewVarNum}.
%% initial_goal(Goal, Bindings, VarNum) -> {Goal,NewBindings,NewVarNum}.
%% Check term for well-formedness as an Erlog term and replace '_'
%% variables with unique numbered variables. Error on non-well-formed
%% goals.
initial_goal(Goal) -> initial_goal(Goal, ec_support:new_bindings(), 0).


%% @private
initial_goal({'_'}, Bs, Vn) -> {{Vn}, Bs, Vn + 1};  %Anonymous variable
initial_goal({Name} = Var0, Bs, Vn) when is_atom(Name) ->
	case ec_support:get_binding(Var0, Bs) of
		{ok, Var1} -> {Var1, Bs, Vn};
		error ->
			Var1 = {Vn},
			{Var1, ec_support:add_binding(Var0, Var1, Bs), Vn + 1}
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

%% @private
%% partial_list(Term, Bindings) -> Term.
%% Dereference all variables and check if partial list.
partial_list([], _) -> [];
partial_list([H | T0], Bs) ->
	T1 = partial_list(T0, Bs),
	[H | T1];
partial_list({V} = Var, Bs) ->
	case ?BIND:find(V, Bs) of
		{ok, T} -> partial_list(T, Bs);
		error -> Var
	end;
partial_list(Other, _) -> erlog_errors:type_error(list, Other).

%% @private
prove_retract(H, B, Params = #param{database = Db}) ->
	Functor = ec_support:functor(H),
	case erlog_memory:get_procedure(Db, Functor) of
		{clauses, Cs} -> retract_clauses(H, B, Cs, Params);
		{code, _} ->
			erlog_errors:permission_error(modify, static_procedure, ec_support:pred_ind(Functor), Db);
		built_in ->
			erlog_errors:permission_error(modify, static_procedure, ec_support:pred_ind(Functor), Db);
		undefined -> erlog_errors:fail(Params)
	end.

%% @private
prove_retractall(H, B, Params = #param{next_goal = Next, bindings = Bs0, var_num = Vn0, database = Db}) ->
	Functor = ec_support:functor(H),
	case erlog_memory:get_procedure(Db, Functor) of
		{clauses, Cs} ->
			lists:foreach(
				fun(Clause) ->
					case ec_unify:unify_clause(H, B, Clause, Bs0, Vn0) of
						{succeed, _, _} ->
							erlog_memory:retract_clause(Db, ec_support:functor(H), element(1, Clause));
						fail -> ok
					end
				end, Cs),
			ec_core:prove_body(Params#param{goal = Next});
		{code, _} ->
			erlog_errors:permission_error(modify, static_procedure, ec_support:pred_ind(Functor), Db);
		built_in ->
			erlog_errors:permission_error(modify, static_procedure, ec_support:pred_ind(Functor), Db);
		undefined -> ec_core:prove_body(Params#param{goal = Next})
	end.
