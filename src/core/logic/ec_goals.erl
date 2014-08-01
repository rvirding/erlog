%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Июль 2014 16:02
%%%-------------------------------------------------------------------
-module(ec_goals).
-author("tihon").

-include("erlog_core.hrl").

%% API
-export([prove_goal/1, initial_goal/1, check_goal/6]).

%% prove_goal(Goal, NextGoal, ChoicePoints, Bindings, VarNum, Database) ->
%%	{succeed,ChoicePoints,NewBindings,NewVarNum,NewDatabase} |
%%      {fail,NewDatabase}.
%% Prove one goal. We seldom return succeed here but usually go directly to
%% to NextGoal.
%% Handle built-in predicates here. RTFM for a description of the
%% built-ins. Hopefully we do the same.

%% Logic and control. Conjunctions are handled in prove_body and true
%% has been compiled away.
prove_goal(Param = #param{goal = {call, G}, next_goal = Next0, choice = Cps,
	bindings = Bs, var_num = Vn, database = Db}) ->
	%% Only add cut CP to Cps if goal contains a cut.
	Label = Vn,
	case check_goal(G, Next0, Bs, Db, false, Label) of
		{Next1, true} ->
			%% Must increment Vn to avoid clashes!!!
			Cut = #cut{label = Label},
			ec_body:prove_body(Param#param{goal = Next1, choice = [Cut | Cps], var_num = Vn + 1});
		{Next1, false} -> ec_body:prove_body(Param#param{goal = Next1, var_num = Vn + 1})
	end;
prove_goal(Param = #param{goal = {{cut}, Label, Last}}) ->
	%% Cut succeeds and trims back to cut ancestor.
	ec_support:cut(Label, Last, Param);
prove_goal(Param = #param{goal = {{disj}, R}, next_goal = Next, choice = Cps, bindings = Bs, var_num = Vn}) ->
	%% There is no L here, it has already been prepended to Next.
	Cp = #cp{type = disjunction, next = R, bs = Bs, vn = Vn},
	ec_body:prove_body(Param#param{goal = Next, choice = [Cp | Cps]});
prove_goal(Params = #param{goal = fail}) -> erlog_errors:fail(Params);
prove_goal(Param = #param{goal = {{if_then}, Label}, next_goal = Next, choice = Cps}) ->
	%% We effetively implement ( C -> T ) with ( C, !, T ) but cuts in
	%% C are local to C.
	%% There is no ( C, !, T ) here, it has already been prepended to Next.
	%%io:fwrite("PG(->): ~p\n", [{Next}]),
	Cut = #cut{label = Label},
	ec_body:prove_body(Param#param{goal = Next, choice = [Cut | Cps]});
prove_goal(Param = #param{goal = {{if_then_else}, Else, Label}, next_goal = Next, choice = Cps, bindings = Bs, var_num = Vn}) ->
	%% Need to push a choicepoint to fail back to inside Cond and a cut
	%% to cut back to before Then when Cond succeeds. #cp{type=if_then_else}
	%% functions as both as is always removed whatever the outcome.
	%% There is no ( C, !, T ) here, it has already been prepended to Next.
	Cp = #cp{type = if_then_else, label = Label, next = Else, bs = Bs, vn = Vn},
	%%io:fwrite("PG(->;): ~p\n", [{Next,Else,[Cp|Cps]}]),
	ec_body:prove_body(Param#param{goal = Next, choice = [Cp | Cps]});
prove_goal(Param = #param{goal = {'\\+', G}, next_goal = Next0, choice = Cps, bindings = Bs, var_num = Vn, database = Db}) ->
	%% We effectively implementing \+ G with ( G -> fail ; true ).
	Label = Vn,
	{Next1, _} = check_goal(G, [{{cut}, Label, true}, fail], Bs, Db, true, Label),
	Cp = #cp{type = if_then_else, label = Label, next = Next0, bs = Bs, vn = Vn},
	%%io:fwrite("PG(\\+): ~p\n", [{G1,[Cp|Cps]]),
	%% Must increment Vn to avoid clashes!!!
	ec_body:prove_body(Param#param{goal = Next1, choice = [Cp | Cps], var_num = Vn + 1});
prove_goal(Param = #param{goal = {{once}, Label}, next_goal = Next, choice = Cps}) ->
	%% We effetively implement once(G) with ( G, ! ) but cuts in
	%% G are local to G.
	%% There is no ( G, ! ) here, it has already been prepended to Next.
	Cut = #cut{label = Label},
	ec_body:prove_body(Param#param{goal = Next, choice = [Cut | Cps]});
prove_goal(Param = #param{goal = repeat, next_goal = Next, choice = Cps, bindings = Bs, var_num = Vn}) ->
	Cp = #cp{type = disjunction, next = [repeat | Next], bs = Bs, vn = Vn},
	ec_body:prove_body(Param#param{goal = Next, choice = [Cp | Cps]});
%% Clause creation and destruction.
prove_goal(Param = #param{goal = {abolish, Pi0}, next_goal = Next, bindings = Bs, database = Db}) ->
	case ec_support:dderef(Pi0, Bs) of
		{'/', N, A} when is_atom(N), is_integer(A), A > 0 ->
			erlog_memory:abolish_clauses(Db, {N, A}),
			ec_body:prove_body(Param#param{goal = Next});
		Pi -> erlog_errors:type_error(predicate_indicator, Pi, Db)
	end;
prove_goal(Param = #param{goal = {Assert, C0}, next_goal = Next, bindings = Bs, database = Db})
	when Assert == assert; Assert == assertz ->
	C = ec_support:dderef(C0, Bs),
	erlog_memory:assertz_clause(Db, C),
	ec_body:prove_body(Param#param{goal = Next});
prove_goal(Param = #param{goal = {asserta, C0}, next_goal = Next, bindings = Bs, database = Db}) ->
	C = ec_support:dderef(C0, Bs),
	erlog_memory:asserta_clause(Db, C),
	ec_body:prove_body(Param#param{goal = Next});
prove_goal(Param = #param{goal = {retract, C0}, bindings = Bs}) ->
	C = ec_support:dderef(C0, Bs),
	erlog_core:prove_retract(C, Param);
prove_goal(Param = #param{goal = {retractall, C0}, bindings = Bs}) ->
	C = ec_support:dderef(C0, Bs),
	erlog_core:prove_retractall(C, Param);
%% Clause retrieval and information
prove_goal(Param = #param{goal = {clause, H0, B}, bindings = Bs}) ->
	H1 = ec_support:dderef(H0, Bs),
	erlog_core:prove_clause(H1, B, Param);
prove_goal(Param = #param{goal = {current_predicate, Pi0}, bindings = Bs}) ->
	Pi = ec_support:dderef(Pi0, Bs),
	erlog_core:prove_current_predicate(Pi, Param);
prove_goal(Param = #param{goal = {predicate_property, H0, P}, bindings = Bs, database = Db}) ->
	H = ec_support:dderef(H0, Bs),
	case catch erlog_memory:get_procedure_type(Db, ec_support:functor(H)) of
		built_in -> ec_body:unify_prove_body(P, built_in, Param);
		compiled -> ec_body:unify_prove_body(P, compiled, Param);
		interpreted -> ec_body:unify_prove_body(P, interpreted, Param);
		undefined -> erlog_errors:fail(Param);
		{erlog_error, E} -> erlog_errors:erlog_error(E, Db)
	end;
%% External interface
prove_goal(Param = #param{goal = {ecall, C0, Val}, bindings = Bs, database = Db}) ->
	%% Build the initial call.
	%%io:fwrite("PG(ecall): ~p\n   ~p\n   ~p\n", [dderef(C0, Bs),Next,Cps]),
	Efun = case ec_support:dderef(C0, Bs) of
		       {':', M, F} when is_atom(M), is_atom(F) ->
			       fun() -> M:F() end;
		       {':', M, {F, A}} when is_atom(M), is_atom(F) ->
			       fun() -> M:F(A) end;
		       {':', M, {F, A1, A2}} when is_atom(M), is_atom(F) ->
			       fun() -> M:F(A1, A2) end;
		       {':', M, T} when is_atom(M), ?IS_FUNCTOR(T) ->
			       L = tuple_to_list(T),
			       fun() -> apply(M, hd(L), tl(L)) end;
		       Fun when is_function(Fun) -> Fun;
		       Other -> erlog_errors:type_error(callable, Other, Db)
	       end,
	erlog_core:prove_ecall(Efun, Val, Param);
%% Non-standard but useful.
prove_goal(Param = #param{goal = {writeln, T}, next_goal = Next, bindings = Bs, event_man = Evman}) ->
	%% Display procedure.
	Res = ec_support:write(T, Bs),
	gen_event:notify(Evman, Res),
	ec_body:prove_body(Param#param{goal = Next});
%% File utils
prove_goal(Param = #param{goal = {consult, Name}, next_goal = Next, bindings = Bs, f_consulter = Fcon, database = Db}) ->
	case erlog_file:consult(Fcon, ec_support:dderef(Name, Bs), Db) of
		ok -> ok;
		{Err, Error} when Err == erlog_error; Err == error ->
			erlog_errors:erlog_error(Error, Db)
	end,
	ec_body:prove_body(Param#param{goal = Next});
prove_goal(Param = #param{goal = {reconsult, Name}, next_goal = Next, f_consulter = Fcon, database = Db}) ->
	case erlog_file:reconsult(Fcon, Name, Db) of
		ok -> ok;
		{Err, Error} when Err == erlog_error; Err == error ->
			erlog_errors:erlog_error(Error, Db)
	end,
	ec_body:prove_body(Param#param{goal = Next});
prove_goal(Param = #param{goal = {use, Library}, next_goal = Next, database = Db}) ->
	try Library:load(Db)
	catch
		_:Error ->
			erlog_errors:erlog_error(Error, Db)
	end,
	ec_body:prove_body(Param#param{goal = Next});
prove_goal(Param = #param{goal = {findall, T, G, B}}) ->
	erlog_core:prove_findall(T, G, B, Param);
prove_goal(Param = #param{goal = {{findall}, Tag, T0}, bindings = Bs, database = Db}) ->
	T1 = ec_support:dderef(T0, Bs),
	erlog_memory:raw_append(Db, Tag, T1),  %Append to saved list
	erlog_errors:fail(Param);
prove_goal(Param = #param{goal = {bagof, Goal, Fun, Res}, choice = Cs0, bindings = Bs0, next_goal = Next, var_num = Vn, database = Db}) ->
	Predicates = erlog_memory:finadll(Db, Fun),
	FunList = tuple_to_list(Fun),
	ResultDict = ec_support:collect_alternatives(Goal, FunList, Predicates),
	Collected = dict:fetch_keys(ResultDict),
	[UBs | Choises] = lists:foldr(
		fun(Key, Acc) ->
			UpdBs0 = ec_support:update_result(Key, ResultDict, Res, Bs0),
			UpdBs1 = ec_support:update_vars(Goal, FunList, Key, UpdBs0),
			[#cp{type = disjunction, label = Fun, next = Next, bs = UpdBs1, vn = Vn} | Acc]
		end, Cs0, Collected),
	ec_body:prove_body(Param#param{goal = Next, bindings = UBs#cp.bs, choice = Choises, var_num = Vn + length(Choises)});
%% Now look up the database.
prove_goal(Param = #param{goal = G, database = Db}) ->
%% 	io:fwrite("PG: ~p\n    ~p\n    ~p\n", [dderef(G, Bs),Next,Cps]),
	case catch erlog_memory:get_procedure(Db, ec_support:functor(G)) of
		built_in -> erlog_bips:prove_goal(G, Param);
		{code, {Mod, Func}} -> Mod:Func(G, Param);
		{clauses, Cs} -> erlog_core:prove_goal_clauses(G, Cs, Param);
		undefined -> erlog_errors:fail(Param);
	%% Getting built_in here is an error!
		{erlog_error, E} -> erlog_errors:erlog_error(E, Db)  %Fill in more error data
	end.

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

%% well_form_goal(Goal, Tail, HasCutAfter, CutLabel) -> {Body,HasCut}.
%%  Check that Goal is well-formed, flatten conjunctions, fix cuts and
%%  add explicit call to top-level variables.
well_form_goal({',', L, R}, Tail0, Cut0, Label) ->
	{Tail1, Cut1} = well_form_goal(R, Tail0, Cut0, Label),
	well_form_goal(L, Tail1, Cut1, Label);
well_form_goal({';', {'->', C0, T0}, E0}, Tail, Cut0, Label) ->
	{T1, Tc} = well_form_goal(T0, Tail, Cut0, Label),
	{C1, _} = well_form_goal(C0, [{{cut}, Label, true} | T1], true, Label),
	{E1, Ec} = well_form_goal(E0, Tail, Cut0, Label),
	{[{{if_then_else}, E1, Label} | C1], Tc or Ec};
well_form_goal({';', L0, R0}, Tail, Cut0, Label) ->
	{L1, Lc} = well_form_goal(L0, Tail, Cut0, Label),
	{R1, Rc} = well_form_goal(R0, Tail, Cut0, Label),
	{[{{disj}, R1} | L1], Lc or Rc};
well_form_goal({'->', C0, T0}, Tail, Cut0, Label) ->
	{T1, Cut1} = well_form_goal(T0, Tail, Cut0, Label),
	%% N.B. an extra cut will be added at run-time!
	{C1, _} = well_form_goal(C0, [{{cut}, Label, true} | T1], true, Label),
	{[{{if_then}, Label} | C1], Cut1};
well_form_goal({once, G}, Tail, Cut, Label) ->
	{G1, _} = well_form_goal(G, [{{cut}, Label, true} | Tail], true, Label),
	{[{{once}, Label} | G1], Cut};
well_form_goal({V}, Tail, Cut, _Label) ->
	{[{call, {V}} | Tail], Cut};
well_form_goal(true, Tail, Cut, _Label) -> {Tail, Cut}; %No-op
well_form_goal(fail, _Tail, _Cut, _Label) -> {[fail], false};  %No further
well_form_goal('!', Tail, Cut, Label) ->
	{[{{cut}, Label, not Cut} | Tail], true};
well_form_goal(Goal, Tail, Cut, _Label) ->
	ec_support:functor(Goal),        %Check goal
	{[Goal | Tail], Cut}.

%% initial_goal(Goal) -> {Goal,Bindings,NewVarNum}.
%% initial_goal(Goal, Bindings, VarNum) -> {Goal,NewBindings,NewVarNum}.
%% Check term for well-formedness as an Erlog term and replace '_'
%% variables with unique numbered variables. Error on non-well-formed
%% goals.
initial_goal(Goal) -> initial_goal(Goal, ec_support:new_bindings(), 0).

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