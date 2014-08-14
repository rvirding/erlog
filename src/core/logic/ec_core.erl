%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Авг. 2014 16:47
%%%-------------------------------------------------------------------
-module(ec_core).
-author("tihon").

-include("erlog_core.hrl").

%% API
-export([prove_body/1, prove_goal/1, prove_goal/4, prove_goal_clauses/3]).

%% prove_goal(Goal, Database) -> Succeed | Fail.
%% This is the main entry point into the interpreter. Check that
%% everything is consistent then prove the goal as a call.
-spec prove_goal(Goal0 :: term(), Db :: pid(), Fcon :: fun(), Event :: pid()) -> term().
prove_goal(Goal0, Db, Fcon, Event) ->
	%% put(erlog_cut, orddict:new()),
	%% put(erlog_cps, orddict:new()),
	%% put(erlog_var, orddict:new()),
	%% Check term and build new instance of term with bindings.
	{Goal1, Bs, Vn} = ec_logic:initial_goal(Goal0),
	Params = #param{goal = [{call, Goal1}], choice = [], bindings = Bs, var_num = Vn,
		event_man = Event, database = Db, f_consulter = Fcon},
	ec_core:prove_body(Params). %TODO use lists:foldr instead!

%% prove_body(Body, ChoicePoints, Bindings, VarNum, Database) ->
%%      {succeed,ChoicePoints,NewBindings,NewVarNum,NewDatabase}.
%% Prove the goals in a body. Remove the first goal and try to prove
%% it. Return when there are no more goals. This is how proving a
%% goal/body succeeds.
prove_body(Params = #param{goal = [G | Gs]}) -> %TODO use lists:foldr instead!
	%%io:fwrite("PB: ~p\n", [{G,Gs,Cps}]),
	prove_goal(Params#param{goal = G, next_goal = Gs});
prove_body(#param{goal = [], choice = Cps, bindings = Bs, var_num = Vn, database = Db}) ->
	%%io:fwrite("Cps: ~p\nCut: ~p\nVar: ~p\nVar: ~p\n",
	%%      [get(erlog_cps),get(erlog_cut),get(erlog_var),dict:size(Bs)]),
	%%io:fwrite("PB: ~p\n", [Cps]),
	{succeed, Cps, Bs, Vn, Db}.      %No more body  %TODO why should we return database?

%% Prove support first. Then find in database.
prove_goal(Param = #param{goal = {{once}, Label}, next_goal = Next, choice = Cps}) ->
	%% We effetively implement once(G) with ( G, ! ) but cuts in
	%% G are local to G.
	%% There is no ( G, ! ) here, it has already been prepended to Next.
	Cut = #cut{label = Label},
	prove_body(Param#param{goal = Next, choice = [Cut | Cps]});
prove_goal(Param = #param{goal = {{if_then_else}, Else, Label}, next_goal = Next, choice = Cps, bindings = Bs, var_num = Vn}) ->
	%% Need to push a choicepoint to fail back to inside Cond and a cut
	%% to cut back to before Then when Cond succeeds. #cp{type=if_then_else}
	%% functions as both as is always removed whatever the outcome.
	%% There is no ( C, !, T ) here, it has already been prepended to Next.
	Cp = #cp{type = if_then_else, label = Label, next = Else, bs = Bs, vn = Vn},
	%%io:fwrite("PG(->;): ~p\n", [{Next,Else,[Cp|Cps]}]),
	prove_body(Param#param{goal = Next, choice = [Cp | Cps]});
prove_goal(Param = #param{goal = {{if_then}, Label}, next_goal = Next, choice = Cps}) ->
	%% We effetively implement ( C -> T ) with ( C, !, T ) but cuts in
	%% C are local to C.
	%% There is no ( C, !, T ) here, it has already been prepended to Next.
	%%io:fwrite("PG(->): ~p\n", [{Next}]),
	Cut = #cut{label = Label},
	prove_body(Param#param{goal = Next, choice = [Cut | Cps]});
prove_goal(Param = #param{goal = {{cut}, Label, Last}}) ->
	%% Cut succeeds and trims back to cut ancestor.
	ec_support:cut(Label, Last, Param);
prove_goal(Param = #param{goal = {{disj}, R}, next_goal = Next, choice = Cps, bindings = Bs, var_num = Vn}) ->
	%% There is no L here, it has already been prepended to Next.
	Cp = #cp{type = disjunction, next = R, bs = Bs, vn = Vn},
	prove_body(Param#param{goal = Next, choice = [Cp | Cps]});
prove_goal(Param = #param{goal = G, database = Db}) ->
%% 	io:fwrite("PG: ~p\n    ~p\n    ~p\n", [dderef(G, Bs),Next,Cps]),
	case catch erlog_memory:get_procedure(Db, ec_support:functor(G)) of
		{built_in, Mod} -> Mod:prove_goal(Param); %kernel space
		{code, {Mod, Func}} -> Mod:Func(G, Param);
		{clauses, Cs} -> prove_goal_clauses(G, Cs, Param);
		undefined -> erlog_errors:fail(Param);
	%% Getting built_in here is an error!
		{erlog_error, E} -> erlog_errors:erlog_error(E, Db)  %Fill in more error data
	end.

%% prove_goal_clauses(Goal, Clauses, Next, ChoicePoints, Bindings, VarNum, Database) ->
%%      void.
%% Try to prove Goal using Clauses which all have the same functor.
prove_goal_clauses(G, [C], Params = #param{choice = Cps, var_num = Vn}) ->
	%% Must be smart here and test whether we need to add a cut point.
	%% C has the structure {Tag,Head,{Body,BodyHasCut}}.
	case element(2, element(3, C)) of
		true ->
			Cut = #cut{label = Vn},
			prove_goal_clause(G, C, Params#param{choice = [Cut | Cps]});
		false ->
			prove_goal_clause(G, C, Params)
	end;
%% prove_goal_clause(G, C, Next, Cps, Bs, Vn, Db);
prove_goal_clauses(G, [C | Cs], Params = #param{next_goal = Next, var_num = Vn, bindings = Bs, choice = Cps}) ->
	Cp = #cp{type = goal_clauses, label = Vn, data = {G, Cs}, next = Next, bs = Bs, vn = Vn},
	prove_goal_clause(G, C, Params#param{choice = [Cp | Cps]});
prove_goal_clauses(_G, [], Param) -> erlog_errors:fail(Param).

prove_goal_clause(G, {_Tag, H0, {B0, _}}, Param = #param{next_goal = Next, bindings = Bs0, var_num = Vn0}) ->
	%% io:fwrite("PGC1: ~p\n", [{G,H0,B0}]),
	Label = Vn0,
	case ec_unify:unify_head(G, H0, Bs0, Vn0 + 1) of
		{succeed, Rs0, Bs1, Vn1} ->
			%% io:fwrite("PGC2: ~p\n", [{Rs0}]),
			{B1, _Rs2, Vn2} = ec_body:body_instance(B0, Next, Rs0, Vn1, Label),
			%% io:fwrite("PGC3: ~p\n", [{B1,Next,Cps}]),
			ec_core:prove_body(Param#param{goal = B1, bindings = Bs1, var_num = Vn2});
		fail -> erlog_errors:fail(Param)
	end.