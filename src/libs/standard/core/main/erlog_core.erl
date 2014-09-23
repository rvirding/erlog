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

-module(erlog_core).

-include("erlog_core.hrl").
-include("erlog_bips.hrl").
-include("erlog_db.hrl").
-include("erlog_dcg.hrl").
-include("erlog_lists.hrl").
-include("erlog_time.hrl").

-behaviour(erlog_stdlib).

%% Main execution functions.
-export([prove_goal/1]).
%% Adding to database.
-export([load/1]).

%% built_in_db(Db) -> Database.
%% Create an initial clause database containing the built-in
%% predicates and predefined library predicates.
load(Db) ->
  lists:foreach(fun(Head) ->
    erlog_memory:load_kernel_space(Db, ?MODULE, Head) end, ?ERLOG_CORE). %% Add the Erlang built-ins.

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
  case ec_logic:check_goal(G, Next0, Bs, Db, false, Label) of
    {Next1, true} ->
      %% Must increment Vn to avoid clashes!!!
      Cut = #cut{label = Label},
      ec_core:prove_body(Param#param{goal = Next1, choice = [Cut | Cps], var_num = Vn + 1});
    {Next1, false} -> ec_core:prove_body(Param#param{goal = Next1, var_num = Vn + 1})
  end;
prove_goal(Params = #param{goal = fail}) -> erlog_errors:fail(Params);
prove_goal(Param = #param{goal = {'\\+', G}, next_goal = Next0, choice = Cps, bindings = Bs, var_num = Vn, database = Db}) ->
  %% We effectively implementing \+ G with ( G -> fail ; true ).
  Label = Vn,
  {Next1, _} = ec_logic:check_goal(G, [{{cut}, Label, true}, fail], Bs, Db, true, Label),
  Cp = #cp{type = if_then_else, label = Label, next = Next0, bs = Bs, vn = Vn},
  %%io:fwrite("PG(\\+): ~p\n", [{G1,[Cp|Cps]]),
  %% Must increment Vn to avoid clashes!!!
  ec_core:prove_body(Param#param{goal = Next1, choice = [Cp | Cps], var_num = Vn + 1});
prove_goal(Param = #param{goal = repeat, next_goal = Next, choice = Cps, bindings = Bs, var_num = Vn}) ->
  Cp = #cp{type = disjunction, next = [repeat | Next], bs = Bs, vn = Vn},
  ec_core:prove_body(Param#param{goal = Next, choice = [Cp | Cps]});
%% Clause creation and destruction.
prove_goal(Param = #param{goal = {abolish, Pi0}, next_goal = Next, bindings = Bs, database = Db}) ->
  case ec_support:dderef(Pi0, Bs) of
    {'/', N, A} when is_atom(N), is_integer(A), A > 0 ->
      erlog_memory:abolish_clauses(Db, {N, A}),
      ec_core:prove_body(Param#param{goal = Next});
    Pi -> erlog_errors:type_error(predicate_indicator, Pi, Db)
  end;
prove_goal(Param = #param{goal = {Assert, C0}, next_goal = Next, bindings = Bs, database = Db})
  when Assert == assert; Assert == assertz ->
  C = ec_support:dderef(C0, Bs),
  erlog_memory:assertz_clause(Db, C),
  ec_core:prove_body(Param#param{goal = Next});
prove_goal(Param = #param{goal = {asserta, C0}, next_goal = Next, bindings = Bs, database = Db}) ->
  C = ec_support:dderef(C0, Bs),
  erlog_memory:asserta_clause(Db, C),
  ec_core:prove_body(Param#param{goal = Next});
prove_goal(Param = #param{goal = {retract, C0}, bindings = Bs}) ->
  C = ec_support:dderef(C0, Bs),
  ec_logic:prove_retract(C, Param);
prove_goal(Param = #param{goal = {retractall, C0}, bindings = Bs}) ->
  C = ec_support:dderef(C0, Bs),
  ec_logic:prove_retractall(C, Param);
%% Clause retrieval and information
prove_goal(Param = #param{goal = {clause, H0, B}, bindings = Bs}) ->
  H1 = ec_support:dderef(H0, Bs),
  ec_logic:prove_clause(H1, B, Param);
prove_goal(Param = #param{goal = {current_predicate, Pi0}, bindings = Bs}) ->
  Pi = ec_support:dderef(Pi0, Bs),
  ec_logic:prove_current_predicate(Pi, Param);
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
  ec_logic:prove_ecall(Efun, Val, Param);
%% Non-standard but useful.
prove_goal(Param = #param{goal = {writeln, T}, next_goal = Next, bindings = Bs, event_man = Evman}) ->
  %% Display procedure.
  Res = ec_support:write(T, Bs),
  gen_event:notify(Evman, Res),
  ec_core:prove_body(Param#param{goal = Next});
%% File utils
prove_goal(Param = #param{goal = {consult, Name}, next_goal = Next, bindings = Bs, f_consulter = Fcon, database = Db}) ->
  case erlog_file:consult(Fcon, ec_support:dderef(Name, Bs), Db) of
    ok -> ok;
    {Err, Error} when Err == erlog_error; Err == error ->
      erlog_errors:erlog_error(Error, Db)
  end,
  ec_core:prove_body(Param#param{goal = Next});
prove_goal(Param = #param{goal = {reconsult, Name}, next_goal = Next, f_consulter = Fcon, database = Db}) ->
  case erlog_file:reconsult(Fcon, Name, Db) of
    ok -> ok;
    {Err, Error} when Err == erlog_error; Err == error ->
      erlog_errors:erlog_error(Error, Db)
  end,
  ec_core:prove_body(Param#param{goal = Next});
prove_goal(Param = #param{goal = {use, Library}, next_goal = Next, database = Db}) ->
  try Library:load(Db)
  catch
    _:Error ->
      erlog_errors:erlog_error(Error, Db)
  end,
  ec_core:prove_body(Param#param{goal = Next});
prove_goal(Param = #param{goal = {listing, Res}, next_goal = Next, bindings = Bs0, database = Db}) ->
  Content = erlog_memory:listing(Db, []),
  Bs = ec_support:add_binding(Res, Content, Bs0),
  ec_core:prove_body(Param#param{goal = Next, bindings = Bs});
prove_goal(Param = #param{goal = {listing, Pred, Res}, next_goal = Next, bindings = Bs0, database = Db}) ->
  Content = erlog_memory:listing(Db, [Pred]),
  Bs = ec_support:add_binding(Res, Content, Bs0),
  ec_core:prove_body(Param#param{goal = Next, bindings = Bs});
prove_goal(Param = #param{goal = {listing, Pred, Arity, Res}, next_goal = Next, bindings = Bs0, database = Db}) ->
  Content = erlog_memory:listing(Db, [Pred, Arity]),
  Bs = ec_support:add_binding(Res, Content, Bs0),
  ec_core:prove_body(Param#param{goal = Next, bindings = Bs});
prove_goal(Param = #param{goal = {findall, T, G, B}}) ->  %findall start
  ec_logic:prove_findall(T, G, B, Param);
prove_goal(Param = #param{goal = {findall, Tag, T0}, bindings = Bs, database = Db}) ->  %findall finish
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
  ec_core:prove_body(Param#param{goal = Next, bindings = UBs#cp.bs, choice = Choises, var_num = Vn + length(Choises)});
prove_goal(Param = #param{goal = {to_integer, NumV, Res}, next_goal = Next, bindings = Bs0}) ->
  Num = ec_support:dderef(NumV, Bs0),
  case catch (ec_logic:parse_int(Num)) of
    Int when is_integer(Int) ->
      Bs = ec_support:add_binding(Res, Int, Bs0),
      ec_core:prove_body(Param#param{goal = Next, bindings = Bs});
    _ -> erlog_errors:fail(Param)
  end;
prove_goal(Param = #param{goal = {to_string, VarV, Res}, next_goal = Next, bindings = Bs0}) ->
  Var = ec_support:dderef(VarV, Bs0),
  Bs = ec_support:add_binding(Res, ec_logic:to_string(Var), Bs0),
  ec_core:prove_body(Param#param{goal = Next, bindings = Bs}).
