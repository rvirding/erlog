%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Июль 2014 16:09
%%%-------------------------------------------------------------------
-module(erlog_ec_support).
-author("tihon").

-include("erlog_core.hrl").

%% API
-export([new_bindings/0, get_binding/2, add_binding/3,
  functor/1, cut/3, collect_alternatives/3,
  update_result/4, update_vars/4, deref/2, dderef_list/2,
  make_vars/2, pred_ind/1, deref_list/2, dderef/2, index_of/2, index_of/3, write/2, is_bound/1, try_add/3, check_var/2, get_vars/2]).

%% deref(Term, Bindings) -> Term.
%% Dereference a variable, else just return the term.
deref({V} = T0, Bs) ->
  case ?BIND:find(V, Bs) of
    {ok, T1} -> deref(T1, Bs);
    error -> T0
  end;
deref(T, _) -> T.        %Not a variable, return it.

%% deref_list(List, Bindings) -> List.
%%  Dereference the top-level checking that it is a list.
deref_list([], _) -> [];      %It already is a list
deref_list([_ | _] = L, _) -> L;
deref_list({V}, Bs) ->
  case dict:find(V, Bs) of
    {ok, L} -> deref_list(L, Bs);
    error -> erlog_errors:instantiation_error()
  end;
deref_list(Other, _) -> erlog_errors:type_error(list, Other).

%% dderef(Term, Bindings) -> Term.
%% Do a deep dereference. Completely dereference all the variables
%% occuring in a term, even those occuring in a variables value.
dderef(A, _) when ?IS_CONSTANT(A) -> A;
dderef([], _) -> [];
dderef([H0 | T0], Bs) ->
  [dderef(H0, Bs) | dderef(T0, Bs)];
dderef({V} = Var, Bs) ->
  case ?BIND:find(V, Bs) of
    {ok, T} -> dderef(T, Bs);
    error -> Var
  end;
dderef(T, Bs) when is_tuple(T) ->
  Es0 = tuple_to_list(T),
  Es1 = dderef(Es0, Bs),
  list_to_tuple(Es1).

%% dderef_list(List, Bindings) -> List.
%%  Dereference all variables to any depth but check that the
%%  top-level is a list.
dderef_list([], _Bs) -> [];
dderef_list([H | T], Bs) ->
  [dderef(H, Bs) | dderef_list(T, Bs)];
dderef_list({V}, Bs) ->
  case ?BIND:find(V, Bs) of
    {ok, L} -> dderef_list(L, Bs);
    error -> erlog_errors:instantiation_error()
  end;
dderef_list(Other, _Bs) -> erlog_errors:type_error(list, Other).

%% detects, whether variable is bound or not
-spec is_bound(term()) -> boolean().
is_bound({N}) when is_integer(N) -> false;
is_bound(_) -> true.

%% takes unbound vars from goal and return it
-spec get_vars(tuple(), dict:dict()) -> list().
get_vars(Goal, Bs) ->
  Bound = dderef(Goal, Bs),
  lists:flatten(get_var(Bound, [])).

%% make_vars(Count, VarNum) -> [Var].
%% Make a list of new variables starting at VarNum.
make_vars(0, _) -> [];
make_vars(I, Vn) ->
  [{Vn} | make_vars(I - 1, Vn + 1)].

%% functor(Goal) -> {Name,Arity}.
functor(T) when ?IS_FUNCTOR(T) ->
  {element(1, T), tuple_size(T) - 1};
functor(T) when is_atom(T) -> {T, 0};
functor(T) -> erlog_errors:type_error(callable, T).

%% Checks - if var is normal, or binded, or < 0 (if int). Returns var's value.
check_var({'-', Var}, Bs) ->
  case check_var(Var, Bs) of
    Res when is_number(Res) -> -1 * Res;
    Res -> Res
  end;
check_var({Var}, Bs) -> check_var(erlog_ec_support:deref({Var}, Bs), Bs);
check_var(Var, _) -> Var.

pred_ind({N, A}) -> {'/', N, A}.

%% pred_ind(N, A) -> {'/',N,A}.

%% Bindings
%% Bindings are kept in a dict where the key is the variable name.
new_bindings() -> ?BIND:new().

%% Add bindings if acc is not bound. If it is bound - match result.
try_add(Var, Res, Bs) ->
  Value = deref(Res, Bs),
  case is_bound(Value) of
    false -> add_binding(Res, Var, Bs); %not bound - just add var
    true ->
      case Value of %is bound. Fail if not the same
        Var -> Bs;
        _ -> error
      end
  end.

add_binding({V}, Val, Bs0) ->
  ?BIND:store(V, Val, Bs0).

get_binding({V}, Bs) ->
  ?BIND:find(V, Bs).

collect_alternatives(Goal, FunList, Predicates) ->
  Element = index_of(Goal, FunList) - 1,
  lists:foldr(
    fun({_, Pred, _}, Dict) ->
      [_ | ParamList] = tuple_to_list(Pred),
      Keys = remove_nth(ParamList, Element),
      dict:append(Keys, lists:nth(Element, ParamList), Dict)
    end, dict:new(), Predicates).

update_result(Key, ResultDict, Res, Bs0) ->
  case dict:find(Key, ResultDict) of
    {ok, Value} -> add_binding(Res, Value, Bs0);
    error -> Bs0
  end.

update_vars(Goal, FunList, Key, Bs) ->
  Vars = tl(FunList) -- [Goal],
  lists:foldl(
    fun({N} = Var, UBs1) ->
      add_binding(Var, lists:nth(N, Key), UBs1)
    end, Bs, Vars).

index_of(Item, List) -> index_of(Item, List, 1).

index_of(_, [], _) -> not_found;
index_of(Item, [Item | _], Index) -> Index;
index_of(Item, [_ | Tl], Index) -> index_of(Item, Tl, Index + 1).

write(Res, Bs) when is_list(Res) ->
  case io_lib:printable_list(Res) of
    true -> Res;
    false -> erlog_ec_support:dderef(Res, Bs)
  end;
write(Res, Bs) ->
  write([Res], Bs).

cut(Label, Last, Param = #param{next_goal = Next, choice = [#cut{label = Label} | Cps] = Cps0}) ->
  if Last -> erlog_ec_core:prove_body(Param#param{goal = Next, choice = Cps});
    true -> erlog_ec_core:prove_body(Param#param{goal = Next, choice = Cps0})
  end;
cut(Label, Last, Param = #param{next_goal = Next, choice = [#cp{type = if_then_else, label = Label} | Cps] = Cps0}) ->
  if Last -> erlog_ec_core:prove_body(Param#param{goal = Next, choice = Cps});
    true -> erlog_ec_core:prove_body(Param#param{goal = Next, choice = Cps0})
  end;
cut(Label, Last, Param = #param{choice = [#cp{type = goal_clauses, label = Label} = Cp | Cps]}) ->
  cut_goal_clauses(Last, Cp, Param#param{choice = Cps});
cut(Label, Last, Param = #param{choice = [_Cp | Cps]}) ->
  cut(Label, Last, Param#param{choice = Cps}).


%% @private
remove_nth(List, N) ->
  {A, B} = lists:split(N - 1, List),
  A ++ tl(B).

%% @private
%% cut_goal_clauses(Last, Next, Cp, Cps, Bs, Vn, Db).
cut_goal_clauses(true, #cp{label = _}, Param = #param{next_goal = Next}) ->
  %% Just remove the choice point completely and continue.
  erlog_ec_core:prove_body(Param#param{goal = Next});
cut_goal_clauses(false, #cp{label = L}, Param = #param{next_goal = Next, choice = Cps}) ->
  %% Replace choice point with cut point then continue.
  Cut = #cut{label = L},
  erlog_ec_core:prove_body(Param#param{goal = Next, choice = [Cut | Cps]}).

%% @private
%% Get unbound vars from goal
get_var(N = {I}, Acc) when is_integer(I)-> [N | Acc];
get_var(Tuple, Acc) when is_tuple(Tuple) -> get_var(tuple_to_list(Tuple), Acc);
get_var([First | Rest], Acc) ->
  FirstPrep = get_var(First, []),
  get_var(Rest, [FirstPrep | Acc]);
get_var(_, Acc) -> Acc.