%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Июль 2014 16:26
%%%-------------------------------------------------------------------
-module(ec_unify).
-author("tihon").

-include("erlog_core.hrl").

%% API
-export([unify/3, unify_clauses/4, unify_head/4, unify_clause/5]).

%% unify(Term, Term, Bindings) -> {succeed,NewBindings} | fail.
%% Unify two terms with a set of bindings.
unify(T10, T20, Bs0) ->
  case {ec_support:deref(T10, Bs0), ec_support:deref(T20, Bs0)} of
    {T1, T2} when ?IS_CONSTANT(T1), T1 == T2 ->
      {succeed, Bs0};
    {{V}, {V}} -> {succeed, Bs0};
    {{_} = Var, T2} -> {succeed, ec_support:add_binding(Var, T2, Bs0)};
    {T1, {_} = Var} -> {succeed, ec_support:add_binding(Var, T1, Bs0)};
    {[H1 | T1], [H2 | T2]} ->
      case unify(H1, H2, Bs0) of
        {succeed, Bs1} -> unify(T1, T2, Bs1);
        fail -> fail
      end;
    {[], []} -> {succeed, Bs0};
    {T1, T2} when tuple_size(T1) == tuple_size(T2),
      element(1, T1) == element(1, T2) ->
      unify_args(T1, T2, Bs0, 2, tuple_size(T1));
    _Other -> fail
  end.

%% Try to unify Head and Body using Clauses which all have the same functor.
unify_clauses(_Ch, _Cb, [], Param) -> erlog_errors:fail(Param);  %no more clauses to try
unify_clauses(Ch, Cb, C, Param = #param{next_goal = Next, bindings = Bs0, var_num = Vn0, choice = Cps, database = Db, cursor = Cursor}) ->
  case unify_clause(Ch, Cb, C, Bs0, Vn0) of
    {succeed, Bs1, Vn1} ->
      Cp = #cp{type = clause, data = {Ch, Cb, Db, Cursor}, next = Next, bs = Bs0, vn = Vn0},
      ec_core:prove_body(Param#param{goal = Next, choice = [Cp | Cps], bindings = Bs1, var_num = Vn1});
    fail -> unify_clauses(Ch, Cb, erlog_memory:next(Db, Cursor), Param)
  end.

unify_clause(Ch, Cb, [C], Bs0, Vn0) -> unify_clause(Ch, Cb, C, Bs0, Vn0);
unify_clause(Ch, Cb, {_Tag, H0, {B0, _}}, Bs0, Vn0) ->
  {H1, Rs1, Vn1} = ec_term:term_instance(H0, Vn0),  %Unique vars on head first
  case unify(Ch, H1, Bs0) of
    {succeed, Bs1} ->
      {B1, _Rs2, Vn2} = ec_body:body_term(B0, Rs1, Vn1),  %Now we need the rest
      case unify(Cb, B1, Bs1) of
        {succeed, Bs2} -> {succeed, Bs2, Vn2};
        fail -> fail
      end;
    fail -> fail
  end.

%% unify_head(Goal, Head, Bindings, VarNum) ->
%%      {succeed,Repls,NewBindings,NewVarNum} | fail
%%  Unify a goal with a head without creating an instance of the
%%  head. This saves us creating many variables which are local to the
%%  clause and saves many variable bindings.
unify_head(Goal, Head, Bs, Vn) ->
  unify_head(ec_support:deref(Goal, Bs), Head, orddict:new(), Bs, Vn).

unify_head(G, H, Rs, Bs, Vn) when ?IS_CONSTANT(G), G == H ->
  {succeed, Rs, Bs, Vn};
unify_head(_T, {'_'}, Rs, Bs, Vn) -> {succeed, Rs, Bs, Vn};
unify_head(T, {V0}, Rs, Bs0, Vn) ->
  %% Now for the tricky bit!
  case orddict:find(V0, Rs) of
    {ok, V1} ->        %Already have a replacement
      case unify(T, V1, Bs0) of
        {succeed, Bs1} -> {succeed, Rs, Bs1, Vn};
        fail -> fail
      end;
    error ->        %Add a replacement
      {succeed, orddict:store(V0, T, Rs), Bs0, Vn}
  end;
unify_head({_} = Var, H0, Rs0, Bs, Vn0) ->
  %% Must have an instance here.
  {H1, Rs1, Vn1} = ec_term:term_instance(H0, Rs0, Vn0),
  {succeed, Rs1, ec_support:add_binding(Var, H1, Bs), Vn1};
unify_head([GH | GT], [HH | HT], Rs0, Bs0, Vn0) ->
  case unify_head(ec_support:deref(GH, Bs0), HH, Rs0, Bs0, Vn0) of
    {succeed, Rs1, Bs1, Vn1} -> unify_head(ec_support:deref(GT, Bs1), HT, Rs1, Bs1, Vn1);
    fail -> fail
  end;
unify_head([], [], Rs, Bs, Vn) -> {succeed, Rs, Bs, Vn};
unify_head(G, H, Rs, Bs, Vn) when tuple_size(G) == tuple_size(H),
  element(1, G) == element(1, H) ->
  unify_head_args(G, H, Rs, Bs, Vn, 2, tuple_size(G));
unify_head(_G, _H, _Rs, _Bs, _Vn) -> fail.

unify_head_args(_G, _H, Rs, Bs, Vn, I, S) when I > S ->
  {succeed, Rs, Bs, Vn};
unify_head_args(G, H, Rs0, Bs0, Vn0, I, S) ->
  case unify_head(ec_support:deref(element(I, G), Bs0), element(I, H), Rs0, Bs0, Vn0) of
    {succeed, Rs1, Bs1, Vn1} -> unify_head_args(G, H, Rs1, Bs1, Vn1, I + 1, S);
    fail -> fail
  end.

unify_args(_, _, Bs, I, S) when I > S -> {succeed, Bs};
unify_args(S1, S2, Bs0, I, S) ->
  case unify(element(I, S1), element(I, S2), Bs0) of
    {succeed, Bs1} -> unify_args(S1, S2, Bs1, I + 1, S);
    fail -> fail
  end.