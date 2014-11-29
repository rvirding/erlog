%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Июль 2014 11:18
%%%-------------------------------------------------------------------
-module(erlog_db).
-author("tihon").

-include("erlog_core.hrl").
-include("erlog_db.hrl").

-behaviour(erlog_exlib).

%% API
-export([load/1,
  db_assert_2/1,
  db_asserta_2/1,
  db_abolish_2/1,
  db_retract_2/1,
  db_retractall_2/1,
  db_call_2/1,
  db_listing_2/1,
  db_listing_3/1,
  db_listing_4/1]).

load(Db) ->
  lists:foreach(fun(Proc) -> erlog_memory:load_native_library(Db, Proc) end, ?ERLOG_DB).

db_call_2(Param = #param{goal = {db_call, _, _} = Goal, next_goal = Next0, bindings = Bs, database = Db}) ->
  {db_call, Table, G} = erlog_ec_support:dderef(Goal, Bs),
  case erlog_memory:db_findall(Db, Table, G) of
    {cursor, Cursor, result, Result} ->
      Fun = fun(Params) -> erlog_db_logic:check_call_result(Result, Params, G, Table, Next0) end,
      erlog_ec_core:run_n_close(Fun, Param#param{cursor = Cursor});
    Result -> erlog_db_logic:check_call_result(Result, Param, G, Table, Next0)
  end.

db_assert_2(Params = #param{goal = {db_assert, _, _} = Goal, next_goal = Next, bindings = Bs, database = Db}) ->
  {db_assert, Table, Fact} = erlog_ec_support:dderef(Goal, Bs),
  erlog_memory:db_assertz_clause(Db, Table, Fact),
  erlog_ec_core:prove_body(Params#param{goal = Next}).

db_asserta_2(Params = #param{goal = {db_asserta, _, _} = Goal, next_goal = Next, bindings = Bs, database = Db}) ->
  {db_asserta, Table, Fact} = erlog_ec_support:dderef(Goal, Bs),
  erlog_memory:db_asserta_clause(Db, Table, Fact),
  erlog_ec_core:prove_body(Params#param{goal = Next}).

db_abolish_2(Params = #param{goal = {db_abolish, _, _} = Goal, next_goal = Next, bindings = Bs, database = Db}) ->
  {db_abolish, Table, Fact} = erlog_ec_support:dderef(Goal, Bs),
  case Fact of
    {'/', N, A} when is_atom(N), is_integer(A), A > 0 ->
      erlog_memory:db_abolish_clauses(Db, Table, {N, A}),
      erlog_ec_core:prove_body(Params#param{goal = Next});
    Pi -> erlog_errors:type_error(predicate_indicator, Pi, Db)
  end.

db_retract_2(Params = #param{goal = {db_retract, _, _} = Goal, bindings = Bs}) ->
  {db_retract, Table, Fact} = erlog_ec_support:dderef(Goal, Bs),
  erlog_db_logic:prove_retract(Fact, Table, Params).

db_retractall_2(Params = #param{goal = {db_retractall, _, _} = Goal, bindings = Bs}) ->
  {db_retractall, Table, Fact} = erlog_ec_support:dderef(Goal, Bs),
  erlog_db_logic:prove_retractall(Fact, Table, Params).

db_listing_2(Params = #param{goal = {db_listing, _, _} = Goal, next_goal = Next, bindings = Bs0, database = Db}) ->
  {db_listing, Table, Res} = erlog_ec_support:dderef(Goal, Bs0),
  Content = erlog_memory:db_listing(Db, Table, []),
  Bs = erlog_ec_support:add_binding(Res, Content, Bs0),
  erlog_ec_core:prove_body(Params#param{goal = Next, bindings = Bs}).

db_listing_3(Params = #param{goal = {db_listing, _, _, _} = Goal, next_goal = Next, bindings = Bs0, database = Db}) ->
  {db_listing, Table, Functor, Res} = erlog_ec_support:dderef(Goal, Bs0),
  Content = erlog_memory:db_listing(Db, Table, [Functor]),
  Bs = erlog_ec_support:add_binding(Res, Content, Bs0),
  erlog_ec_core:prove_body(Params#param{goal = Next, bindings = Bs}).

db_listing_4(Params = #param{goal = {db_listing, _, _, _, _} = Goal, next_goal = Next, bindings = Bs0, database = Db}) ->
  {db_listing, Table, Functor, Arity, Res} = erlog_ec_support:dderef(Goal, Bs0),
  Content = erlog_memory:db_listing(Db, Table, [Functor, Arity]),
  Bs = erlog_ec_support:add_binding(Res, Content, Bs0),
  erlog_ec_core:prove_body(Params#param{goal = Next, bindings = Bs}).