%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. июн 2014 21:48
%%%-------------------------------------------------------------------
-module(erlog_memory).
-author("tihon").

-include("erlog.hrl").
-include("erlog_core.hrl").

%% API
-export([
  load_native_library/2,
  load_extended_library/2,
  load_extended_library/3,
  assertz_clause/3,
  asserta_clause/3,
  retract_clause/3,
  abolish_clauses/2,
  get_procedure/2,
  get_procedure_type/2,
  get_interp_functors/1,
  assertz_clause/2,
  asserta_clause/2,
  finadll/2,
  raw_store/3,
  raw_fetch/2,
  raw_append/3,
  raw_erase/2,
  listing/2,
  next/2,
  close/2]).

-export([
  db_assertz_clause/3,
  db_assertz_clause/4,
  db_asserta_clause/4,
  db_asserta_clause/3,
  db_retract_clause/4,
  db_abolish_clauses/3,
  get_db_procedure/3,
  db_findall/3,
  db_listing/3,
  db_next/3]).

-export([load_kernel_space/3]).


%%%===================================================================
%%% API
%%%===================================================================
%% kernelspace predicate loading
load_kernel_space(DBState = #db_state{stdlib = StdLib}, Module, Functor) ->
  UStdlib = dict:store(Functor, {built_in, Module}, StdLib),
  DBState#db_state{stdlib = UStdlib}.

%% libraryspace predicate loading
load_native_library(DBState = #db_state{stdlib = StdLib, exlib = ExLib}, {Functor, M, F}) ->
  check_immutable(StdLib, Functor),
  DBState#db_state{exlib = dict:store(Functor, {code, {M, F}}, ExLib)}.

%% add prolog functor to libraryspace
load_extended_library(DBState, {':-', Head, Body}) -> load_extended_library(DBState, Head, Body);
load_extended_library(DBState, Head) -> load_extended_library(DBState, Head, true).
load_extended_library(DBState = #db_state{stdlib = StdLib, exlib = ExLib}, Head, Body) ->
  check_immutable(StdLib, erlog_ec_support:functor(Head)),
  {Res, UExLib} = erlog_dict:assertz_clause({StdLib, ExLib, ExLib}, {Head, Body}), %use erlog_dict module to assert library to exlib dict
  {Res, DBState#db_state{exlib = UExLib}}.

%% userspace predicate loading
assertz_clause(Database, {':-', Head, Body}) -> assertz_clause(Database, Head, Body);
assertz_clause(Database, Head) -> assertz_clause(Database, Head, true).
assertz_clause(DBState, Head, Body) ->
  F = erlog_ec_support:functor(Head),
  do_action(DBState, assertz_clause, F, {Head, Body}).

asserta_clause(Database, {':-', H, B}) -> asserta_clause(Database, H, B);
asserta_clause(Database, H) -> asserta_clause(Database, H, true).
asserta_clause(DBState, Head, Body) ->
  F = erlog_ec_support:functor(Head),
  do_action(DBState, asserta_clause, F, {Head, Body}).

db_assertz_clause(Database, Collection, {':-', Head, Body}) -> db_assertz_clause(Database, Collection, Head, Body);
db_assertz_clause(Database, Collection, Head) -> db_assertz_clause(Database, Collection, Head, true).
db_assertz_clause(DBState, Collection, Head, Body) ->
  F = erlog_ec_support:functor(Head),
  do_action(DBState, db_assertz_clause, F, {Collection, Head, Body}).

db_asserta_clause(Database, Collection, {':-', H, B}) -> db_asserta_clause(Database, Collection, H, B);
db_asserta_clause(Database, Collection, H) -> db_asserta_clause(Database, Collection, H, true).
db_asserta_clause(DBState, Collection, Head, Body) ->
  F = erlog_ec_support:functor(Head),
  do_action(DBState, db_asserta_clause, F, {Collection, Head, Body}).

next(DBState, Cursor) ->
  do_next(DBState, next, Cursor).
db_next(DBState, Cursor, Table) ->
  do_next(DBState, db_next, {Cursor, Table}).

retract_clause(DBState, F, Ct) ->
  do_action(DBState, retract_clause, F, {F, Ct}).

db_retract_clause(DBState, Collection, F, Ct) ->
  do_action(DBState, db_retract_clause, F, {Collection, F, Ct}).

abolish_clauses(DBState = #db_state{stdlib = StdLib}, Func) ->
  check_immutable(StdLib, Func),
  check_abolish(abolish_clauses, Func, Func, DBState).

db_abolish_clauses(DBState = #db_state{stdlib = StdLib}, Collection, Func) ->
  check_immutable(StdLib, Func),  %abolishing fact from default memory need to be checked
  check_abolish(db_abolish_clauses, Func, {Collection, Func}, DBState).

get_procedure(DbState, Func) ->
  do_action(DbState, get_procedure, Func).

get_db_procedure(DbState, Collection, Func) ->
  do_action(DbState, get_db_procedure, {Collection, Func}).

get_procedure_type(DbState, Func) ->
  do_action(DbState, get_procedure_type, Func).

get_interp_functors(DbState) ->
  do_action(DbState, get_interp_functors).

db_findall(DbState, Collection, Fun) ->
  do_action(DbState, db_findall, {Collection, Fun}).

finadll(DbState, Fun) ->
  do_action(DbState, findall, Fun).

listing(DbState, Args) ->
  do_action(DbState, listing, Args).

db_listing(DbState, Collection, Args) ->
  do_action(DbState, db_listing, {Collection, Args}).

raw_store(DBState = #db_state{in_mem = InMem}, Key, Value) ->
  Umem = store(Key, Value, InMem),
  DBState#db_state{in_mem = Umem}.

raw_fetch(#db_state{in_mem = InMem}, Key) ->
  fetch(Key, InMem).

raw_append(DBState = #db_state{in_mem = InMem}, Key, Value) ->
  Value = fetch(Key, InMem),
  Umem = store(Key, lists:concat([Value, [Value]]), InMem),
  DBState#db_state{in_mem = Umem}.

raw_erase(DBState = #db_state{in_mem = InMem}, Key) ->
  Umem = dict:erase(Key, InMem),
  DBState#db_state{in_mem = Umem}.

close(DBState = #db_state{state = State, database = Db}, Cursor) ->
  {Res, UState} = Db:close(State, Cursor),
  {Res, DBState#db_state{state = UState}}.


%% @private
do_action(DBState = #db_state{stdlib = StdLib, exlib = ExLib, database = Db, state = State}, Fun, F, Args) ->
  check_immutable(StdLib, F),  %modifying fact in default memory need to be checked
  check_immutable(ExLib, F),
  {Res, UState} = Db:Fun({StdLib, ExLib, State}, Args),
  {Res, DBState#db_state{state = UState}}.

%% @private
do_action(DBState = #db_state{stdlib = StdLib, exlib = ExLib, database = Db, state = State}, Fun, Args) ->
  {Res, UState} = Db:Fun({StdLib, ExLib, State}, Args),
  {Res, DBState#db_state{state = UState}}.

%% @private
do_action(DBState = #db_state{stdlib = StdLib, exlib = ExLib, database = Db, state = State}, Fun) ->
  {Res, UState} = Db:Fun({StdLib, ExLib, State}),
  {Res, DBState#db_state{state = UState}}.

%% @private
do_next(DBState = #db_state{database = Db, state = State}, Fun, Cursor) ->
  {Res, UState} = Db:Fun(State, Cursor),
  Ans = case Res of
          {cursor, After, result, Result} -> {After, Result}; %got new (or same cursor) and result. Form and return
          [] -> {Cursor, []}  %no result got - return old cursor and empty result
        end,
  {Ans, DBState#db_state{state = UState}}.

%% @private
fetch(Key, Memory) ->
  case dict:find(Key, Memory) of
    error -> [];
    {ok, Value} -> Value
  end.

%% @private
store(Key, Value, Memory) ->
  dict:store(Key, Value, Memory).

%% @private
check_abolish(F, Func, Params, State = #db_state{state = DbState, database = Db, stdlib = StdLib, exlib = ExLib}) ->
  case dict:erase(Func, ExLib) of
    ExLib ->  %dict not changed - was not deleted. Search userspace
      {_, UState} = Db:F({StdLib, ExLib, DbState}, Params),
      State#db_state{state = UState};
    UExlib -> %dict changed -> was deleted
      State#db_state{exlib = UExlib}
  end.

%% @private
check_immutable(Dict, Functor) ->
  case dict:is_key(Functor, Dict) of
    false -> ok;
    true ->
      erlog_errors:permission_error(modify, static_procedure, erlog_ec_support:pred_ind(Functor)) %TODO will crash db process, but not erlog
  end.