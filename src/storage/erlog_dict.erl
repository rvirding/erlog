%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. июн 2014 18:00
%%%-------------------------------------------------------------------

-module(erlog_dict).

-include("erlog_core.hrl").

-behaviour(erlog_storage).

%% erlog callbacks
-export([new/0, new/1,
  assertz_clause/2,
  asserta_clause/2,
  retract_clause/2,
  abolish_clauses/2,
  get_procedure/2,
  get_procedure_type/2,
  get_interp_functors/1,
  findall/2,
  listing/2,
  close/2,
  next/2]).

%% erlog db callbacks
-export([db_assertz_clause/2,
  db_asserta_clause/2,
  db_retract_clause/2,
  db_abolish_clauses/2,
  db_findall/2,
  get_db_procedure/2,
  db_listing/2]).

new() -> {ok, dict:new()}.

new(_) -> {ok, dict:new()}.

db_assertz_clause({StdLib, ExLib, Db}, {Collection, Head, Body0}) ->
  Dict = erlog_db_storage:get_db(dict, Collection),
  {Res, Udict} = assertz_clause({StdLib, ExLib, Dict}, {Head, Body0}),
  erlog_db_storage:update_db(Collection, Udict),
  {Res, Db}.

assertz_clause({_, _, Db} = Memory, {Head, Body0}) ->
  Udb = clause(Head, Body0, Memory,
    fun(Functor, Cs, Body) ->
      case check_duplicates(Cs, Head, Body) of
        true -> Db;  %found - do nothing
        _ -> dict:append(Functor, {length(Cs), Head, Body}, Db) %not found - insert new
      end
    end),
  {ok, Udb}.

db_asserta_clause({StdLib, ExLib, Db}, {Collection, Head, Body0}) ->
  Dict = erlog_db_storage:get_db(dict, Collection),
  {Res, Udict} = asserta_clause({StdLib, ExLib, Dict}, {Head, Body0}),
  erlog_db_storage:update_db(Collection, Udict),
  {Res, Db}.

asserta_clause({_, _, Db} = Memory, {Head, Body0}) ->
  Udb = clause(Head, Body0, Memory,
    fun(Functor, Cs, Body) ->
      case check_duplicates(Cs, Head, Body) of
        true -> Db;  %found - do nothing
        _ ->
          dict:update(Functor,
            fun(Old) ->
              [{length(Cs), Head, Body} | Old]
            end, [{length(Cs), Head, Body}], Db) %not found - insert new
      end
    end),
  {ok, Udb}.

db_retract_clause({StdLib, ExLib, Db}, {Collection, Functor, Ct}) ->
  Dict = erlog_db_storage:get_db(dict, Collection),
  {Res, Udict} = retract_clause({StdLib, ExLib, Dict}, {Functor, Ct}),
  erlog_db_storage:update_db(Collection, Udict),
  {Res, Db}.

retract_clause({StdLib, ExLib, Db}, {Functor, Ct}) ->
  ok = check_immutable(StdLib, Functor),
  ok = check_immutable(ExLib, Functor),
  Udb = case dict:is_key(Functor, Db) of
          true ->
            dict:update(Functor, fun(Old) -> lists:keydelete(Ct, 1, Old) end, [], Db);
          false -> Db        %Do nothing
        end,
  {ok, Udb}.

db_abolish_clauses({StdLib, ExLib, Db}, {Collection, Functor}) ->
  Dict = erlog_db_storage:get_db(dict, Collection),
  {Res, Udict} = abolish_clauses({StdLib, ExLib, Dict}, {Functor}),
  erlog_db_storage:update_db(Collection, Udict),
  {Res, Db}.

abolish_clauses({StdLib, _, Db}, Functor) ->
  ok = check_immutable(StdLib, Functor),
  Udb = case dict:is_key(Functor, Db) of
          true -> dict:erase(Functor, Db);
          false -> Db        %Do nothing
        end,
  {ok, Udb}.

db_findall({StdLib, ExLib, Db}, {Collection, Functor}) ->  %for db_call
  Dict = erlog_db_storage:get_db(dict, Collection),
  case dict:find(Functor, StdLib) of %search built-in first
    {ok, StFun} -> {StFun, Db};
    error ->
      case dict:find(Functor, ExLib) of  %search libraryspace then
        {ok, ExFun} -> {ExFun, Db};
        error ->
          case dict:find(Functor, Dict) of  %search userspace last
            {ok, Cs} ->
              {First, Cursor} = form_clauses(Cs), %TODO fix bagof, possibly broken by return format
              {{cursor, Cursor, result, {clauses, First}}, Db};
            error -> {[], Db}
          end
      end
  end.

findall({StdLib, ExLib, Db}, Functor) ->  %for bagof
  case dict:find(Functor, StdLib) of %search built-in first
    {ok, StFun} -> {StFun, Db};
    error ->
      case dict:find(Functor, ExLib) of  %search libraryspace then
        {ok, ExFun} -> {ExFun, Db};
        error ->
          case dict:find(Functor, Db) of  %search userspace last
            {ok, Cs} -> {Cs, Db};
            error -> {[], Db}
          end
      end
  end.

close(Db, _) -> {ok, Db}.

next(Db, undefined) -> {[], Db};
next(Db, Queue) ->
  case queue:out(Queue) of  %take variant
    {{value, Val}, UQ} ->
      {{cursor, UQ, result, Val}, Db};  %return it
    {empty, UQ} -> {{cursor, UQ, result, []}, Db}  %nothing to return
  end.

get_db_procedure({StdLib, ExLib, Db}, {Collection, Functor}) ->
  Dict = erlog_db_storage:get_db(dict, Collection),
  {Res, Udict} = get_procedure({StdLib, ExLib, Dict}, {Functor}),
  erlog_db_storage:update_db(Collection, Udict),
  {Res, Db}.

get_procedure({StdLib, ExLib, Db}, Functor) ->
  Res = case dict:find(Functor, StdLib) of %search built-in first
          {ok, StFun} -> StFun;
          error ->
            case dict:find(Functor, ExLib) of  %search libraryspace then
              {ok, ExFun} -> ExFun;
              error ->
                case dict:find(Functor, Db) of  %search userspace last
                  {ok, Cs} ->
                    {First, Cursor} = form_clauses(Cs),
                    {cursor, Cursor, result, {clauses, First}};
                  error -> undefined
                end
            end
        end,
  {Res, Db}.

get_procedure_type({StdLib, ExLib, Db}, Functor) ->
  Res = case dict:is_key(Functor, StdLib) of %search built-in first
          true -> built_in;
          false ->
            case dict:is_key(Functor, ExLib) of  %search libraryspace then
              true -> compiled;
              false ->
                case dict:is_key(Functor, Db) of  %search userspace last
                  true -> interpreted;
                  false -> undefined
                end
            end
        end,
  {Res, Db}.

get_interp_functors({_, ExLib, Db}) ->
  Library = dict:fetch_keys(ExLib),
  UserSpace = dict:fetch_keys(Db),
  {lists:concat([Library, UserSpace]), Db}.

db_listing({StdLib, ExLib, Db}, {Collection, Params}) ->
  Dict = erlog_db_storage:get_db(dict, Collection),
  {Res, Udict} = listing({StdLib, ExLib, Dict}, Params),
  erlog_db_storage:update_db(Collection, Udict),
  {Res, Db}.

listing({_, _, Db}, [Functor, Arity]) ->
  {dict:fold(
    fun({F, A} = Res, _, Acc) when F == Functor andalso A == Arity ->
      [Res | Acc];
      (_, _, Acc) -> Acc
    end, [], Db), Db};
listing({_, _, Db}, [Functor]) ->
  {dict:fold(
    fun({F, Arity}, _, Acc) when F == Functor ->
      [{Functor, Arity} | Acc];
      (_, _, Acc) -> Acc
    end, [], Db), Db};
listing({_, _, Db}, []) ->
  {dict:fetch_keys(Db), Db}.

%% @private
clause(Head, Body0, {StdLib, ExLib, Db}, ClauseFun) ->
  {Functor, Body} = case catch {ok, erlog_ec_support:functor(Head), erlog_ec_body:well_form_body(Body0, false, sture)} of
                      {erlog_error, E} -> erlog_errors:erlog_error(E, Db);
                      {ok, F, B} -> {F, B}
                    end,
  ok = check_immutable(StdLib, Functor),  %check built-in functions (read only) for clause
  ok = check_immutable(ExLib, Functor),   %check library functions (read only) for clauses
  case dict:find(Functor, Db) of
    {ok, Cs} -> ClauseFun(Functor, Cs, Body);
    error -> dict:append(Functor, {0, Head, Body}, Db)
  end.

%% @private
%% true - duplicate found
-spec check_duplicates(list(), tuple(), tuple()) -> boolean().
check_duplicates(Cs, Head, Body) ->
  catch (lists:foldl(
    fun({_, H, B}, _) when H == Head andalso B == Body -> throw(true);  %find same fact
      (_, Acc) -> Acc
    end, false, Cs)).

%% @private
check_immutable(Dict, Functor) ->
  case dict:is_key(Functor, Dict) of
    false -> ok;
    true -> erlog_errors:permission_error(modify, static_procedure, erlog_ec_support:pred_ind(Functor))
  end.

%% @private
form_clauses([]) -> {[], queue:new()};
form_clauses([First | Loaded]) ->
  Queue = queue:from_list(Loaded),
  {First, Queue}.