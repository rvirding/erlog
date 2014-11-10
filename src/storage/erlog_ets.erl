%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. июн 2014 18:00
%%%-------------------------------------------------------------------

-module(erlog_ets).

-include("erlog_core.hrl").

-behaviour(erlog_storage).

%% erlog callbacks
-export([new/1,
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
  db_listing/2,
  db_next/2]).

new(_) -> {ok, ets:new(eets, [bag, private])}.

db_assertz_clause({StdLib, ExLib, Db}, {Collection, Head, Body0}) ->
  Ets = erlog_db_storage:get_db(ets, Collection),
  {Res, _} = assertz_clause({StdLib, ExLib, Ets}, {Head, Body0}),
  {Res, Db}.

assertz_clause({_, _, Db} = Memory, {Head, Body0}) ->
  clause(Head, Body0, Memory,
    fun(Functor, Cs, Body) ->
      case check_duplicates(Cs, Head, Body) of
        false -> ok;  %found - do nothing
        _ -> ets:insert(Db, {Functor, {length(Cs), Head, Body}}) %not found - insert new
      end
    end),
  {ok, Db}.

db_asserta_clause({StdLib, ExLib, Db}, {Collection, Head, Body0}) ->
  Ets = erlog_db_storage:get_db(ets, Collection),
  {Res, _} = asserta_clause({StdLib, ExLib, Ets}, {Head, Body0}),
  {Res, Db}.

asserta_clause({_, _, Db} = Memory, {Head, Body0}) ->
  clause(Head, Body0, Memory,
    fun(Functor, Cs, Body) ->
      case check_duplicates(Cs, Head, Body) of
        false -> ok;  %found - do nothing
        _ ->
          Clauses = [{Functor, {length(Cs), Head, Body}} | Cs],
          ets:delete(Db, Functor),
          ets:insert(Db, [Clauses])
      end
    end),
  {ok, Db}.

db_retract_clause({StdLib, ExLib, Db}, {Collection, Functor, Ct}) ->
  Ets = erlog_db_storage:get_db(ets, Collection),
  {Res, _} = retract_clause({StdLib, ExLib, Ets}, {Functor, Ct}),
  {Res, Db}.

retract_clause({_, _, Db}, {Functor, Ct}) ->
  case catch ets:lookup_element(Db, Functor, 2) of
    Cs when is_list(Cs) ->
      Object = lists:keyfind(Ct, 1, Cs),
      ets:delete_object(Db, Object);
    _ -> ok
  end,
  {ok, Db}.

db_abolish_clauses({StdLib, ExLib, Db}, {Collection, Functor}) ->
  Ets = erlog_db_storage:get_db(ets, Collection),
  {Res, _} = abolish_clauses({StdLib, ExLib, Ets}, Functor),
  {Res, Db}.

abolish_clauses({_, _, Db}, Functor) ->
  ets:delete(Db, Functor),
  {ok, Db}.

db_findall({StdLib, ExLib, Db}, {Collection, Goal}) ->  %for db_call
  Functor = erlog_ec_support:functor(Goal),
  Ets = erlog_db_storage:get_db(ets, Collection),
  case dict:find(Functor, StdLib) of %search built-in first
    {ok, StFun} -> {StFun, Db};
    error ->
      case dict:find(Functor, ExLib) of  %search libraryspace then
        {ok, ExFun} -> {ExFun, Db};
        error ->
          CS = case catch ets:lookup_element(Ets, Functor, 2) of  %search userspace last
                 Cs when is_list(Cs) -> Cs;
                 _ -> []
               end,
          {work_with_clauses(CS), Db}
      end
  end.

findall({StdLib, ExLib, Db}, Goal) ->
  Functor = erlog_ec_support:functor(Goal),
  case dict:find(Functor, StdLib) of %search built-in first
    {ok, StFun} -> {StFun, Db};
    error ->
      case dict:find(Functor, ExLib) of  %search libraryspace then
        {ok, ExFun} -> {ExFun, Db};
        error ->
          CS = case catch ets:lookup_element(Db, Functor, 2) of  %search userspace last
                 Cs when is_list(Cs) -> Cs;
                 _ -> []
               end,
          {CS, Db}
      end
  end.

close(Ets, undefined) -> {ok, Ets};
close(Ets, Cursor) ->
  put(Cursor, queue:new()), %save empty queue
  {ok, Ets}.

next(Ets, undefined) -> {[], Ets};
next(Ets, Queue) ->
  case queue:out(Queue) of  %take variant
    {{value, Val}, UQ} ->
      {{cursor, UQ, result, Val}, Ets};  %return it
    {empty, UQ} -> {{cursor, UQ, result, []}, Ets}  %nothing to return
  end.

db_next(Db, {Queue, _Table}) -> next(Db, Queue).

get_db_procedure({StdLib, ExLib, _}, {Collection, Goal}) ->
  Ets = erlog_db_storage:get_db(ets, Collection),
  get_procedure({StdLib, ExLib, Ets}, Goal).

get_procedure({StdLib, ExLib, Db}, Goal) ->
  Functor = erlog_ec_support:functor(Goal),
  Res = case dict:find(Functor, StdLib) of %search built-in first
          {ok, StFun} -> StFun;
          error ->
            case dict:find(Functor, ExLib) of  %search libraryspace then
              {ok, ExFun} -> ExFun;
              error ->
                case catch ets:lookup_element(Db, Functor, 2) of  %search userspace last
                  Cs when is_list(Cs) -> work_with_clauses(Cs);
                  _ -> undefined
                end
            end
        end,
  {Res, Db}.

get_procedure_type({StdLib, ExLib, Db}, Goal) ->
  Functor = erlog_ec_support:functor(Goal),
  Res = case dict:is_key(Functor, StdLib) of %search built-in first
          true -> built_in;
          false ->
            case dict:is_key(Functor, ExLib) of  %search libraryspace then
              true -> compiled;
              false ->
                case ets:member(Db, Functor) of  %search userspace last
                  true -> interpreted;
                  false -> undefined
                end
            end
        end,
  {Res, Db}.

get_interp_functors({_, ExLib, Db}) ->
  Library = dict:fetch_keys(ExLib),

  Res = ets:foldl(fun({Func, _}, Fs) -> [Func | Fs];
    (_, Fs) -> Fs
  end, Library, Db),
  {Res, Db}.

db_listing({StdLib, ExLib, Db}, {Collection, Params}) ->
  Ets = erlog_db_storage:get_db(ets, Collection),
  {Res, _} = listing({StdLib, ExLib, Ets}, {Params}),
  {Res, Db}.

listing({_, _, Db}, {[Functor, Arity]}) ->
  {ets:foldl(
    fun({{F, A} = Res, _}, Acc) when F == Functor andalso A == Arity ->
      [Res | Acc];
      (_, Acc) -> Acc
    end, [], Db), Db};
listing({_, _, Db}, {[Functor]}) ->
  {ets:foldl(
    fun({{F, Arity}, _}, Acc) when F == Functor ->
      [{Functor, Arity} | Acc];
      (_, Acc) -> Acc
    end, [], Db), Db};
listing({_, _, Db}, {[]}) ->
  {ets:foldl(
    fun({Fun, _}, Acc) -> [Fun | Acc];
      (_, Acc) -> Acc
    end, [], Db), Db}.

%% @private
clause(Head, Body0, {_, _, Db}, ClauseFun) ->
  {Functor, Body} = case catch {ok, erlog_ec_support:functor(Head), erlog_ec_body:well_form_body(Body0, false, sture)} of
                      {erlog_error, E} -> erlog_errors:erlog_error(E, Db);
                      {ok, F, B} -> {F, B}
                    end,
  case ets:lookup(Db, Functor) of
    [] -> ets:insert(Db, {Functor, {0, Head, Body}});
    Cs -> ClauseFun(Functor, Cs, Body)
  end.

%% @private
-spec check_duplicates(list(), tuple(), tuple()) -> boolean().
check_duplicates(Cs, Head, Body) ->
  lists:foldl(
    fun({_, {_, H, B}}, _) when H == Head andalso B == Body -> false;  %find same fact
      (_, Acc) -> Acc
    end, true, Cs).

%% @private
form_clauses([]) -> {[], queue:new()};
form_clauses([First | Loaded]) ->
  Queue = queue:from_list(Loaded),
  {First, Queue}.

%% @private
work_with_clauses(Cs) ->
  {First, Cursor} = form_clauses(Cs),
  {cursor, Cursor, result, {clauses, First}}.