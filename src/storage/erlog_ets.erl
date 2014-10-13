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

new() -> {ok, ets:new(eets, [bag, private])}.

new(_) -> {ok, ets:new(eets, [bag, private])}.

assertz_clause({StdLib, ExLib, Db}, {Collection, Head, Body0}) ->
  Ets = erlog_db_storage:get_db(ets, Collection),
  {Res, _} = assertz_clause({StdLib, ExLib, Ets}, {Head, Body0}),
  {Res, Db};
assertz_clause({_, _, Db} = Memory, {Head, Body0}) ->
  clause(Head, Body0, Memory,
    fun(Functor, Cs, Body) ->
      case check_duplicates(Cs, Head, Body) of
        false -> ok;  %found - do nothing
        _ -> ets:insert(Db, {Functor, {length(Cs), Head, Body}}) %not found - insert new
      end
    end),
  {ok, Db}.

asserta_clause({StdLib, ExLib, Db}, {Collection, Head, Body0}) ->
  Ets = erlog_db_storage:get_db(ets, Collection),
  {Res, _} = asserta_clause({StdLib, ExLib, Ets}, {Head, Body0}),
  {Res, Db};
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

retract_clause({StdLib, ExLib, Db}, {Collection, Functor, Ct}) ->
  Ets = erlog_db_storage:get_db(ets, Collection),
  {Res, _} = retract_clause({StdLib, ExLib, Ets}, {Functor, Ct}),
  {Res, Db};
retract_clause({StdLib, ExLib, Db}, {Functor, Ct}) ->
  ok = check_immutable(StdLib, Functor),
  ok = check_immutable(ExLib, Functor),
  case catch ets:lookup_element(Db, Functor, 2) of
    Cs when is_list(Cs) ->
      Object = lists:keyfind(Ct, 1, Cs),
      ets:delete_object(Db, Object);
    _ -> ok
  end,
  {ok, Db}.

abolish_clauses({StdLib, ExLib, Db}, {Collection, Functor}) ->
  Ets = erlog_db_storage:get_db(ets, Collection),
  {Res, _} = abolish_clauses({StdLib, ExLib, Ets}, {Functor}),
  {Res, Db};
abolish_clauses({StdLib, _, Db}, {Functor}) ->
  ok = check_immutable(StdLib, Functor),
  ets:delete(Db, Functor),
  {ok, Db}.

findall({StdLib, ExLib, Db}, {Collection, Functor}) ->  %for db_call
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
          Cursor = form_cursor(),
          {{cursor, Cursor, result, {clauses, form_clauses(CS, Cursor)}}, Db}
      end
  end;
findall({StdLib, ExLib, Db}, {Functor}) ->
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
next(Ets, Cursor) ->
  case get(Cursor) of   %get clauses
    undefined -> {[], Ets};  %empty cursor
    Queue -> case queue:out(Queue) of  %take variant
               {{value, Val}, UQ} ->
                 put(Cursor, UQ),  %save others
                 {{cursor, Cursor, result, Val}, Ets};  %return it
               {empty, _} -> {cursor, Cursor, result, [], Ets}  %nothing to return
             end
  end.

get_procedure({StdLib, ExLib, _}, {Collection, Functor}) ->
  Ets = erlog_db_storage:get_db(ets, Collection),
  get_procedure({StdLib, ExLib, Ets}, {Functor});
get_procedure({StdLib, ExLib, Db}, {Functor}) ->
  Res = case dict:find(Functor, StdLib) of %search built-in first
          {ok, StFun} -> StFun;
          error ->
            case dict:find(Functor, ExLib) of  %search libraryspace then
              {ok, ExFun} -> ExFun;
              error ->
                case catch ets:lookup_element(Db, Functor, 2) of  %search userspace last
                  Cs when is_list(Cs) ->
                    Cursor = form_cursor(),
                    {cursor, Cursor, result, {clauses, form_clauses(Cs, Cursor)}};
                  _ -> undefined
                end
            end
        end,
  {Res, Db}.

get_procedure_type({StdLib, ExLib, Db}, {Functor}) ->
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

listing({StdLib, ExLib, Db}, {Collection, Params}) ->
  Ets = erlog_db_storage:get_db(ets, Collection),
  {Res, _} = listing({StdLib, ExLib, Ets}, {Params}),
  {Res, Db};
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
clause(Head, Body0, {StdLib, ExLib, Db}, ClauseFun) ->
  {Functor, Body} = case catch {ok, ec_support:functor(Head), ec_body:well_form_body(Body0, false, sture)} of
                      {erlog_error, E} -> erlog_errors:erlog_error(E, Db);
                      {ok, F, B} -> {F, B}
                    end,
  ok = check_immutable(StdLib, Functor),  %check built-in functions (read only) for clause
  ok = check_immutable(ExLib, Functor),   %check library functions (read only) for clauses
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
check_immutable(Dict, Functor) -> %TODO may be move me to erlog_memory?
  case dict:is_key(Functor, Dict) of
    false -> ok;
    true -> erlog_errors:permission_error(modify, static_procedure, ec_support:pred_ind(Functor))
  end.

%% @private
form_clauses(Loaded, _) when length(Loaded) =< 1 -> Loaded;
form_clauses([First | Loaded], Cursor) ->
  Queue = queue:from_list(Loaded),
  put(Cursor, Queue),
  First.

form_cursor() ->
  [random:uniform(X) || X <- lists:seq(1, 20)].