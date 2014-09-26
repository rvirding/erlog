%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. июн 2014 18:00
%%%-------------------------------------------------------------------

-module(erlog_dict).

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
  close/1,
  next/1]).

new() -> {ok, dict:new()}.

new(_) -> {ok, dict:new()}.

assertz_clause({StdLib, ExLib, Db}, {Collection, Head, Body0}) ->
  Dict = erlog_db_storage:get_db(dict, Collection),
  {Res, Udict} = assertz_clause({StdLib, ExLib, Dict}, {Head, Body0}),
  erlog_db_storage:update_db(Collection, Udict),
  {Res, Db};
assertz_clause({_, _, Db} = Memory, {Head, Body0}) ->
  Udb = clause(Head, Body0, Memory,
    fun(Functor, Cs, Body) ->
      case check_duplicates(Cs, Head, Body) of
        true -> Db;  %found - do nothing
        _ -> dict:append(Functor, {length(Cs), Head, Body}, Db) %not found - insert new
      end
    end),
  {ok, Udb}.

asserta_clause({StdLib, ExLib, Db}, {Collection, Head, Body0}) ->
  Dict = erlog_db_storage:get_db(dict, Collection),
  {Res, Udict} = asserta_clause({StdLib, ExLib, Dict}, {Head, Body0}),
  erlog_db_storage:update_db(Collection, Udict),
  {Res, Db};
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

retract_clause({StdLib, ExLib, Db}, {Collection, Functor, Ct}) ->
  Dict = erlog_db_storage:get_db(dict, Collection),
  {Res, Udict} = retract_clause({StdLib, ExLib, Dict}, {Functor, Ct}),
  erlog_db_storage:update_db(Collection, Udict),
  {Res, Db};
retract_clause({StdLib, ExLib, Db}, {Functor, Ct}) ->
  ok = check_immutable(StdLib, Functor),
  ok = check_immutable(ExLib, Functor),
  Udb = case dict:is_key(Functor, Db) of
          true ->
            dict:update(Functor, fun(Old) -> lists:keydelete(Ct, 1, Old) end, [], Db);
          false -> Db        %Do nothing
        end,
  {ok, Udb}.

abolish_clauses({StdLib, ExLib, Db}, {Collection, Functor}) ->
  Dict = erlog_db_storage:get_db(dict, Collection),
  {Res, Udict} = abolish_clauses({StdLib, ExLib, Dict}, {Functor}),
  erlog_db_storage:update_db(Collection, Udict),
  {Res, Db};
abolish_clauses({StdLib, _, Db}, {Functor}) ->
  ok = check_immutable(StdLib, Functor),
  Udb = case dict:is_key(Functor, Db) of
          true -> dict:erase(Functor, Db);
          false -> Db        %Do nothing
        end,
  {ok, Udb}.

findall({StdLib, ExLib, Db}, {Collection, Functor}) ->  %for db_call
  Dict = erlog_db_storage:get_db(dict, Collection),
  case dict:find(Functor, StdLib) of %search built-in first
    {ok, StFun} -> {StFun, Db};
    error ->
      case dict:find(Functor, ExLib) of  %search libraryspace then
        {ok, ExFun} -> {ExFun, Db};
        error ->
          case dict:find(Functor, Dict) of  %search userspace last
            {ok, Cs} -> {{external, {clauses, form_clauses(Cs, external)}}, Db};
            error -> {[], Db}
          end
      end
  end;
findall({StdLib, ExLib, Db}, {Functor}) ->  %for bagof
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

close(undefined) -> ok;
close(Cursor) -> put(Cursor, queue:new()). %save empty queue

next(undefined) -> [];
next(Cursor) ->
  Queue = get(Cursor),  %get clauses
  case queue:out(Queue) of  %take variant
    {{value, Val}, UQ} ->
      put(Cursor, UQ),  %save others
      Val;  %return it
    {empty, _} -> []  %nothing to return
  end.

get_procedure({StdLib, ExLib, Db}, {Collection, Functor}) ->
  Dict = erlog_db_storage:get_db(dict, Collection),
  case get_procedure({StdLib, ExLib, Dict}, {{Functor}, external}) of
    {external, {Res, Udict}} -> %return with cursor
      erlog_db_storage:update_db(Collection, Udict),
      {{external, Res}, Db};
    {Res, Udict} ->
      erlog_db_storage:update_db(Collection, Udict),
      {Res, Db} %normal return
  end;
get_procedure({StdLib, ExLib, Db}, Param) ->
  {Functor, Cursor} = check_param(Param),
  Res = case dict:find(Functor, StdLib) of %search built-in first
          {ok, StFun} -> StFun;
          error ->
            case dict:find(Functor, ExLib) of  %search libraryspace then
              {ok, ExFun} -> ExFun;
              error ->
                case dict:find(Functor, Db) of  %search userspace last
                  {ok, Cs} -> {Cursor, {clauses, form_clauses(Cs, Cursor)}};
                  error -> undefined
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

listing({StdLib, ExLib, Db}, {Collection, Params}) ->
  Dict = erlog_db_storage:get_db(dict, Collection),
  {Res, Udict} = listing({StdLib, ExLib, Dict}, {Params}),
  erlog_db_storage:update_db(Collection, Udict),
  {Res, Db};
listing({_, _, Db}, {[Functor, Arity]}) ->
  {dict:fold(
    fun({F, A} = Res, _, Acc) when F == Functor andalso A == Arity ->
      [Res | Acc];
      (_, _, Acc) -> Acc
    end, [], Db), Db};
listing({_, _, Db}, {[Functor]}) ->
  {dict:fold(
    fun({F, Arity}, _, Acc) when F == Functor ->
      [{Functor, Arity} | Acc];
      (_, _, Acc) -> Acc
    end, [], Db), Db};
listing({_, _, Db}, {[]}) ->
  {dict:fetch_keys(Db), Db}.

%% @private
clause(Head, Body0, {StdLib, ExLib, Db}, ClauseFun) ->
  {Functor, Body} = case catch {ok, ec_support:functor(Head), ec_body:well_form_body(Body0, false, sture)} of
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
    true -> erlog_errors:permission_error(modify, static_procedure, ec_support:pred_ind(Functor))
  end.

%% @private
form_clauses(Loaded, _) when length(Loaded) =< 1 -> Loaded;
form_clauses([First | Loaded], Cursor) ->
  Queue = queue:from_list(Loaded),
  put(Cursor, Queue),
  First.

%% @private
check_param({Functor}) -> {Functor, cursor};
check_param({{Functor}, external}) -> {Functor, external}.