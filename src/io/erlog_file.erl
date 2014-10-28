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

%% File    : erlog_file.erl
%% Author  : Robert Virding
%% Purpose : The Erlog file consulter.

-module(erlog_file).

-export([consult/3, reconsult/3, load_library/3]).


%% consult(File, Database) ->
%%	{ok,NewDatabase} | {error,Error} | {erlog_error,Error}.
%% reconsult(File, Database) ->
%%	{ok,NewDatabase} | {error,Error} | {erlog_error,Error}.
%% Load/reload an Erlog file into the interpreter. Reloading will
%% abolish old definitons of clauses.
-spec consult(atom(), File :: string(), Db :: pid()) -> ok | tuple().
consult(Consulter, File, Db) ->
  case Consulter:load(File) of %call erlog_file_consulter implementation
    {ok, Terms} -> io:format("consult terms ~p~n", [Terms]), consult_terms(fun consult_assert/2, Db, Terms);
    Error -> Error
  end.

%% consult to library space
-spec load_library(atom(), File :: string(), Db :: pid()) -> ok | tuple().
load_library(Consulter, File, Db) ->
  case Consulter:load(File) of %call erlog_file_consulter implementation
    {ok, Terms} -> consult_terms(fun consult_lib/2, Db, Terms);
    Error -> Error
  end.

-spec reconsult(atom(), File :: string(), Db :: pid()) -> ok | tuple().
reconsult(Consulter, File, Db) ->
  case Consulter:load(File) of %call erlog_file_consulter implementation
    {ok, Terms} ->
      case consult_terms(fun reconsult_assert/2, {Db, []}, Terms) of
        ok -> ok;
        Error -> Error
      end;
    Error -> Error
  end.

%% @private
-spec consult_assert(Term0 :: term(), Db :: pid()) -> {ok, Db :: pid()}.
consult_assert(Term0, Db) ->
  Term1 = erlog_ed_logic:expand_term(Term0),
  check_assert(Db, Term1),
  {ok, Db}.  %TODO refactor consult_terms not to pass DB everywhere!

%% @private
-spec consult_lib(Term0 :: term(), Db :: pid()) -> {ok, Db :: pid()}.
consult_lib(Term0, Db) ->
  Term1 = erlog_ed_logic:expand_term(Term0),
  check_load(Db, Term1),
  {ok, Db}.


%% @private
-spec reconsult_assert(Term0 :: term(), {Db :: pid(), Seen :: list()}) -> {ok, {Db :: pid(), list()}}.
reconsult_assert(Term0, {Db, Seen}) ->
  Term1 = erlog_ed_logic:expand_term(Term0),
  Func = functor(Term1),
  case lists:member(Func, Seen) of
    true ->
      check_assert(Db, Term1),
      {ok, {Db, Seen}};  %TODO refactor consult_terms not to pass DB everywhere!
    false ->
      check_abolish(Db, Func),
      check_assert(Db, Term1),
      {ok, {Db, [Func | Seen]}}
  end.

%% @private
%% consult_terms(InsertFun, Database, Terms) ->
%%      {ok,NewDatabase} | {erlog_error,Error}.
%% Add terms to the database using InsertFun. Ignore directives and
%% queries.
-spec consult_terms(fun(), any(), list()) -> ok | tuple().
consult_terms(Ifun, Params, [{':-', _} | Ts]) ->  %TODO refactor me to make interface for Params unifyed! (or may be lists:foreach will be better this hand made recursion)
  consult_terms(Ifun, Params, Ts);
consult_terms(Ifun, Params, [{'?-', _} | Ts]) ->
  consult_terms(Ifun, Params, Ts);
consult_terms(Ifun, Params, [Term | Ts]) ->
  case catch Ifun(Term, Params) of
    {ok, UpdParams} -> consult_terms(Ifun, UpdParams, Ts);
    {erlog_error, E, _} -> {erlog_error, E};
    {erlog_error, E} -> {erlog_error, E}
  end;
consult_terms(_, _, []) -> ok.

%% @private
functor({':-', H, _B}) -> erlog_ec_support:functor(H);
functor(T) -> erlog_ec_support:functor(T).

%% @private
check_assert(Db, Term) ->
  case erlog_memory:assertz_clause(Db, Term) of
    {erlog_error, E} -> erlog_errors:erlog_error(E);
    _ -> ok
  end.

%% @private
%% Same as check assert, but use library space
check_load(Db, Term) ->
  case erlog_memory:load_extended_library(Db, Term) of
    {erlog_error, E} -> erlog_errors:erlog_error(E);
    _ -> ok
  end.

%% @private
check_abolish(Db, Term) ->
  case erlog_memory:abolish_clauses(Db, Term) of
    {erlog_error, E} -> erlog_errors:erlog_error(E);
    _ -> ok
  end.