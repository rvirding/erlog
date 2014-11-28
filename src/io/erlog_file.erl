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

-include("erlog.hrl").

-export([consult/3, reconsult/3, deconsult/3, load_library/3]).


%% consult(File, Database) ->
%%	{ok,NewDatabase} | {error,Error} | {erlog_error,Error}.
%% reconsult(File, Database) ->
%%	{ok,NewDatabase} | {error,Error} | {erlog_error,Error}.
%% Load/reload an Erlog file into the interpreter. Reloading will
%% abolish old definitons of clauses.
-spec consult(atom(), File :: string(), DbState :: #db_state{}) -> {ok, #db_state{}}.
consult(Consulter, File, DbState) ->
  case Consulter:load(File) of %call erlog_file_consulter implementation
    {ok, Terms} -> iterate_terms(fun consult_assert/2, DbState, Terms);
    Error -> Error
  end.

%% consult to library space
-spec load_library(atom(), File :: string(), DbState :: #db_state{}) -> {ok, #db_state{}}.
load_library(Consulter, File, DbState) ->
  case Consulter:load(File) of %call erlog_file_consulter implementation
    {ok, Terms} -> iterate_terms(fun consult_lib/2, DbState, Terms);
    Error -> Error
  end.

-spec reconsult(atom(), File :: string(), DbState :: #db_state{}) -> {ok, #db_state{}} | {error, Error :: term()}.
reconsult(Consulter, File, DbState) ->
  case Consulter:load(File) of %call erlog_file_consulter implementation
    {ok, Terms} ->
      iterate_terms(fun reconsult_assert/2, {DbState, []}, Terms);
    Error -> Error
  end.

-spec deconsult(atom(), File :: string(), DbState :: #db_state{}) -> {ok, #db_state{}}.
deconsult(Consulter, File, DbState) ->
  case Consulter:load(File) of %call erlog_file_consulter implementation
    {ok, Terms} ->
      iterate_terms(fun deconsult_assert/2, {DbState, []}, Terms);
    Error -> Error
  end.

%% @private
-spec consult_assert(Term0 :: term(), DbState :: #db_state{}) -> {ok, UDbState :: #db_state{}}.
consult_assert(Term0, Db) ->
  Term1 = erlog_ed_logic:expand_term(Term0),
  erlog_memory:assertz_clause(Db, Term1).

%% @private
-spec consult_lib(Term0 :: term(), Db :: pid()) -> {ok, UDbState :: #db_state{}}.
consult_lib(Term0, Db) ->
  Term1 = erlog_ed_logic:expand_term(Term0),
  erlog_memory:load_extended_library(Db, Term1).

%% @private
-spec reconsult_assert(Term0 :: term(), {Db :: #db_state{}, Seen :: list()}) -> {ok, {Db :: #db_state{}, list()}}.
reconsult_assert(Term0, {Db, Seen}) ->
  Term1 = erlog_ed_logic:expand_term(Term0),
  Func = functor(Term1),
  case lists:member(Func, Seen) of
    true ->
      {_, UDb} = erlog_memory:assertz_clause(Db, Term1),
      {ok, {UDb, Seen}};
    false ->
      {_, Udb1} = erlog_memory:abolish_clauses(Db, Func),
      {_, Udb2} = erlog_memory:assertz_clause(Udb1, Term1),
      {ok, {Udb2, [Func | Seen]}}
  end.

%% @private
-spec deconsult_assert(Term0 :: term(), {Db :: pid(), Seen :: list()}) -> {ok, {Db :: #db_state{}, list()}}.
deconsult_assert(Term0, {Db, Seen}) ->
  Term1 = erlog_ed_logic:expand_term(Term0),
  Func = functor(Term1),
  case lists:member(Func, Seen) of
    true ->
      {ok, {Db, Seen}};
    false ->
      {_, Udb1} = erlog_memory:abolish_clauses(Db, Func),
      {_, Udb2} = erlog_memory:assertz_clause(Udb1, Term1),
      {ok, {Udb2, [Func | Seen]}}
  end.

%% @private
%% consult_terms(InsertFun, Database, Terms) ->
%%      {ok,NewDatabase} | {erlog_error,Error}.
%% Add terms to the database using InsertFun. Ignore directives and
%% queries.
-spec iterate_terms(fun(), any(), list()) -> {ok, #db_state{}}.
iterate_terms(Ifun, DbState, [{':-', _} | Ts]) ->
  iterate_terms(Ifun, DbState, Ts);
iterate_terms(Ifun, DbState, [{'?-', _} | Ts]) ->
  iterate_terms(Ifun, DbState, Ts);
iterate_terms(Ifun, DbState, [Term | Ts]) ->
  case catch Ifun(Term, DbState) of
    {ok, UDbState} -> iterate_terms(Ifun, UDbState, Ts);
    {erlog_error, E, _} -> {erlog_error, E};
    {erlog_error, E} -> {erlog_error, E}
  end;
iterate_terms(_, DbState, []) -> {ok, DbState}.

%% @private
functor({':-', H, _B}) -> erlog_ec_support:functor(H);
functor(T) -> erlog_ec_support:functor(T).