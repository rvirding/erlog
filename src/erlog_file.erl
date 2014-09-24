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

-include("erlog_int.hrl").

%% Main interface functions.
-export([consult/2,reconsult/2]).

%% consult(File, DatabaseState) ->
%%     {ok,NewState} | {error,Error} | {erlog_error,Error}.
%% reconsult(File, State) ->
%%     {ok,NewState} | {error,Error} | {erlog_error,Error}.
%%  Load/reload an Erlog file into the interpreter. Reloading will
%%  abolish old definitons of clauses.

consult(File, St) ->
    case erlog_io:read_file(File) of
	{ok,Terms} ->
	    consult_terms(fun consult_assert/2, St, Terms);
	Error -> Error
    end.

consult_assert(Term0, #est{db=Db0}=St) ->
    Term1 = erlog_lib_dcg:expand_term(Term0),
    Db1 = erlog_int:assertz_clause(Term1, Db0),
    {ok,St#est{db=Db1}}.

reconsult(File, St0) ->
    case erlog_io:read_file(File) of
	{ok,Terms} ->
	    case consult_terms(fun reconsult_assert/2, {St0,[]}, Terms) of
		{ok,{St1,_Seen1}} -> {ok,St1};
		Error -> Error
	    end;
	Error -> Error
    end.

reconsult_assert(Term0, {#est{db=Db0}=St,Seen}) ->
    Term1 = erlog_lib_dcg:expand_term(Term0),
    Func = functor(Term1),
    case lists:member(Func, Seen) of
	true ->
	    Db1 = erlog_int:assertz_clause(Term1, Db0),
	    {ok,{St#est{db=Db1}, Seen}};
	false ->
	    Db1 = erlog_int:abolish_clauses(Func, Db0),
	    Db2 = erlog_int:assertz_clause(Term1, Db1),
	    {ok,{St#est{db=Db2},[Func|Seen]}}
    end.

%% consult_terms(InsertFun, Database, Terms) ->
%%      {ok,NewDatabase} | {erlog_error,Error}.
%% Add terms to the database using InsertFun. Ignore directives and
%% queries.

consult_terms(Ifun, Db, [{':-',_}|Ts]) ->
    consult_terms(Ifun, Db, Ts);
consult_terms(Ifun, Db, [{'?-',_}|Ts]) ->
    consult_terms(Ifun, Db, Ts);
consult_terms(Ifun, Db0, [T|Ts]) ->
    case catch Ifun(T, Db0) of
	{ok,Db1} -> consult_terms(Ifun, Db1, Ts);
	{erlog_error,E,_Db1} -> {erlog_error,E};
	{erlog_error,E} -> {erlog_error,E}
    end;
consult_terms(_Ifun, Db, []) -> {ok,Db}.

functor({':-',H,_B}) -> erlog_int:functor(H);
functor(T) -> erlog_int:functor(T).
