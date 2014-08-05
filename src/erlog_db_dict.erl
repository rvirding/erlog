%% Copyright (c) 2014 Robert Virding
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

%% File    : erlog_db_dict.erl
%% Author  : Robert Virding
%% Purpose : Interface to an erlog database built with dict.

%% The database is a dict where the key is the functor pair {Name,Arity}.
%% The value is: built_in |
%%		 {clauses,NextTag,[{Tag,Head,Body}]} |
%%		 {code,{Module,Function}}.
%% Built-ins are defined by the system and cannot manipulated by user
%% code.

-module(erlog_db_dict).

-export([new/1]).
-export([add_built_in/2,add_compiled_proc/4,asserta_clause/4,assertz_clause/4]).
-export([retract_clause/3,abolish_clauses/2]).
-export([get_procedure/2,get_procedure_type/2]).
-export([get_interpreted_functors/1]).

%% Return {ok,E} or catch thrown error and just return it.
-define(RET_CATCH(E), try
			  {ok,E}
		      catch
			  throw:Error -> Error
		      end).

%% new(InitArgs) -> Db.

new(_Args) ->
    dict:new().

%% add_built_in(Db, Functor) -> Db.
%%  Add functor as a built-in in the database.

add_built_in(Db, Functor) ->
    dict:store(Functor, built_in, Db).

%% add_compiled_code(Db, Functor, Module, Function) -> {ok,Db} | error.
%%  Add functor as a compiled procedure with code in M:F in the
%%  database. Check that it is not a built-in, if so return error.

add_compiled_proc(Db, Functor, M, F) ->
    Code = {code,{M,F}},
    Fun = fun (built_in) -> throw(error);
	      (_) -> Code
	  end,
    ?RET_CATCH(dict:update(Functor, Fun, Code, Db)).

%% asserta_clause(Db, Functor, Head, Body) -> {ok,NewDb} | error.
%% assertz_clause(Db, Functor, Head, Body) -> {ok,NewDb} | error.
%%  We DON'T check format and just put it straight into the database.

asserta_clause(Db, Functor, Head, Body) ->
    Fun = fun ({clauses,T,Cs}) ->
		  {clauses,T+1,[{T,Head,Body}|Cs]};
	      (_) -> throw(error)
	  end,
    ?RET_CATCH(dict:update(Functor, Fun, {clauses,1,[{0,Head,Body}]}, Db)).

assertz_clause(Db, Functor, Head, Body) ->
    Fun = fun ({clauses,T,Cs}) ->
		  {clauses,T+1,Cs ++ [{T,Head,Body}]};
	      (_) -> throw(error)
	  end,
    ?RET_CATCH(dict:update(Functor, Fun, {clauses,1,[{0,Head,Body}]}, Db)).

%% retract_clause(Db, Functor, ClauseTag) -> {ok,NewDb} | error.
%%  Retract (remove) the clause with tag ClauseTag from the list of
%%  clauses of Functor.

retract_clause(Db, Functor, Tag) ->
    case dict:find(Functor, Db) of
	{ok,{clauses,Nt,Cs}} ->			%We can retract here
	    Db1 = dict:store(Functor,
			     {clauses,Nt,lists:keydelete(Tag, 1, Cs)}, Db),
	    {ok,Db1};
	{ok,_} -> error;			%We can't retract here
	error -> {ok,Db}			%Do nothing
    end.

%% abolish_clause(Db, Functor) -> {ok,NewDb} | error.

abolish_clauses(Db, Functor) ->
    case dict:find(Functor, Db) of
	{ok,built_in} -> error;			%Can't abolish here
	{ok,{code,_}} -> {ok,dict:erase(Functor, Db)};
	{ok,{clauses,_,_}} -> {ok,dict:erase(Functor, Db)};
	error -> {ok,Db}			%Do nothing
    end.

%% get_procedure(Db, Functor) ->
%%	built_in | {code,{Mod,Func}} | {clauses,[Clause]} | undefined.
%% Return the procedure type and data for a functor.

get_procedure(Db, Functor) ->
    case dict:find(Functor, Db) of
	{ok,built_in} -> built_in;
	{ok,{code,_}=P} -> P;
	{ok,{clauses,_,Cs}} -> {clauses,Cs};
	error -> undefined
    end.

%% get_procedure(Db, Functor) ->
%%	built_in | compiled | interpreted | undefined.
%%  Return the procedure type for a functor.

get_procedure_type(Db, Functor) ->
    case dict:find(Functor, Db) of
	{ok,built_in} -> built_in;
	{ok,{code,_}} -> compiled;
	{ok,{clauses,_,_}} -> interpreted;
	error -> undefined
    end.

%% get_intepreted_functors(Db) -> [Functor].

get_interpreted_functors(Db) ->
    dict:fold(fun (Func, {clauses,_,_}, Fs) -> [Func|Fs];
		  (_, _, Fs) -> Fs
	      end, [], Db).
