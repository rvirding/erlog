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

%% File    : erlog_db_ets.erl
%% Author  : Robert Virding
%% Purpose : Interface to an erlog database built with ETS.

%% The database is an ets table where the key is the functor pair {Name,Arity}.
%% The value is: {Functor,built_in} |
%%		 {Functor,clauses,NextTag,[{Tag,Head,Body}]} |
%%		 {Functor,code,{Module,Function}}.
%% Built-ins are defined by the system and cannot manipulated by user
%% code.

-module(erlog_db_ets).


-export([new/1]).
-export([add_built_in/2,add_compiled_proc/4,asserta_clause/4,assertz_clause/4]).
-export([retract_clause/3,abolish_clauses/2]).
-export([get_procedure/2,get_procedure_type/2]).
-export([get_interpreted_functors/1]).

%% new(InitArgs) -> Db.

new(Name) ->
    ets:new(Name, [named_table,set,protected,{keypos,1}]).

%% add_built_in(Functor, Database) -> NewDatabase.
%%  Add Functor as a built-in in the database.

add_built_in(Db, Functor) ->
    ets:insert(Db, {Functor,built_in}),
    Db.

%% add_compiled_proc(Db, Functor, Module, Function) -> {ok,NewDb} | error.
%%  Add functor as a compiled procedure with code in M:F in the
%%  database. Check that it is not a built-in, if so return error.

add_compiled_proc(Db, Functor, M, F) ->
    case ets:lookup(Db, Functor) of
	[{_,built_in}] -> error;
	_ ->
	    ets:insert(Db, {Functor,code,{M,F}}),
	    {ok,Db}
    end.

%% asserta_clause(Db, Functor, Head, Body) -> {ok,NewDb} | error.
%% assertz_clause(Db, Functor, Head, Body) -> {ok,NewDb} | error.
%%  We DON'T check format and just put it straight into the database.

asserta_clause(Db, Functor, Head, Body) ->
    case ets:lookup(Db, Functor) of
	[{_,built_in}] -> error;
	[{_,code,_}] -> error;
	[{_,clauses,Tag,Cs}] ->
	    ets:insert(Db, {Functor,clauses,Tag+1,[{Tag,Head,Body}|Cs]}),
	    {ok,Db};
	[] ->
	    ets:insert(Db, {Functor,clauses,1,[{0,Head,Body}]}),
	    {ok,Db}
    end.

assertz_clause(Db, Functor, Head, Body) ->
    case ets:lookup(Db, Functor) of
	[{_,built_in}] -> error;
	[{_,code,_}] -> error;
	[{_,clauses,Tag,Cs}] ->
	    ets:insert(Db, {Functor,clauses,Tag+1,Cs ++ [{Tag,Head,Body}]}),
	    {ok,Db};
	[] ->
	    ets:insert(Db, {Functor,clauses,1,[{0,Head,Body}]}),
	    {ok,Db}
    end.

%% retract_clause(Db, Functor, ClauseTag) -> {ok,NewDb} | error.
%%  Retract (remove) the clause with tag ClauseTag from the list of
%%  clauses of Functor.

retract_clause(F, Ct, Db) ->
    case ets:lookup(Db, F) of
	[{_,built_in}] -> error;
	[{_,code,_}] -> error;
	[{_,clauses,Nt,Cs}] ->
	    ets:insert(Db, {F,clauses,Nt,lists:keydelete(Ct, 1, Cs)}),
	    {ok,Db};
	[] -> {ok,Db}				%Do nothing
    end.

%% abolish_clauses(Database, Functor) -> NewDatabase.

abolish_clauses(Db, Func) ->
    case ets:lookup(Db, Func) of
	[{_,built_in}] -> error;
	[{_,code,_}] ->
	    ets:delete(Db, Func),
	    {ok,Db};
	[{_,clauses,_,_}] ->
	    ets:delete(Db, Func),
	    {ok,Db};
	[] -> {ok,Db}				%Do nothing
    end.

%% get_procedure(Db, Functor) ->
%%	built_in | {code,{Mod,Func}} | {clauses,[Clause]} | undefined.
%% Return the procedure type and data for a functor.

get_procedure(Db, Functor) ->
    case ets:lookup(Db, Functor) of
	[{_,built_in}] -> built_in;
	[{_,code,C}] -> {code,C};
	[{_,clauses,_,Cs}] -> {clauses,Cs};
	[] -> undefined
    end.

%% get_procedure_type(Db, Functor) ->
%%	built_in | compiled | interpreted | undefined.
%%  Return the procedure type for a functor.

get_procedure_type(Db, Functor) ->
    case ets:lookup(Db, Functor) of
	[{_,built_in}] -> built_in;		%A built-in
	[{_,code,_}] -> compiled;		%Compiled (perhaps someday)
	[{_,clauses,_,_}] -> interpreted;	%Interpreted clauses
	[] -> undefined				%Undefined
    end.

%% get_interp_functors(Database) -> [Functor].

get_interpreted_functors(Db) ->
    ets:foldl(fun ({Func,clauses,_,_}, Fs) -> [Func|Fs];
		  (_, Fs) -> Fs
	      end, [], Db).
