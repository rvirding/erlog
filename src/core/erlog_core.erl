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

%% File    : erlog.erl
%% Author  : Robert Virding
%% Purpose : Main interface to the Erlog interpreter.
%%
%% Structures	- {Functor,arg1, Arg2,...} where Functor is an atom
%% Variables	- {Name} where Name is an atom or integer
%% Lists	- Erlang lists
%% Atomic	- Erlang constants
%%
%% There is no problem with the representation of variables as Prolog
%% functors of arity 0 are atoms. This representation is much easier
%% to test for, and create new variables with than using funny atom
%% names like '$1' (yuch!), and we need LOTS of variables.

-module(erlog_core).
-behaviour(gen_server).
-vsn('0.7').

-include("erlog_int.hrl").

%% Interface to server.
-export([start_link/0]).

%% Api for calling prolog core via erlang
-export([prove/2, next/1, consult/2, reconsult/2, get_db/1, set_db/2, halt/1]).

%% Gen server callbacs.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Erlang server code.
-record(state,
{
	db, %database
	state = normal :: normal | list() %state for solution selecting. atom or list of params.
}).

%% prove(Erlog, Goal) -> {succeed,Bindings} | fail.
%% next(Erlog) -> {succeed,Bindings} | fail.
%% consult(Erlog, File) -> ok | {error,Error}.
%% reconsult(Erlog, File) -> ok | {error,Error}.
%% get_db(Erlog) -> {ok,Database}.
%% set_db(Erlog, Database) -> ok.
%% halt(Erlog) -> ok.
%%  Interface functions to server.
prove(Erl, Goal) when is_list(Goal) ->
	{ok, TS, _} = erlog_scan:string(Goal ++ " "),
	{ok, G} = erlog_parse:term(TS),
	prove(Erl, G);
prove(Erl, Goal) -> gen_server:call(Erl, {prove, Goal}, infinity).

next(Erl) -> gen_server:call(Erl, next, infinity).

consult(Erl, File) -> gen_server:call(Erl, {consult, File}, infinity).

reconsult(Erl, File) -> gen_server:call(Erl, {reconsult, File}, infinity).

get_db(Erl) -> gen_server:call(Erl, get_db, infinity).

set_db(Erl, Db) -> gen_server:call(Erl, {set_db, Db}, infinity).

halt(Erl) -> gen_server:cast(Erl, halt).

start_link() ->
	gen_server:start_link(?MODULE, [], []).

init(_) ->
	Db0 = erlog_int:built_in_db(),    %Basic interpreter predicates
	Db1 = lists:foldl(fun(Mod, Db) -> Mod:load(Db) end, Db0,
		[erlog_bips,      %Built in predicates
			erlog_dcg,      %DCG predicates
			erlog_lists      %Common lists library
		]),
	{ok, #state{db = Db1}}.

handle_call(Command, _From, State) ->
	{Res, NewState} = process_command(Command, State),
	{reply, Res, NewState}.

handle_cast(halt, St) ->
	{stop, normal, St}.

handle_info(_, St) ->
	{noreply, St}.

terminate(_, _) ->
	ok.

code_change(_, _, St) -> {ok, St}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% @private
%% Process command, modify state. Return {Result, NewState}
-spec process_command(tuple() | atom(), State :: #state{}) -> tuple().
process_command({prove, Goal}, State) ->
	prove_goal(Goal, State);
process_command(next, State = #state{state = normal}) ->
	{fail, State};
process_command(next, State = #state{state = [Vs, Cps], db = Db}) ->
	{prove_result(catch erlog_int:fail(Cps, Db), Vs), State};
process_command({consult, File}, State = #state{db = Db}) ->
	case erlog_file:consult(File, Db) of
		{ok, Db1} -> ok;  %TODO Db1?
		{Err, Error} when Err == erlog_error; Err == error ->
			{{error, Error}, State}
	end;
process_command({reconsult, File}, State = #state{db = Db}) ->
	case erlog_file:reconsult(File, Db) of
		{ok, Db1} -> ok;  %TODO Db1?
		{Err, Error} when Err == erlog_error; Err == error ->
			{{error, Error}, State}
	end;
process_command(get_db, State = #state{db = Db}) ->
	{Db, State};
process_command({set_db, NewDb}, State = #state{db = Db}) -> % set new db, return old
	{{ok, Db}, State#state{db = NewDb}};
process_command(halt, State) ->
	gen_server:cast(self(), halt),
	{ok, State}.

%% @private
prove_goal(Goal0, State = #state{db = Db}) ->
	Vs = erlog_logic:vars_in(Goal0),
	%% Goal may be a list of goals, ensure proper goal.
	Goal1 = unlistify(Goal0),
	%% Must use 'catch' here as 'try' does not do last-call
	%% optimisation.
	case prove_result(catch erlog_int:prove_goal(Goal1, Db), Vs) of
		{succeed, Res, Args} ->
			{{succeed, Res}, State#state{state = Args}};
		OtherRes -> {OtherRes, State#state{state = normal}}
	end.

%% @private
unlistify([G]) -> G;
unlistify([G | Gs]) -> {',', G, unlistify(Gs)};
unlistify([]) -> true;
unlistify(G) -> G.        %In case it wasn't a list.

%% @private
prove_result({succeed, Cps, Bs, _Vn, _Db1}, Vs) ->
	{succeed, erlog_int:dderef(Vs, Bs), [Vs, Cps]};
prove_result({fail, _Db1}, _Vs) ->
	fail;
prove_result({erlog_error, Error, _Db1}, _Vs) ->
	{error, Error};
prove_result({erlog_error, Error}, _Vs) ->  %No new database
	{error, Error};
prove_result({'EXIT', Error}, _Vs) ->
	{'EXIT', Error}.