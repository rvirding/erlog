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

-module(erlog).
-behaviour(gen_server).
-vsn('0.7').

-include("erlog_int.hrl").

%% Interface to server.
-export([start_link/1, start_link/0, execute/2]).

%% Gen server callbacs.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Erlang server code.
-record(state,
{
	db, %database
	state = normal :: normal | list() %state for solution selecting. atom or list of params.
}).

execute(Worker, Command) -> gen_server:call(Worker, {execute, Command}).

-spec start_link() -> pid().
start_link() ->
	gen_server:start_link(?MODULE, [], []).

-spec start_link(Database :: atom()) -> pid().
start_link(Database) ->
	gen_server:start_link(?MODULE, [Database], []).

init([]) -> % use built in database
	{ok, Db} = erlog_memory:start_link(erlog_ets), %default database is ets module
	load_built_in(Db),
	{ok, #state{db = Db}};
init(Database) -> % use custom database implementation
	{ok, Db} = erlog_memory:start_link(Database),
	load_built_in(Db),
	{ok, #state{db = Db}}.

handle_call({execute, Command}, _From, State = #state{state = normal}) -> %in normal mode
	{Res, NewState} = case erlog_scan:tokens([], Command, 1) of
		                  {done, Result, _Rest} -> run_command(Result, State); % command is finished, run.
		                  {more, _} -> {ok, more} % unfinished command. Ask for ending.
	                  end,
	{reply, Res, NewState};
handle_call({execute, Command}, _From, State) ->  %in selection solutions mode
	{Res, NewState} = preprocess_command({select, Command}, State),
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
load_built_in(Database) ->
	link(Database), %TODO some better solution to clean database, close it properly and free memory after erlog terminates
	%Load basic interpreter predicates
	lists:foreach(fun(Mod) -> Mod:load(Database) end,
		[
			erlog_core,       %Core predicates
			erlog_bips,       %Built in predicates
			erlog_dcg,        %DCG predicates
			erlog_lists       %Common lists library
		]).

%% @private
%% Run scanned command
run_command(Command, State) ->
	case erlog_parse:parse_prolog_term(Command) of
		{ok, halt} -> {ok, halt};
		PrologCmd -> preprocess_command(PrologCmd, State)
	end.

%% @private
%% Preprocess command
preprocess_command({ok, Command}, State) when is_list(Command) ->
	{{ok, Db0}, NewState1} = process_command(get_db, State),
	case erlog_logic:reconsult_files(Command, Db0) of
		{ok, Db1} ->
			{{ok, _Db}, NewState2} = process_command({set_db, Db1}, NewState1),
			{<<"Yes">>, NewState2};
		{error, {L, Pm, Pe}} ->
			{erlog_io:format_error([L, Pm:format_error(Pe)]), NewState1};
		{Error, Message} when Error == error; Error == erlog_error ->
			{erlog_io:format_error([Message]), NewState1}
	end;
preprocess_command({ok, Command}, State) ->
	{Res, NewState} = process_command({prove, Command}, State),
	{erlog_logic:shell_prove_result(Res), NewState};
preprocess_command({error, {_, Em, E}}, State) -> {erlog_io:format_error([Em:format_error(E)]), State};
preprocess_command({select, Value}, State) ->
	{Next, State} = process_command(next, State),
	{erlog_logic:select_bindings(Value, Next), State}.

%% @private
%% Process command, modify state. Return {Result, NewState}
-spec process_command(tuple() | atom(), State :: #state{}) -> tuple().
process_command({prove, Goal}, State) ->
	prove_goal(Goal, State);
process_command(next, State = #state{state = normal}) ->
	{fail, State};
process_command(next, State = #state{state = [Vs, Cps], db = Db}) ->
	{erlog_logic:prove_result(catch erlog_errors:fail(Cps, Db), Vs), State};
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
	Goal1 = erlog_logic:unlistify(Goal0),
	%% Must use 'catch' here as 'try' does not do last-call
	%% optimisation.
	case erlog_logic:prove_result(catch erlog_core:prove_goal(Goal1, Db), Vs) of
		{succeed, Res, Args} -> %TODO Args?
			{{succeed, Res}, State};
		OtherRes -> {OtherRes, State#state{state = normal}}
	end.