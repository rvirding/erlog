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
	db :: atom(), %database
	f_consulter :: fun(), %file consulter
	state = normal :: normal | list() %state for solution selecting.
}).

execute(Worker, Command) -> gen_server:call(Worker, {execute, trim_command(Command)}).

-spec start_link() -> pid().
start_link() ->
	gen_server:start_link(?MODULE, [], []).

%% Database is your callback module. Params will be send to it's new(Params) callback
-spec start_link(Params :: proplists:proplist()) -> pid().
start_link(Params) ->
	gen_server:start_link(?MODULE, Params, []).

init([]) -> % use built in database
	{ok, Db} = erlog_memory:start_link(erlog_ets), %default database is ets module
	load_built_in(Db),
	F = fun erlog_io:read_file/1, %set default consult function
	{ok, #state{db = Db, f_consulter = F}};
init(Params) -> % use custom database implementation
	Database = proplists:get_value(database, Params),
	Args = proplists:get_value(arguments, Params),
	FileCon = case proplists:get_value(f_consulter, Params) of  %get function from params or default
		          undefined -> fun erlog_io:read_file/1;
		          Other -> Other
	          end,
	{ok, Db} = erlog_memory:start_link(Database, Args),
	load_built_in(Db),
	{ok, #state{db = Db, f_consulter = FileCon}}.

handle_call({execute, Command}, _From, State = #state{state = normal}) -> %in normal mode
	{Res, UpdateState} = case erlog_scan:tokens([], Command, 1) of
		                     {done, Result, _Rest} -> run_command(Result, State); % command is finished, run.
		                     {more, _} -> {{ok, more}, State} % unfinished command. Ask for ending.
	                     end,
	NewState = case Res of  % change state, depending on reply
		           {_, select} -> UpdateState;
		           _ -> UpdateState#state{state = normal}
	           end,
	{reply, Res, NewState};
handle_call({execute, Command}, _From, State) ->  %in selection solutions mode
	{Reply, NewState} = case preprocess_command({select, Command}, State) of  % change state, depending on reply
		                    {{_, select} = Res, UpdatedState} -> {Res, UpdatedState};
		                    {Res, UpdatedState} -> {Res, UpdatedState#state{state = normal}}
	                    end,
	{reply, Reply, NewState}.

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
preprocess_command({ok, Command}, State = #state{f_consulter = Fun, db = Db}) when is_list(Command) ->
	case erlog_logic:reconsult_files(Command, Db, Fun) of
		ok ->
			{<<"Yes">>, State};
		{error, {L, Pm, Pe}} ->
			{erlog_io:format_error([L, Pm:format_error(Pe)]), State};
		{Error, Message} when Error == error; Error == erlog_error ->
			{erlog_io:format_error([Message]), State}
	end;
preprocess_command({ok, Command}, State) ->
	{Result, NewState} = process_command({prove, Command}, State),
	{erlog_logic:shell_prove_result(Result), NewState};
preprocess_command({error, {_, Em, E}}, State) -> {erlog_io:format_error([Em:format_error(E)]), State};
preprocess_command({select, Value}, State) ->
	{Next, NewState} = process_command(next, State),
	{erlog_logic:select_bindings(Value, Next), NewState}.

%% @private
%% Process command, modify state. Return {Result, NewState}
-spec process_command(tuple() | atom(), State :: #state{}) -> tuple().
process_command({prove, Goal}, State) ->
	prove_goal(Goal, State);
process_command(next, State = #state{state = normal}) ->  % can't select solution, when not in select mode
	{fail, State};
process_command(next, State = #state{state = [Vs, Cps], db = Db}) ->
	case erlog_logic:prove_result(catch erlog_errors:fail(Cps, Db), Vs) of
		{Atom, Res, Args} -> {{Atom, Res}, State#state{state = Args}};
		Other -> {Other, State}
	end;
process_command({consult, File}, State = #state{db = Db, f_consulter = Fun}) -> %TODO consult unused?
	case erlog_file:consult(Fun, File, Db) of
		ok -> ok;
		{Err, Error} when Err == erlog_error; Err == error ->
			{{error, Error}, State}
	end;
process_command({reconsult, File}, State = #state{db = Db, f_consulter = Fun}) -> %TODO reconsult unused?
	case erlog_file:reconsult(Fun, File, Db) of
		ok -> ok;
		{Err, Error} when Err == erlog_error; Err == error ->
			{{error, Error}, State}
	end;
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
		{succeed, Res, Args} -> {{succeed, Res}, State#state{state = Args}};
		OtherRes -> {OtherRes, State}
	end.

%% @private
%% Adds "\r\n" to command. We need this, as erlog_scan reply more on commands without such ending
trim_command(Command) ->
	case lists:suffix([13, 10], Command) of
		true -> Command;
		_ -> lists:append(Command, [13, 10])
	end.