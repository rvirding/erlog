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

-include("erlog_core.hrl").

%% Interface to server.
-export([start_link/1, start_link/0, execute/2, select/2]).

%% Gen server callbacs.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Erlang server code.
-record(state,
{
	db :: atom(), %database
	f_consulter :: fun(), %file consulter
	e_man :: pid(), %event manager, used for debuging and other output (not for return)
	state = normal :: normal | list() %state for solution selecting.
}).

execute(Worker, Command) -> gen_server:call(Worker, {execute, trim_command(Command)}).
select(Worker, Command) -> gen_server:call(Worker, {select, trim_command(Command)}).

-spec start_link() -> pid().
start_link() ->
	gen_server:start_link(?MODULE, [], []).

%% Database is your callback module. Params will be send to it's new(Params) callback
-spec start_link(Params :: proplists:proplist()) -> pid().
start_link(Params) ->
	gen_server:start_link(?MODULE, Params, []).

init([]) -> % use built in database
	{ok, Db} = init_database([]), %default database is ets module
	F = init_consulter([]),
	{ok, E} = gen_event:start_link(),
	gen_event:add_handler(E, erlog_simple_printer, []), %set the default debug module
	{ok, #state{db = Db, f_consulter = F, e_man = E}};
init(Params) -> % use custom database implementation
	FileCon = init_consulter(Params),
	{ok, Db} = init_database(Params),
	{ok, E} = gen_event:start_link(),
	case proplists:get_value(event_h, Params) of  %register handler, if any
		undefined -> ok;
		{Module, Arguments} -> gen_event:add_handler(E, Module, Arguments)
	end,
	{ok, #state{db = Db, f_consulter = FileCon, e_man = E}}.

handle_call({execute, Command}, _From, State) -> %running prolog code in normal mode
	{Res, _} = Repl = case erlog_scan:tokens([], Command, 1) of
		                  {done, Result, _Rest} -> run_command(Result, State); % command is finished, run.
		                  {more, _} -> {{ok, more}, State} % unfinished command. Report it and do nothing.
	                  end,
	NewState = change_state(Repl),
	{reply, Res, NewState};
handle_call({select, Command}, _From, State) ->  %in selection solutions mode
	{Res, _} = Repl = preprocess_command({select, Command}, State),
	NewState = change_state(Repl), % change state, depending on reply
	{reply, Res, NewState}.

handle_cast(halt, St = #state{e_man = E}) ->
	gen_event:stop(E),  %stom all handlers and event man
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
% change state, depending on reply
change_state({{_, select}, State}) -> State;
change_state({_, State}) -> State#state{state = normal}.

%% @private
%% Configurates database with arguments, populates it and returns.
-spec init_database(Params :: proplists:proplist()) -> {ok, Pid :: pid()}.
init_database(Params) ->
	{ok, DbPid} = case proplists:get_value(database, Params) of
		              undefined -> erlog_memory:start_link(erlog_ets);
		              Module ->
			              Args = proplists:get_value(arguments, Params),
			              erlog_memory:start_link(Module, Args)
	              end,
	load_built_in(DbPid),
	{ok, DbPid}.

%% @private
-spec init_consulter(Params :: proplists:proplist()) -> fun() | any().
init_consulter(Params) ->
	case proplists:get_value(f_consulter, Params) of  %get function from params or default
		undefined -> fun erlog_io:read_file/1;
		Other -> Other
	end.

%% @private
load_built_in(Database) ->
	link(Database), %TODO some better solution to clean database, close it properly and free memory after erlog terminates
	%Load basic interpreter predicates
	lists:foreach(fun(Mod) -> Mod:load(Database) end,
		[
			erlog_core,       %Core predicates
			erlog_bips,       %Built in predicates
			erlog_dcg,        %DCG predicates
			erlog_lists,      %Common lists library
			erlog_time        %Bindings for working with data and time
		]).

%% @private
%% Run scanned command
run_command(Command, State) ->
	case erlog_parse:parse_prolog_term(Command) of
		{ok, halt} ->
			gen_server:cast(self(), halt),
			{true, State};
		PrologCmd -> preprocess_command(PrologCmd, State)
	end.

%% @private
%% Preprocess command
preprocess_command({ok, Command}, State = #state{f_consulter = Fun, db = Db}) when is_list(Command) ->  %TODO may be remove me?
	case erlog_logic:reconsult_files(Command, Db, Fun) of
		ok ->
			{true, State};
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
process_command(next, State = #state{state = [Vs, Cps], db = Db, f_consulter = Fcon}) ->
	case erlog_logic:prove_result(catch erlog_errors:fail(#param{choice = Cps, database = Db, f_consulter = Fcon}), Vs) of
		{Atom, Res, Args} -> {{Atom, Res}, State#state{state = Args}};
		Other -> {Other, State}
	end;
process_command(halt, State) ->
	gen_server:cast(self(), halt),
	{ok, State}.

%% @private
prove_goal(Goal0, State = #state{db = Db, f_consulter = Fcon, e_man = Event}) ->
	Vs = erlog_logic:vars_in(Goal0),
	%% Goal may be a list of goals, ensure proper goal.
	Goal1 = erlog_logic:unlistify(Goal0),
	%% Must use 'catch' here as 'try' does not do last-call
	%% optimisation.
	case erlog_logic:prove_result(catch erlog_core:prove_goal(Goal1, Db, Fcon, Event), Vs) of
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