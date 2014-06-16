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

-include("erlog_int.hrl").

%% Basic evaluator interface.
-export([new/0]).
%% Interface to server.
-export([start/0, start_link/0]).
-export([prove/2, next_solution/1, consult/2, reconsult/2, get_db/1, set_db/2, halt/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-behaviour(gen_server).
-vsn('0.6').

%% new() -> erlog().
%%  Define an Erlog instance. This is a fun which is called with the
%%  top-level command and returns the result and the continutation in
%%  a new fun.

new() ->  %TODO link with spawning process to die with it
	Db0 = erlog_int:built_in_db(),    %Basic interpreter predicates
	Db1 = lists:foldl(fun(Mod, Db) -> Mod:load(Db) end, Db0,
		[erlog_bips,      %Built in predicates
			erlog_dcg,      %DCG predicates
			erlog_lists      %Common lists library
		]),
	fun(Cmd) -> top_cmd(Cmd, Db1) end.
%TODO OTP me?
top_cmd({prove, Goal}, Db) ->
	prove_goal(Goal, Db);
top_cmd(next_solution, Db) ->
	{fail, fun(Cmd) -> top_cmd(Cmd, Db) end};
top_cmd({consult, File}, Db0) ->
	case erlog_file:consult(File, Db0) of
		{ok, Db1} -> {ok, fun(Cmd) -> top_cmd(Cmd, Db1) end};
		{erlog_error, Error} ->
			{{error, Error}, fun(Cmd) -> top_cmd(Cmd, Db0) end};
		{error, Error} ->
			{{error, Error}, fun(Cmd) -> top_cmd(Cmd, Db0) end}
	end;
top_cmd({reconsult, File}, Db0) ->
	case erlog_file:reconsult(File, Db0) of
		{ok, Db1} -> {ok, fun(Cmd) -> top_cmd(Cmd, Db1) end};
		{erlog_error, Error} ->
			{{error, Error}, fun(Cmd) -> top_cmd(Cmd, Db0) end};
		{error, Error} ->
			{{error, Error}, fun(Cmd) -> top_cmd(Cmd, Db0) end}
	end;
top_cmd(get_db, Db) ->
	{{ok, Db}, fun(Cmd) -> top_cmd(Cmd, Db) end};
top_cmd({set_db, NewDb}, _Db) ->
	{ok, fun(Cmd) -> top_cmd(Cmd, NewDb) end};
top_cmd(halt, _Db) -> ok.

prove_goal(Goal0, Db) ->
	Vs = erlog_logic:vars_in(Goal0),
	%% Goal may be a list of goals, ensure proper goal.
	Goal1 = unlistify(Goal0),
	%% Must use 'catch' here as 'try' does not do last-call
	%% optimisation.
	prove_result(catch erlog_int:prove_goal(Goal1, Db), Vs, Db).

unlistify([G]) -> G;
unlistify([G | Gs]) -> {',', G, unlistify(Gs)};
unlistify([]) -> true;
unlistify(G) -> G.        %In case it wasn't a list.

prove_result({succeed, Cps, Bs, Vn, Db1}, Vs, _Db0) ->
	{{succeed, erlog_int:dderef(Vs, Bs)},
		fun(Cmd) -> prove_cmd(Cmd, Vs, Cps, Bs, Vn, Db1) end};
prove_result({fail, Db1}, _Vs, _Db0) ->
	{fail, fun(Cmd) -> top_cmd(Cmd, Db1) end};
prove_result({erlog_error, Error, Db1}, _Vs, _Db0) ->
	{{error, Error}, fun(Cmd) -> top_cmd(Cmd, Db1) end};
prove_result({erlog_error, Error}, _Vs, Db) ->  %No new database
	{{error, Error}, fun(Cmd) -> top_cmd(Cmd, Db) end};
prove_result({'EXIT', Error}, _Vs, Db) ->
	{{'EXIT', Error}, fun(Cmd) -> top_cmd(Cmd, Db) end}.

prove_cmd(next_solution, Vs, Cps, _Bs, _Vn, Db) ->
	prove_result(catch erlog_int:fail(Cps, Db), Vs, Db);
prove_cmd(Cmd, _Vs, _Cps, _Bs, _Vn, Db) ->
	top_cmd(Cmd, Db).

%% prove(Erlog, Goal) -> {succeed,Bindings} | fail.
%% next_solution(Erlog) -> {succeed,Bindings} | fail.
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

next_solution(Erl) -> gen_server:call(Erl, next_solution, infinity).

consult(Erl, File) -> gen_server:call(Erl, {consult, File}, infinity).

reconsult(Erl, File) -> gen_server:call(Erl, {reconsult, File}, infinity).

get_db(Erl) -> gen_server:call(Erl, get_db, infinity).

set_db(Erl, Db) -> gen_server:call(Erl, {set_db, Db}, infinity).

halt(Erl) -> gen_server:cast(Erl, halt).

%% Erlang server code.
-record(state, {erlog}).      %Erlog state

start() ->
	gen_server:start(?MODULE, [], []).

start_link() ->
	gen_server:start_link(?MODULE, [], []).

init(_) ->
	{ok, #state{erlog = new()}}.

handle_call(Req, _, St) ->
	{Res, Erl} = (St#state.erlog)(Req),
	{reply, Res, St#state{erlog = Erl}}.

handle_cast(halt, St) ->
	{stop, normal, St}.

handle_info(_, St) ->
	{noreply, St}.

terminate(_, St) ->
	(St#state.erlog)(halt).

code_change(_, _, St) -> {ok, St}.