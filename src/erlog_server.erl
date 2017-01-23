%% Copyright (c) 2015 Robert Virding
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

%% File    : erlog_server.erl
%% Author  : Robert Virding
%% Purpose : Simple example of an Erlog server process.

%% This is a simple gen_server which implements all the interface
%% functions in the erlog modules as calls to a server.

-module(erlog_server).

-behaviour(gen_server).

%% Management API.
-export([start/0,start_link/0,start/2,start_link/2,stop/1]).

%% User API.
-export([prove/2,next_solution/1,consult/2,reconsult/2,load/2,
	 get_db/1,set_db/2,set_db/3]).

%% The behaviour callbacks.
-export([init/1,terminate/2,code_change/3,
	 handle_call/3,handle_cast/2,handle_info/2]).

%% Management API.
%%  We can start/start_link with either the Erlog default database or
%%  we can specify the database module and initial argument to use.

start_link() ->
    gen_server:start_link(?MODULE, none, []).

start() ->
    gen_server:start(?MODULE, none, []).

start_link(DbMod, DbArg) ->
    gen_server:start_link(?MODULE, {DbMod,DbArg}, []).

start(DbMod, DbArg) ->
    gen_server:start(?MODULE, {DbMod,DbArg}, []).

stop(Pid) ->
    gen_server:call(Pid, stop).

%% User API.
%%  These are all the basic calls in the erlog module.

prove(Pid, Goal) ->
    gen_server:call(Pid, {prove,Goal}).

next_solution(Pid) ->
    gen_server:call(Pid, next_solution).

consult(Pid, File) ->
    gen_server:call(Pid, {consult,File}).

reconsult(Pid, File) ->
    gen_server:call(Pid, {reconsult,File}).

load(Pid, Module) ->
    gen_server:call(Pid, {load,Module}).

get_db(Pid) ->
    gen_server:call(Pid, get_db).

set_db(Pid, DbRef) ->
    gen_server:call(Pid, {set_db,DbRef}).

set_db(Pid, DbMod, DbRef) ->
    gen_server:call(Pid, {set_db,DbMod,DbRef}).

%% Behaviour callbacks.

-record(state, {erlog}).			%Only the erlog state so far

init(none) ->					%Use default database
    {ok,Erlog} = erlog:new(),
    {ok,#state{erlog=Erlog}};
init({DbMod,DbArg}) ->				%We specify database
    {ok,Erlog} = erlog:new(DbMod, DbArg),
    {ok,#state{erlog=Erlog}}.

terminate(_, _) -> ok.				%No need to do anything

handle_call({prove,Goal}, _, #state{erlog=E0}=State) ->
    {Res,E1} = erlog:prove(Goal, E0),
    {reply,Res,State#state{erlog=E1}};
handle_call(next_solution, _, #state{erlog=E0}=State) ->
    {Res,E1} = erlog:next_solution(E0),
    {reply,Res,State#state{erlog=E1}};
handle_call({consult,File}, _, #state{erlog=E0}=State) ->
    case erlog:consult(File, E0) of
	{ok,E1} ->
	    {reply,ok,State#state{erlog=E1}};
	{error,Error} ->
	    {reply,{error,Error},State}
    end;
handle_call({reconsult,File}, _, #state{erlog=E0}=State) ->
    case erlog:reconsult(File, E0) of
	{ok,E1} ->
	    {reply,ok,State#state{erlog=E1}};
	{error,Error} ->
	    {reply,{error,Error},State}
    end;
handle_call({load,Module}, _, #state{erlog=E0}=State) ->
    {ok,E1} = erlog:load(Module, E0),
    {reply,ok,State#state{erlog=E1}};
handle_call(get_db, _, #state{erlog=E}=State) ->
    {reply,erlog:get_db(E),State};
handle_call({set_db,DbRef}, _, #state{erlog=E0}=State) ->
    E1 = erlog:set_db(DbRef, E0),
    {reply,ok,State#state{erlog=E1}};
handle_call({set_db,DbMod,DbRef}, _, #state{erlog=E0}=State) ->
    E1 = erlog:set_db(DbMod, DbRef, E0),
    {reply,ok,State#state{erlog=E1}};
handle_call(stop, _, State) ->
    {stop,normal,ok,State};
handle_call(_Other, _, State) ->		%Ignore unknown requests
    {noreply,State}.				%Let them timeout

%% Unused callbacks.

handle_cast(_, State) ->
    {noreply,State}.

handle_info(_, State) ->
    {noreply,State}.

code_change(_, State, _) ->
    {ok,State}.
