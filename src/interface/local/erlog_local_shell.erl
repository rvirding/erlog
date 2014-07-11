%% Copyright (c) 2008-2013 Robert Virding
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

%% File : erlog_shell.erl
%% Author : Robert Virding
%% Purpose : A simple Erlog shell.

-module(erlog_local_shell).

-export([start/0]).

-import(lists, [foldl/3, foreach/2]).

start() ->
	io:fwrite("Erlog Shell V~s (abort with ^G)\n",
		[erlang:system_info(version)]),
	{ok, Core} = erlog:start_link(),
	link(Core),
	server_loop(Core, normal, []).

%% A simple Erlog shell similar to a "normal" Prolog shell. It allows
%% user to enter goals, see resulting bindings and request next
%% solution.
server_loop(Core, State, Line) ->
	case io:fread('| ?- ', "~s") of
		{ok, [Term]} ->
			Res = erlog:execute(Core, lists:append(Line, Term)),
			{NewState, NewLine} = process_execute(Res, State, Line, Term),
			server_loop(Core, NewState, NewLine);
		{error, {_, Em, E}} ->
			io:fwrite("Error: ~s\n", [Em:format_error(E)]),
			server_loop(Core, State, Line)
	end.

%% Processes return value after execution.
-spec process_execute(tuple(), atom(), list(), string()) -> tuple().
process_execute({ok, more}, State, Line, Command) ->
	{State, lists:append(Line, Command)};
process_execute({ok, halt}, _, _, _) ->
	io:format("OK."),
	exit(normal);
process_execute(Reply, _, _, _) ->
	process_reply(Reply).

%% Processes reply from prolog. Form it to normal view.
-spec process_reply(tuple()) -> tuple().
process_reply({Res, select}) ->
	io:format("~p~n: ", [Res]),
	{select, []};
process_reply(Res) ->
	io:format("~p~n", [Res]),
	{normal, []}.