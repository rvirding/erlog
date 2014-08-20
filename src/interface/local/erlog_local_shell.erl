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
	{ok, Proc} = ets_db_storage:start_link(),  %start default ets-implementation of stand-alone database-module
	link(Proc),
	server_loop(Core, normal, []).

%% A simple Erlog shell similar to a "normal" Prolog shell. It allows
%% user to enter goals, see resulting bindings and request next
%% solution.
server_loop(Core, State, Line) ->
	Term = io:get_line('| ?- '),
	Res = case State of
		      select -> erlog:select(Core, lists:append(Line, Term));
		      _ -> erlog:execute(Core, lists:append(Line, Term))
	      end,
	{NewState, NewLine} = process_execute(Res, State, Line, Term),
	case Term of
		"halt.\n" -> ok;
		_ -> server_loop(Core, NewState, NewLine)
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
	print_res(Res),
	{select, []};
process_reply(Res) ->
	print_res(Res),
	{normal, []}.

print_res({Bool, Bindings}) ->
	io:format("~p~n", [Bool]),
	lists:foreach(fun({Var, Value}) -> io:format("~p = ~p~n", [Var, Value]) end, Bindings);
print_res(Res) ->
	io:format("~p~n", [Res]).