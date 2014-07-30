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

%% File    : erlog_shell.erl
%% Author  : Robert Virding
%% Purpose : A simple Erlog shell.

-module(erlog_shell).

-export([start/0,start/2,server/0,server/2]).

-import(lists, [foldl/3,foreach/2]).

start() -> spawn(fun () -> server() end).

start(M, A) -> spawn(fun () -> server(M, A) end).

server() -> server(erlog_db_dict, null).

server(M, A) ->
    io:fwrite("Erlog Shell V~s (abort with ^G)\n",
	      [erlang:system_info(version)]),
    {ok,St} = erlog:new(M, A),
    server_loop(St).

%% A simple Erlog shell similar to a "normal" Prolog shell. It allows
%% user to enter goals, see resulting bindings and request next
%% solution.

server_loop(St0) ->
    case erlog_io:read('| ?- ') of
	{ok,halt} -> ok;
	{ok,Files} when is_list(Files) ->
	    case reconsult_files(Files, St0) of
		{ok,St1} ->
		    io:fwrite("Yes\n"),
		    server_loop(St1);
		{erlog_error,Error} ->
		    io:fwrite("Error: ~p\n", [Error]),
		    server_loop(St0);
		{error,{L,Pm,Pe}} ->
		    io:fwrite("Error: ~w: ~s\n", [L,Pm:format_error(Pe)]),
		    server_loop(St0);
		{error,Error} ->
		    io:fwrite("Error: ~p\n", [Error]),
		    server_loop(St0)
	    end;
	{ok,{load,Mod}} ->
	    case erlog:load(St0, Mod) of
		{ok,St1} -> show_bindings([], St1);
		{error,Error} ->
		    io:fwrite("Error: ~p\n", [Error]),
		    server_loop(St0)
	    end;
	{ok,Goal} ->
	    shell_prove_result(erlog:prove(St0, Goal));
	{error,{_,Em,E}} ->
	    io:fwrite("Error: ~s\n", [Em:format_error(E)]),
	    server_loop(St0)
    end.

reconsult_files([F|Fs], St0) ->
    case erlog:reconsult(St0, F) of
	{ok,St1} -> reconsult_files(Fs, St1);
	{error,Error} -> {error,Error}
    end;
reconsult_files([], St) -> {ok,St};
reconsult_files(Other, _Db) -> {error,{type_error,list,Other}}.

shell_prove_result({{succeed,Vs},St}) -> show_bindings(Vs, St);
shell_prove_result({fail,St}) ->
    io:fwrite("No\n"),
    server_loop(St);
shell_prove_result({{error,Error},St}) ->
    %% Errors from the Erlog interpreters.
    io:fwrite("Error: ~p\n", [Error]),
    server_loop(St);
shell_prove_result({{'EXIT',Error},St}) ->	%No new database here
    %% Errors and exits from user code.
    io:fwrite("EXIT: ~p\n", [Error]),
    server_loop(St).

%% show_bindings(VarList, Estate)
%%  Show the bindings and query user for next solution.

show_bindings([], St) ->
    io:fwrite("Yes\n"),
    server_loop(St);
show_bindings(Vs, St) ->
    foreach(fun ({Name,Val}) ->
		    Out = erlog_io:writeq1({'=',{Name},Val}),
		    io:fwrite("~s\n", [Out])
	    end, Vs),
    Line = io:get_line(': '),
    case string:chr(Line, $;) of
	0 ->
	    io:fwrite("Yes\n"),
	    server_loop(St);
	_ ->
	    shell_prove_result(erlog:next_solution(St))
    end.
