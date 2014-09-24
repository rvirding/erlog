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
    {ok,Erl} = erlog:new(M, A),
    server_loop(Erl).

%% A simple Erlog shell similar to a "normal" Prolog shell. It allows
%% user to enter goals, see resulting bindings and request next
%% solution.

server_loop(Erl0) ->
    case erlog_io:read('| ?- ') of
	{ok,halt} -> ok;
	{ok,Files} when is_list(Files) ->
	    case reconsult_files(Files, Erl0) of
		{ok,Erl1} ->
		    io:fwrite("Yes\n"),
		    server_loop(Erl1);
		{erlog_error,Error} ->
		    io:fwrite("Error: ~p\n", [Error]),
		    server_loop(Erl0);
		{error,{L,Pm,Pe}} ->
		    io:fwrite("Error: ~w: ~s\n", [L,Pm:format_error(Pe)]),
		    server_loop(Erl0);
		{error,Error} ->
		    io:fwrite("Error: ~p\n", [Error]),
		    server_loop(Erl0)
	    end;
	{ok,{load,Mod}} ->
	    case erlog:load(Erl0, Mod) of
		{ok,Erl1} -> show_bindings([], Erl1);
		{error,Error} ->
		    io:fwrite("Error: ~p\n", [Error]),
		    server_loop(Erl0)
	    end;
	{ok,Goal} ->
	    shell_prove_result(erlog:prove(Erl0, Goal));
	{error,{_,Em,E}} ->
	    io:fwrite("Error: ~s\n", [Em:format_error(E)]),
	    server_loop(Erl0)
    end.

reconsult_files([F|Fs], Erl0) ->
    case erlog:reconsult(Erl0, F) of
	{ok,Erl1} -> reconsult_files(Fs, Erl1);
	{error,Error} -> {error,Error}
    end;
reconsult_files([], Erl) -> {ok,Erl};
reconsult_files(Other, _Db) -> {error,{type_error,list,Other}}.

shell_prove_result({{succeed,Vs},Erl}) -> show_bindings(Vs, Erl);
shell_prove_result({fail,Erl}) ->
    io:fwrite("No\n"),
    server_loop(Erl);
shell_prove_result({{error,Error},Erl}) ->
    %% Errors from the Erlog interpreters.
    io:fwrite("Error: ~p\n", [Error]),
    server_loop(Erl);
shell_prove_result({{'EXIT',Error},Erl}) ->	%No new database here
    %% Errors and exits from user code.
    io:fwrite("EXIT: ~p\n", [Error]),
    server_loop(Erl).

%% show_bindings(VarList, Estate)
%%  Show the bindings and query user for next solution.

show_bindings([], Erl) ->
    io:fwrite("Yes\n"),
    server_loop(Erl);
show_bindings(Vs, Erl) ->
    foreach(fun ({Name,Val}) ->
		    Out = erlog_io:writeq1({'=',{Name},Val}),
		    io:fwrite("~s\n", [Out])
	    end, Vs),
    Line = io:get_line(': '),
    case string:chr(Line, $;) of
	0 ->
	    io:fwrite("Yes\n"),
	    server_loop(Erl);
	_ ->
	    shell_prove_result(erlog:next_solution(Erl))
    end.
