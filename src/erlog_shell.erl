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

-export([start/0,start/1,server/0,server/1]).

-import(lists, [foldl/3,foreach/2]).

start() -> start(default).

start(P) ->
    spawn(fun () -> server(P) end).

server() -> server(default).

server(_) ->
    io:fwrite("Erlog Shell V~s (abort with ^G)\n",
	      [erlang:system_info(version)]),
    server_loop(erlog:new()).

%% A simple Erlog shell similar to a "normal" Prolog shell. It allows
%% user to enter goals, see resulting bindings and request next
%% solution.

server_loop(P0) ->
    case erlog_io:read('| ?- ') of
	{ok,halt} -> ok;
	{ok,Files} when is_list(Files) ->
	    {{ok,Db0},P1} = P0(get_db),
	    case reconsult_files(Files, Db0) of
		{ok,Db1} ->
		    io:fwrite("Yes\n"),
		    {ok,P2} = P1({set_db,Db1}),
		    server_loop(P2);
		{erlog_error,Error} ->
		    io:fwrite("Error: ~p\n", [Error]),
		    server_loop(P0);
		{error,{L,Pm,Pe}} ->
		    io:fwrite("Error: ~w: ~s\n", [L,Pm:format_error(Pe)]),
		    server_loop(P0);
		{error,Error} ->
		    io:fwrite("Error: ~p\n", [Error]),
		    server_loop(P0)
	    end;
	{ok,{load,Mod}} ->
	    case P0({load,Mod}) of
		{ok,P1} -> show_bindings([], P1);
		{{error,Error},P1} ->
		    io:fwrite("Error: ~p\n", [Error]),
		    server_loop(P1)
	    end;
	{ok,Goal} ->
	    shell_prove_result(P0({prove,Goal}));
	{error,{_,Em,E}} ->
	    io:fwrite("Error: ~s\n", [Em:format_error(E)]),
	    server_loop(P0)
    end.

reconsult_files([F|Fs], Db0) ->
    F1 = case filename:extension(F) of
             [] -> [F, ".pl"];
             _ -> F
         end,
    case erlog_file:reconsult(F1, Db0) of
	{ok,Db1} -> reconsult_files(Fs, Db1);
	{erlog_error,Error} -> {erlog_error,Error};
	{error,Error} -> {error,Error}
    end;
reconsult_files([], Db) -> {ok,Db};
reconsult_files(Other, _Db) -> {error,{type_error,list,Other}}.

shell_prove_result({{succeed,Vs},P}) -> show_bindings(Vs, P);
shell_prove_result({fail,P}) ->
    io:fwrite("No\n"),
    server_loop(P);
shell_prove_result({{error,Error},P}) ->
    %% Errors from the Erlog interpreters.
    io:fwrite("Error: ~p\n", [Error]),
    server_loop(P);
shell_prove_result({{'EXIT',Error},P}) ->	%No new database here
    %% Errors and exits from user code.
    io:fwrite("EXIT: ~p\n", [Error]),
    server_loop(P).

%% show_bindings(VarList, Prolog())
%% Show the bindings and query user for next solution.

show_bindings([], P) ->
    io:fwrite("Yes\n"),
    server_loop(P);
show_bindings(Vs, P) ->
    foreach(fun ({Name,Val}) ->
		    Out = erlog_io:writeq1({'=',{Name},Val}),
		    io:fwrite("~s\n", [Out])
	    end, Vs),
    Line = io:get_line(': '),
    case string:chr(Line, $;) of
	0 ->
	    io:fwrite("Yes\n"),
	    server_loop(P);
	_ ->
	    shell_prove_result(P(next_solution))
    end.
