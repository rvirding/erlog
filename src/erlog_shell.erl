%% @copyright (c) 2008 Robert Virding. All rights reserved.
%% </b>
%% <p>
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions
%% are met:
%% <ul>
%%   <li> Redistributions of source code must retain the above copyright
%%        notice, this list of conditions and the following disclaimer.</li>
%%   <li> Redistributions in binary form must reproduce the above copyright
%%        notice, this list of conditions and the following disclaimer in the
%%        documentation and/or other materials provided with the distribution.</li>
%% </ul>
%% </p><p>
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
%% FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
%% COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
%% INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%% CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
%% ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%% POSSIBILITY OF SUCH DAMAGE.</P>
%%% @end

%%% @author Robert Virding <robert.virding@telia.com>
%%%	(with thanks to Richard O'Keefe for explaining some finer
%%%	points of the Prolog standard)
%%% @end

%%% @doc <p>Erlog is a Prolog interpreter implemented in Erlang and
%%% 	 integrated with the Erlang runtime system. This is a simple
%%%	 prolog like shell to run Erlog.</p>
%%% @end

-module(erlog_shell).

-export([start/0,start/1,server/0,server/1]).

-import(lists, [foreach/2]).

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
	{ok,Goal} ->
	    shell_prove_result(P0({prove,Goal}));
	{error,{_,Em,E}} ->
	    io:fwrite("Error: ~s\n", [Em:format_error(E)]),
	    server_loop(P0)
    end.

reconsult_files([F|Fs], Db0) ->
    case erlog:reconsult_file(F, Db0) of
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
		    Out = erlog_io:write1(Val),	%Write Erlog term
		    io:fwrite("~s = ~s\n", [Name,Out])
	    end, Vs),
    Line = io:get_line(': '),
    case string:chr(Line, $;) of
	0 ->
	    io:fwrite("Yes\n"),
	    server_loop(P);
	_ ->
	    shell_prove_result(P(next_solution))
    end.
