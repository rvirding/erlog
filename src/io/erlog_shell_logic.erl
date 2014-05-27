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

-module(erlog_shell_logic).

-export([process_command/2]).

% Gets prolog function and command, executes it.
process_command(Core, Command) when is_list(Command) ->
	{{ok, Db0}, P1} = Core(get_db),
	case reconsult_files(Command, Db0) of
		{ok, Db1} ->
			{ok, P2} = P1({set_db, Db1}),
			{P2, <<"Yes\n">>};
		{error, {L, Pm, Pe}} ->
			{Core, erlog_io:format_error([L, Pm:format_error(Pe)])};
		{Error, Message} when Error == error; Error == erlog_error ->
			{Core, erlog_io:format_error([Message])}
	end;
process_command(Core, Command) ->
	shell_prove_result(Core({prove, Command})).

reconsult_files([F | Fs], Db0) ->
	case erlog_file:reconsult(F, Db0) of
		{ok, Db1} -> reconsult_files(Fs, Db1);
		{erlog_error, Error} -> {erlog_error, Error};
		{error, Error} -> {error, Error}
	end;
reconsult_files([], Db) -> {ok, Db};
reconsult_files(Other, _Db) -> {error, {type_error, list, Other}}.

shell_prove_result({{succeed, Vs}, P}) -> show_bindings(Vs, P);
shell_prove_result({fail, P}) -> {P, <<"No\n">>};
%% Errors from the Erlog interpreters.
shell_prove_result({{error, Error}, P}) -> {P, erlog_io:format_error([Error])};
%Errors and exits from user code. No new database here
shell_prove_result({{'EXIT', Error}, P}) -> {P, erlog_io:format_error("EXIT", [Error])}.

%% show_bindings(VarList, Prolog())
%% Show the bindings and query user for next solution.
show_bindings([], P) -> {P, <<"Yes\n">>};
show_bindings(Vs, P) ->
	Out = lists:foldr(
		fun({Name, Val}, Acc) ->
			[erlog_io:writeq1({'=', {Name}, Val}) | Acc]
		end, [], Vs), %format reply

	F = fun(Selection) ->
		case string:chr(Selection, $;) of
			0 ->
				{P, <<"Yes\n">>};
			_ ->
				shell_prove_result(P(next_solution))
		end
	end,
	{F, Out}.