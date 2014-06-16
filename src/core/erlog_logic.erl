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
%% Purpose : Module with functions realisation of erlog module api

-module(erlog_logic).

-include("erlog_int.hrl").

-export([vars_in/1, is_legal_term/1, reconsult_files/2, shell_prove_result/1, select_bindings/2]).

reconsult_files([F | Fs], Db0) ->
	case erlog_file:reconsult(F, Db0) of
		{ok, Db1} -> reconsult_files(Fs, Db1);
		{erlog_error, Error} -> {erlog_error, Error};
		{error, Error} -> {error, Error}
	end;
reconsult_files([], Db) -> {ok, Db};
reconsult_files(Other, _Db) -> {error, {type_error, list, Other}}.

shell_prove_result({{succeed, Vs}, P}) -> show_bindings(Vs, P);
shell_prove_result({fail, P}) -> {P, <<"No">>};
%% Errors from the Erlog interpreters.
shell_prove_result({{error, Error}, P}) -> {P, erlog_io:format_error([Error])};
%% Errors and exits from user code. No new database here
shell_prove_result({{'EXIT', Error}, P}) -> {P, erlog_io:format_error("EXIT", [Error])}.

%% show_bindings(VarList, Prolog())
%% Show the bindings and query user for next solution.
show_bindings([], P) -> {P, <<"Yes">>};
show_bindings(Vs, P) ->
	Out = lists:foldr(
		fun({Name, Val}, Acc) ->
			[erlog_io:writeq1({'=', {Name}, Val}) | Acc]
		end, [], Vs), %format reply
	{P, Out, select}.

select_bindings(Selection, Core) ->
	case string:chr(Selection, $;) of
		0 -> {Core, <<"Yes">>};
		_ -> shell_prove_result(gen_server:call(Core, next_solution))
	end.

%% vars_in(Term) -> [{Name,Var}].
%% Returns an ordered list of {VarName,Variable} pairs.
vars_in(Term) -> vars_in(Term, orddict:new()).

vars_in({'_'}, Vs) -> Vs;      %Never in!
vars_in({Name} = Var, Vs) -> orddict:store(Name, Var, Vs);
vars_in(Struct, Vs) when is_tuple(Struct) ->
	vars_in_struct(Struct, 2, size(Struct), Vs);
vars_in([H | T], Vs) ->
	vars_in(T, vars_in(H, Vs));
vars_in(_, Vs) -> Vs.

vars_in_struct(_Str, I, S, Vs) when I > S -> Vs;
vars_in_struct(Str, I, S, Vs) ->
	vars_in_struct(Str, I + 1, S, vars_in(element(I, Str), Vs)).

%% is_legal_term(Goal) -> true | false.
%% Test if a goal is a legal Erlog term. Basically just check if
%% tuples are used correctly as structures and variables.
is_legal_term({V}) -> is_atom(V);
is_legal_term([H | T]) ->
	is_legal_term(H) andalso is_legal_term(T);
is_legal_term(T) when is_tuple(T) ->
	if tuple_size(T) >= 2, is_atom(element(1, T)) ->
		are_legal_args(T, 2, size(T));  %The right tuples.
		true -> false
	end;
is_legal_term(T) when ?IS_ATOMIC(T) -> true;  %All constants, including []
is_legal_term(_T) -> false.

are_legal_args(_T, I, S) when I > S -> true;
are_legal_args(T, I, S) ->
	is_legal_term(element(I, T)) andalso are_legal_args(T, I + 1, S).