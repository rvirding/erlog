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

-include("erlog_core.hrl").

-export([vars_in/1, is_legal_term/1, reconsult_files/3, select_bindings/2, shell_prove_result/1, prove_result/2, unlistify/1]).

%% @private
unlistify([G | Gs]) -> {',', G, unlistify(Gs)};
unlistify([]) -> true;
unlistify(G) -> G.        %In case it wasn't a list.

prove_result({succeed, Cps, Bs, _Vn, _Db1}, Vs) ->
	{succeed, ec_support:dderef(Vs, Bs), [Vs, Cps]};
prove_result({fail, _Db1}, _Vs) ->
	fail;
prove_result({erlog_error, Error, _Db1}, _Vs) ->
	{error, Error};
prove_result({erlog_error, Error}, _Vs) ->  %No new database
	{error, Error};
prove_result({'EXIT', Error}, _Vs) ->
	{'EXIT', Error}.

-spec reconsult_files(list(), pid(), fun()) -> ok | tuple().
reconsult_files([], _Db, _Fun) -> ok; %TODO lists:foldr instead!
reconsult_files([F | Fs], Db, Fun) ->
	case erlog_file:reconsult(Fun, F, Db) of
		ok -> reconsult_files(Fs, Db, Fun);
		{erlog_error, Error} -> {erlog_error, Error};
		{error, Error} -> {error, Error}
	end;
reconsult_files(Other, _Db, _Fun) -> {error, {type_error, list, Other}}.

shell_prove_result({succeed, Vs}) -> show_bindings(Vs);
shell_prove_result(fail) -> false;
shell_prove_result({error, Error}) -> erlog_io:format_error([Error]);
shell_prove_result({'EXIT', Error}) -> erlog_io:format_error("EXIT", [Error]).

%% show_bindings(VarList, Pid)
%% Show the bindings and query user for next solution.
show_bindings([]) -> true;
show_bindings(Vs) ->  %TODO where atoms are created?
	Out = lists:foldr(
		fun({Name, Val}, Acc) ->
%% 			[erlog_io:writeq1({'=', {Name}, Val}) | Acc]
			[{Name, Val} | Acc] %TODO. Test, is this suitable for all variants? If so - writeq can be deleted.
		end, [], Vs), %format reply
	{{true, Out}, select}.

select_bindings(Selection, Next) ->
	case string:chr(Selection, $;) of
		0 -> true;
		_ -> shell_prove_result(Next)
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