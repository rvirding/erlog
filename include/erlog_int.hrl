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

%% File    : erlog_int.erl
%% Author  : Robert Virding
%% Purpose : Basic interpreter of a Prolog definitions.

%% Some standard type macros.

-define(IS_ATOMIC(T), (not (is_tuple(T) orelse (is_list(T) andalso T /= [])))).
-define(IS_FUNCTOR(T), (is_tuple(T) andalso (tuple_size(T) >= 2) andalso is_atom(element(1, T)))).

%% The old is_constant/1 ?
-define(IS_CONSTANT(T), (not (is_tuple(T) orelse is_list(T)))).

%% Define the choice point record
-record(cp, {type, label, data, next, bs, vn}).
-record(cut, {label, next}).
%TODO move me to different hrl files (one lib - one file)
%% record for passing arguments to erlog_core:prove_goal
-record(param,
{
	goal,
	next_goal,
	choice,
	bindings,
	var_num,
	database,
	event_man,
	f_consulter
}).

-define(ERLOG_BIPS,
	[
		%% Term unification and comparison
		{'=', 2},
		{'\\=', 2},
		{'@>', 2},
		{'@>=', 2},
		{'==', 2},
		{'\\==', 2},
		{'@<', 2},
		{'@=<', 2},
		%% Term creation and decomposition.
		{arg, 3},
		{copy_term, 2},
		{functor, 3},
		{'=..', 2},
		%% Type testing.
		{atom, 1},
		{atomic, 1},
		{compound, 1},
		{integer, 1},
		{float, 1},
		{number, 1},
		{nonvar, 1},
		{var, 1},
		%% Atom processing.
		{atom_chars, 2},
		{atom_length, 2},
		%% Arithmetic evaluation and comparison
		{'is', 2},
		{'>', 2},
		{'>=', 2},
		{'=:=', 2},
		{'=\\=', 2},
		{'<', 2},
		{'=<', 2}
	]).

-define(ERLOG_DCG,
	[
		{{expand_term, 2}, erlog_dcg, expand_term_2},
		{{phrase, 3}, erlog_dcg, phrase_3}
	]).

-define(ERLOG_TIME,
	[
		{{localtime, 1}, ?MODULE, localtime_1}
	]).

-define(ERLOG_LISTS,
	[
		{{append, 3}, ?MODULE, append_3},
		{{insert, 3}, ?MODULE, insert_3},
		{{member, 2}, ?MODULE, member_2},
		{{memberchk, 2}, ?MODULE, memberchk_2},
		{{reverse, 2}, ?MODULE, reverse_2},
		{{sort, 2}, ?MODULE, sort_2}
	]).

-define(ERLOG_CORE,
	[
		%% Logic and control.
		{call, 1},
		{',', 2},
		{'!', 0},
		{';', 2},
		{fail, 0},
		{'->', 2},
		{'\\+', 1},
		{once, 1},
		{repeat, 0},
		{true, 0},
		%% Clause creation and destruction.
		{abolish, 1},
		{assert, 1},
		{asserta, 1},
		{assertz, 1},
		{retract, 1},
		{retractall, 1},
		%% Clause retrieval and information.
		{clause, 2},
		{current_predicate, 1},
		{predicate_property, 2},
		%% All solutions
		%% External interface
		{ecall, 2},
		%% File utils
		{consult, 1},
		{reconsult, 1},
		%% Debug functions
		{writeln, 1},
		%% Searching functions
		{findall, 3},
		{bagof, 3},
		{setof, 3}
	]
).