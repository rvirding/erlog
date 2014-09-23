%% Copyright (c) 2008-2014 Robert Virding
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

%% File    : erlog.erl
%% Author  : Robert Virding
%% Purpose : Main interface to the Erlog interpreter.
%%
%% Structures	- {Functor,arg1, Arg2,...} where Functor is an atom
%% Variables	- {Name} where Name is an atom or integer
%% Lists	- Erlang lists
%% Atomic	- Erlang constants
%%
%% There is no problem with the representation of variables as Prolog
%% functors of arity 0 are atoms. This representation is much easier
%% to test for, and create new variables with than using funny atom
%% names like '$1' (yuch!), and we need LOTS of variables.

-module(erlog).

-include("erlog_int.hrl").

%% Top level erlog state.
-record(erlog, {vs=[],est}).

%% Basic evaluator interface.
-export([new/0,new/2,
	 prove/2,next_solution/1,
	 consult/2,reconsult/2,load/2,
	 get_db/1,set_db/2,set_db/3]).
%% User utilities.
-export([is_legal_term/1,vars_in/1]).

-import(lists, [foldl/3,foreach/2]).

%% -compile(export_all).

%% new() -> {ok,ErlogState}.
%% new(DbModule, DbInitArgs) -> {ok,ErlogState}.
%%  Initialise a new erlog state.

new() -> new(erlog_db_dict, null).		%The default

new(DbMod, DbArg) ->
    {ok,St} = erlog_int:new(DbMod, DbArg),
    Db1 = foldl(fun (M, Db) -> M:load(Db) end, St#est.db,
		[erlog_bips,
		 erlog_lib_dcg,
		 erlog_lib_lists
		]),
    {ok,#erlog{vs=[],est=St#est{db=Db1}}}.

prove(#erlog{}=Erl, Goal) ->
    prove_goal(Goal, Erl).

next_solution(#erlog{vs=Vs,est=St}=Erl) ->
    %% This generates a completely new #est{}.
    prove_result(catch erlog_int:fail(St), Vs, Erl).

consult(#erlog{est=St0}=Erl, File) ->
    case erlog_file:consult(File, St0) of
	{ok,St1} -> {ok,Erl#erlog{est=St1}};
	{erlog_error,Error} -> {error,Error};
	{error,Error} -> {error,Error}
    end.

reconsult(#erlog{est=St0}=Erl, File) ->
    case erlog_file:reconsult(File, St0) of
  	{ok,St1} -> {ok,Erl#erlog{est=St1}};
	{erlog_error,Error} -> {error,Error};
	{error,Error} -> {error,Error}
    end.

load(#erlog{est=St}=Erl, Mod) ->
    Db1 = Mod:load(St#est.db),
    {ok,Erl#erlog{est=St#est{db=Db1}}}.

get_db(#erlog{est=St}) ->
    (St#est.db)#db.ref.

set_db(#erlog{est=St}=Erl, Ref) ->
    #est{db=Db0} = St,
    Db1 = Db0#db{ref=Ref},
    Erl#erlog{est=St#est{db=Db1}}.

set_db(#erlog{est=St}=Erl, Mod, Ref) ->
    #est{db=Db0} = St,
    Db1 = Db0#db{mod=Mod,ref=Ref},
    Erl#erlog{est=St#est{db=Db1}}.

%% Internal functions.

prove_goal(Goal0, #erlog{est=St}=Erl) ->
    Vs = vars_in(Goal0),
    %% Goal may be a list of goals, ensure proper goal.
    Goal1 = unlistify(Goal0),
    %% Must use 'catch' here as 'try' does not do last-call
    %% optimisation.
    %% This generates a completely new #est{}.
    prove_result(catch erlog_int:prove_goal(Goal1, St), Vs, Erl).

unlistify([G]) -> G;
unlistify([G|Gs]) -> {',',G,unlistify(Gs)};
unlistify([]) -> true;
unlistify(G) -> G.				%In case it wasn't a list.

prove_result({succeed,#est{bs=Bs}=St}, Vs, Erl) ->
    {{succeed,erlog_int:dderef(Vs, Bs)},
     Erl#erlog{vs=Vs,est=St}};
prove_result({fail,St}, _Vs, Erl) ->
    {fail,Erl#erlog{vs=[],est=St}};
prove_result({erlog_error,Error,St}, _Vs, Erl) ->
    {{error,Error},Erl#erlog{vs=[],est=St}};
prove_result({erlog_error,Error}, _Vs, Erl) ->	%No new state
    {{error,Error},Erl#erlog{vs=[]}};		% keep old state
prove_result({'EXIT',Error}, _Vs, Erl) ->
    {{'EXIT',Error},Erl#erlog{vs=[]}}.		%Keep old state

%% vars_in(Term) -> [{Name,Var}].
%% Returns an ordered list of {VarName,Variable} pairs.

vars_in(Term) -> vars_in(Term, orddict:new()).

vars_in({'_'}, Vs) -> Vs;			%Never in!
vars_in({Name}=Var, Vs) -> orddict:store(Name, Var, Vs);
vars_in(Struct, Vs) when is_tuple(Struct) ->
    vars_in_struct(Struct, 2, tuple_size(Struct), Vs);
vars_in([H|T], Vs) ->
    vars_in(T, vars_in(H, Vs));
vars_in(_, Vs) -> Vs.

vars_in_struct(_Str, I, S, Vs) when I > S -> Vs;
vars_in_struct(Str, I, S, Vs) ->
    vars_in_struct(Str, I+1, S, vars_in(element(I, Str), Vs)).

%% is_legal_term(Goal) -> true | false.
%% Test if a goal is a legal Erlog term. Basically just check if
%% tuples are used correctly as structures and variables.

is_legal_term({V}) -> is_atom(V);
is_legal_term([H|T]) ->
    is_legal_term(H) andalso is_legal_term(T);
is_legal_term(T) when ?IS_FUNCTOR(T) ->
    are_legal_args(T, 2, tuple_size(T));
is_legal_term(T) when ?IS_ATOMIC(T) -> true;	%All constants, including []
is_legal_term(_T) -> false.

are_legal_args(_T, I, S) when I > S -> true;
are_legal_args(T, I, S) ->
    is_legal_term(element(I, T)) andalso are_legal_args(T, I+1, S).
