%% Copyright (c) 2008 Robert Virding. All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions
%% are met:
%%
%% 1. Redistributions of source code must retain the above copyright
%%    notice, this list of conditions and the following disclaimer.
%% 2. Redistributions in binary form must reproduce the above copyright
%%    notice, this list of conditions and the following disclaimer in the
%%    documentation and/or other materials provided with the distribution.
%%
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
%% POSSIBILITY OF SUCH DAMAGE.

%%% File    : erlog.erl
%%% Author  : Robert Virding
%%% Purpose : Main interface to the Erlog interpreter.
%%%
%%% Structures	- {Functor,arg1, Arg2,...} where Functor is an atom
%%% Variables	- {Name} where Name is an atom or integer
%%% Lists	- Erlang lists
%%% Atomic	- Erlang constants
%%%
%%% There is no problem with the representation of variables as Prolog
%%% functors of arity 0 are atoms. This representation is much easier
%%% to test for, and create new variables with than using funny atom
%%% names like '$1' (yuch!), and we need LOTS of variables.

-module(erlog).

%% Basic evaluator interface.
-export([new/0]).
%% Interface to server.
-export([start/0,start_link/0]).
-export([prove/2,next_solution/1,
	 consult/2,reconsult/2,get_db/1,set_db/2,halt/1]).
%% User utilities.
-export([is_legal_term/1,vars_in/1]).
-export([consult_file/2,reconsult_file/2]).

-import(lists, [foldl/3,foreach/2]).

%% -compile(export_all).

new() ->
    Db = erlog_int:built_in_db(),
    fun (Cmd) -> top_cmd(Cmd, Db) end.

top_cmd({prove,Goal}, Db) ->
    prove_goal(Goal, Db);
top_cmd(next_solution, Db) ->
    {fail,fun (Cmd) -> top_cmd(Cmd, Db) end};
top_cmd({consult,File}, Db0) ->
    case consult_file(File, Db0) of
	{ok,Db1} -> {ok,fun (Cmd) -> top_cmd(Cmd, Db1) end};
	{erlog_error,Error} ->
	    {{error,Error},fun (Cmd) -> top_cmd(Cmd, Db0) end};
	{error,Error} ->
	    {{error,Error},fun (Cmd) -> top_cmd(Cmd, Db0) end}
    end;
top_cmd({reconsult,File}, Db0) ->
    case reconsult_file(File, Db0) of
	{ok,Db1} -> {ok,fun (Cmd) -> top_cmd(Cmd, Db1) end};
	{erlog_error,Error} ->
	    {{error,Error},fun (Cmd) -> top_cmd(Cmd, Db0) end};
	{error,Error} ->
	    {{error,Error},fun (Cmd) -> top_cmd(Cmd, Db0) end}
    end;
top_cmd(get_db, Db) ->
    {{ok,Db},fun (Cmd) -> top_cmd(Cmd, Db) end};
top_cmd({set_db,NewDb}, _Db) ->
    {ok,fun (Cmd) -> top_cmd(Cmd, NewDb) end};
top_cmd(halt, _Db) -> ok.

prove_goal(Goal0, Db) ->
    Vs = vars_in(Goal0),
    %% Goal may be a list of goals, ensure proper goal.
    Goal1 = unlistify(Goal0),
    %% Must use 'catch' here as 'try' does not do last-call
    %% optimisation.
    prove_result(catch erlog_int:prove_goal(Goal1, Db), Vs, Db).

unlistify([G]) -> G;
unlistify([G|Gs]) -> {',',G,unlistify(Gs)};
unlistify([]) -> true;
unlistify(G) -> G.				%In case it wasn't a list.

prove_result({succeed,Cps,Bs,Vn,Db1}, Vs, _Db0) ->
    {{succeed,erlog_int:dderef(Vs, Bs)},
     fun (Cmd) -> prove_cmd(Cmd, Vs, Cps, Bs, Vn, Db1) end};
prove_result({fail,Db1}, _Vs, _Db0) ->
    {fail,fun (Cmd) -> top_cmd(Cmd, Db1) end};
prove_result({erlog_error,Error,Db1}, _Vs, _Db0) ->
    {{error,Error},fun (Cmd) -> top_cmd(Cmd, Db1) end};
prove_result({erlog_error,Error}, _Vs, Db) ->	%No new database
    {{error,Error},fun (Cmd) -> top_cmd(Cmd, Db) end};
prove_result({'EXIT',Error}, _Vs, Db) ->
    {{'EXIT',Error},fun (Cmd) -> top_cmd(Cmd, Db) end}.

prove_cmd(next_solution, Vs, Cps, _Bs, _Vn, Db) ->
    prove_result(catch erlog_int:fail(Cps, Db), Vs, Db);
prove_cmd(Cmd, _Vs, _Cps, _Bs, _Vn, Db) ->
    top_cmd(Cmd, Db).

%% prove(Erlog, Goal) -> {succeed,Bindings} | fail.
%% next_solution(Erlog) -> {succeed,Bindings} | fail.
%% consult(Erlog, File) -> ok | {error,Error}.
%% reconsult(Erlog, File) -> ok | {error,Error}.
%% get_db(Erlog) -> {ok,Database}.
%% set_db(Erlog, Database) -> ok.
%% halt(Erlog) -> ok.
%% Interface functions to server.

prove(Erl, Goal) ->
    send_request(Erl, {prove,Goal}),
    wait_reply().

next_solution(Erl) ->
    send_request(Erl, next_solution),
    wait_reply().

consult(Erl, File) ->
    send_request(Erl, {consult,File}),
    wait_reply().

reconsult(Erl, File) ->
    send_request(Erl, {reconsult,File}),
    wait_reply().

get_db(Erl) ->
    send_request(Erl, get_db),
    wait_reply().

set_db(Erl, Db) ->
    send_request(Erl, {set_db,Db}),
    wait_reply().

halt(Erl) ->
    send_request(Erl, halt),
    wait_reply().

%% start()
%% start_link()
%% Start an Erlog server.

start() ->
    spawn(fun () -> server_loop(new()) end).

start_link() ->
    spawn_link(fun () -> server_loop(new()) end).

server_loop(P0) ->
    receive
	{erlog_request,From,Req} ->
	    {Res,P1} = P0(Req),
	    send_reply(From, Res),
	    server_loop(P1)
    end.

%% send_request(Erlog, Request)
%% send_reply(To, Reply)
%% wait_reply() -> Reply.

send_request(Erl, Req) -> Erl ! {erlog_request,self(),Req}.
    
send_reply(To, Rep) -> To ! {erlog_reply,Rep}.

wait_reply() ->
    receive
	{erlog_reply,Rep} -> Rep
    end.

%% vars_in(Term) -> [{Name,Var}].
%% Returns an ordered list of {VarName,Variable} pairs.

vars_in(Term) -> vars_in(Term, orddict:new()).

vars_in({'_'}, Vs) -> Vs;			%Never in!
vars_in({Name}=Var, Vs) -> orddict:store(Name, Var, Vs);
vars_in(Struct, Vs) when is_tuple(Struct) ->
    vars_in_struct(Struct, 2, size(Struct), Vs);
vars_in([H|T], Vs) ->
    vars_in(T, vars_in(H, Vs));
vars_in(_, Vs) -> Vs.

vars_in_struct(_Str, I, S, Vs) when I > S -> Vs;
vars_in_struct(Str, I, S, Vs) ->
    vars_in_struct(Str, I+1, S, vars_in(element(I, Str), Vs)).

%% consult_file(File, Database) ->
%%	{ok,NewDatabase} | {error,Error} | {erlog_error,Error}.
%% reconsult_file(File, Database) ->
%%	{ok,NewDatabase} | {error,Error} | {erlog_error,Error}.
%% Load/reload an Erlog file into the interpreter. Reloading will
%% abolish old definitons of clauses.

consult_file(File, Db0) ->
    case erlog_io:read_file(File) of
	{ok,Terms} ->
	    consult_terms(fun consult_assert/2, Db0, Terms);
	Error -> Error
    end.

consult_assert(Term0, Db) ->
    Term1 = erlog_int:expand_term(Term0),
    {ok,erlog_int:assertz_clause(Term1, Db)}.

reconsult_file(File, Db0) ->
    case erlog_io:read_file(File) of
	{ok,Terms} ->
	    case consult_terms(fun reconsult_assert/2, {Db0,[]}, Terms) of
		{ok,{Db1,_Seen1}} -> {ok,Db1};
		Error -> Error
	    end;
	Error -> Error
    end.

reconsult_assert(Term0, {Db0,Seen}) ->
    Term1 = erlog_int:expand_term(Term0),
    Func = functor(Term1),
    case lists:member(Func, Seen) of
	true ->
	    {ok,{erlog_int:assertz_clause(Term1, Db0), Seen}};
	false ->
	    Db1 = erlog_int:abolish_clauses(Func, Db0),
	    {ok,{erlog_int:assertz_clause(Term1, Db1), [Func|Seen]}}
    end.

%% consult_terms(InsertFun, Database, Terms) ->
%%      {ok,NewDatabase} | {erlog_error,Error}.
%% Add terms to the database using InsertFun. Ignore directives and
%% queries.

consult_terms(Ifun, Db, [{':-',_}|Ts]) ->
    consult_terms(Ifun, Db, Ts);
consult_terms(Ifun, Db, [{'?-',_}|Ts]) ->
    consult_terms(Ifun, Db, Ts);
consult_terms(Ifun, Db0, [T|Ts]) ->
    case catch Ifun(T, Db0) of
	{ok,Db1} -> consult_terms(Ifun, Db1, Ts);
	{erlog_error,E,_Db1} -> {erlog_error,E};
	{erlog_error,E} -> {erlog_error,E}
    end;
consult_terms(_Ifun, Db, []) -> {ok,Db}.

functor({':-',H,_B}) -> erlog_int:functor(H);
functor(T) -> erlog_int:functor(T).

%% The old is_constant/1 ?
-define(IS_CONSTANT(T), (not (is_tuple(T) orelse is_list(T)))).

%% is_legal_term(Goal) -> true | false.
%% Test if a goal is a legal Erlog term. Basically just check if
%% tuples are used correctly as structures and variables.

is_legal_term({V}) -> is_atom(V);
is_legal_term([H|T]) ->
    is_legal_term(H) andalso is_legal_term(T);
is_legal_term(T) when is_tuple(T) ->
    if  size(T) >= 2, is_atom(element(1, T)) ->
	    are_legal_args(T, 2, size(T));	%The right tuples.
	true -> false
    end;
is_legal_term(T) when ?IS_CONSTANT(T) -> true;	%All constants, including []
is_legal_term(_T) -> false.

are_legal_args(_T, I, S) when I > S -> true;
are_legal_args(T, I, S) ->
    is_legal_term(element(I, T)) andalso are_legal_args(T, I+1, S).
