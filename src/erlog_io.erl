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

%% File    : erlog_io.erl
%% Author  : Robert Virding
%% Purpose : Some basic i/o functions for Erlog.
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

-module(erlog_io).

-export([scan_file/1,read_file/1,read/1,read/2,
	 write/1,write/2,writeq/1,writeq/2,write_canonical/1,write_canonical/2,
	 write1/1]).

scan_file(File) ->
    case file:open(File, [read]) of
	{ok,Fd} ->
	    try
		{ok,scan_stream(Fd, 1)}
	    catch
		throw:Term -> Term;
		error:Error -> {error,einval,Error};
		exit:Exit -> {exit,einval,Exit}
	    after
		file:close(Fd)
	    end;
	Error -> Error
    end.

scan_stream(Fd, L0) ->
    case scan_erlog_term(Fd, '', L0) of
	{ok,Toks,L1} -> [Toks|scan_stream(Fd, L1)];
	{error,Error,_} -> throw({error,Error});
	{eof,_}=Eof -> Eof
    end.

%% read_file(FileName) -> {ok,[Term]} | {error,Error}.
%% Read a file containing Prolog terms. This has been taken from 'io'
%% but cleaned up using try.

read_file(File) ->
    case file:open(File, [read]) of
	{ok,Fd} ->
	    try
		{ok,read_stream(Fd, 1)}
	    catch
		throw:Term -> Term;
		error:Error -> {error,einval,Error};
		exit:Exit -> {exit,einval,Exit}
	    after
		file:close(Fd)
	    end;
	Error -> Error
    end.

read_stream(Fd, L0) ->
    case scan_erlog_term(Fd, '', L0) of
	{ok,Toks,L1} ->
	    case erlog_parse:term(Toks, L0) of
		{ok,end_of_file} -> [];		%Prolog does this.
		{ok,Term} ->
		    [Term|read_stream(Fd, L1)];
		{error,What} -> throw({error,What})
	    end;
	{error,Error,_} -> throw({error,Error});
	{eof,_} -> []
    end.

%% read([IoDevice], Prompt) -> Term.
%%  A very simple read function. Returns the direct representation of
%%  the term without variable processing.

read(P) -> read(standard_io, P).

read(Io, P) ->
    case scan_erlog_term(Io, P, 1) of
	{ok,Ts,_} ->
	    case erlog_parse:term(Ts) of
		{ok,T} -> {ok,T};
		{error,Pe} -> {error,Pe}
	    end;
	{error,Se,_} -> {error,Se};
	{eof,_} -> {ok,end_of_file}		%Prolog does this
    end.

scan_erlog_term(Io, Prompt, Line) ->
    io:request(Io, {get_until,Prompt,erlog_scan,tokens,[Line]}).

-record(ops, {op=false,q=true}).

%% write([IoDevice], Term) -> ok.
%% writeq([IoDevice], Term) -> ok.
%% write_canonical([IoDevice], Term) -> ok.
%%  A very simple write function. Does not pretty-print but can handle
%%  operators.

write(T) -> write(standard_io, T).

write(Io, T) ->
    io:put_chars(Io, write1(T, 1200, #ops{op=true,q=false})).

writeq(T) -> writeq(standard_io, T).

writeq(Io, T) ->
    io:put_chars(Io, write1(T, 1200, #ops{op=true,q=true})).

write_canonical(T) -> write_canonical(standard_io, T).

write_canonical(Io, T) ->
    io:put_chars(Io, write1(T, 1200, #ops{op=false,q=true})).

%% write1(Term) -> iolist().
%% write1(Term, Precedence, Ops) -> iolist().
%%  The function which does the actual writing.

write1(T) -> write1(T, 1200, #ops{op=true,q=false}).

write1(T, Prec, Ops) when is_atom(T) -> write1_atom(T, Prec, Ops);
write1(T, _, _) when is_number(T) -> io_lib:write(T);
write1({V}, _, _) when is_integer(V) -> "_" ++ integer_to_list(V);
write1({V}, _, _) -> atom_to_list(V);		%Variable
write1([H|T], _, Ops) ->
    [$[,write1(H, 999, Ops),write1_tail(T, Ops),$]];
write1([], _, _) -> "[]";
write1({F,A}, Prec, #ops{op=true}=Ops) ->
    case erlog_parse:prefix_op(F) of
	{yes,OpP,ArgP} ->
	    Out = [write1(F, 1200, Ops),$\s,write1(A, ArgP, Ops)],
	    if OpP > Prec -> [$(,Out,$)];
	       true -> Out
	    end;
	no ->
	    case erlog_parse:postfix_op(F) of
		{yes,ArgP,OpP} ->
		    Out = [write1(A, ArgP, Ops),$\s,write1(F, 1200, Ops)],
		    if OpP > Prec -> [$(,Out,$)];
		       true -> Out
		    end;
		no ->
		    [write1(F, 1200, Ops),$(,write1(A, 999, Ops),$)]
	    end
    end;
write1({F,A1,A2}, Prec, #ops{op=true}=Ops) ->
    case erlog_parse:infix_op(F) of
	{yes,Lp,OpP,Rp} ->
	    Out = [write1(A1, Lp, Ops),$\s,write1(F, 1200, Ops),
		   $\s,write1(A2, Rp,Ops)],
	    if OpP > Prec -> [$(,Out,$)];
	       true -> Out
	    end;
	no ->
	    [write1(F, 1200, Ops),$(,write1(A1, 999, Ops),
				    $,,write1(A2, 999, Ops),$)]
    end;
write1(T, _, Ops) when is_tuple(T) ->
    [F,A1|As] = tuple_to_list(T),
    [write1(F, 1200, Ops),$(,write1(A1, 999, Ops),write1_tail(As, Ops),$)];
write1(T, _, _) ->			     %Else use default Erlang.
    io_lib:write(T).

write1_tail([T|Ts], Ops) ->
    [$,,write1(T, 999, Ops)|write1_tail(Ts, Ops)];
write1_tail([], _) -> [];
write1_tail(T, Ops) -> [$|,write1(T, 999, Ops)].

write1_atom(A, Prec, _) ->
    Out = atom_to_list(A),
    case erlog_parse:prefix_op(A) of
	{yes,OpP,_} when OpP > Prec -> [$(,Out,$)];
	_ ->
	    case erlog_parse:postfix_op(A) of
		{yes,_,OpP} when OpP > Prec -> [$(,Out,$)];
		_ -> Out
	    end
    end.
