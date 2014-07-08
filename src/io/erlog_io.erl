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

-export([scan_file/1, read_file/1, format_error/1, format_error/2]).
-export([write/1, write/2, write1/1, writeq/1, writeq/2, writeq1/1,
	write_canonical/1, write_canonical/2, write_canonical1/1]).

-record(ops, {op = false, q = true}).

scan_file(File) ->
	case file:open(File, [read]) of
		{ok, Fd} ->
			try
				{ok, scan_stream(Fd, 1)}
			catch
				throw:Term -> Term;
				error:Error -> {error, einval, Error};
				exit:Exit -> {exit, einval, Exit}
			after
				file:close(Fd)
			end;
		Error -> Error
	end.

scan_stream(Fd, L0) ->
	case scan_erlog_term(Fd, '', L0) of
		{ok, Toks, L1} -> [Toks | scan_stream(Fd, L1)];
		{error, Error, _} -> throw({error, Error});
		{eof, _} = Eof -> Eof
	end.

%% read_file(FileName) -> {ok,[Term]} | {error,Error}.
%% Read a file containing Prolog terms. This has been taken from 'io'
%% but cleaned up using try.
read_file(File) ->
	case file:open(File, [read]) of
		{ok, Fd} ->
			try
				{ok, read_stream(Fd, 1)}
			catch
				throw:Term -> Term;
				error:Error -> {error, einval, Error};
				exit:Exit -> {exit, einval, Exit}
			after
				file:close(Fd)
			end;
		Error -> Error
	end.

read_stream(Fd, L0) ->
	case scan_erlog_term(Fd, '', L0) of
		{ok, Toks, L1} ->
			case erlog_parse:term(Toks, L0) of
				{ok, end_of_file} -> [];    %Prolog does this.
				{ok, Term} -> [Term | read_stream(Fd, L1)]; %TODO recurstion is not tail!
				{error, What} -> throw({error, What})
			end;
		{error, Error, _} -> throw({error, Error});
		{eof, _} -> []
	end.

scan_erlog_term(Io, Prompt, Line) ->
	io:request(Io, {get_until, Prompt, erlog_scan, tokens, [Line]}).

%% write([IoDevice], Term) -> ok.
%% writeq([IoDevice], Term) -> ok.
%% write_canonical([IoDevice], Term) -> ok.
%%  A very simple write function. Does not pretty-print but can handle
%%  operators. The xxx1 verions return an iolist of the characters.
write(T) -> write(standard_io, T).

write(Io, T) -> io:put_chars(Io, write1(T)).

write1(T) -> write1(T, 1200, #ops{op = true, q = false}).

writeq(T) -> writeq(standard_io, T).

writeq(Io, T) -> io:put_chars(Io, writeq1(T)).

writeq1(T) -> write1(T, 1200, #ops{op = true, q = true}).

write_canonical(T) -> write_canonical(standard_io, T).

write_canonical(Io, T) -> io:put_chars(Io, write_canonical1(T)).

write_canonical1(T) -> write1(T, 1200, #ops{op = false, q = true}).

%% write1(Term, Precedence, Ops) -> iolist().
%%  The function which does the actual writing.
write1(T, Prec, Ops) when is_atom(T) -> write1_atom(T, Prec, Ops);
write1(T, _, _) when is_number(T) -> io_lib:write(T);
write1({V}, _, _) when is_integer(V) -> "_" ++ integer_to_list(V);
write1({V}, _, _) -> atom_to_list(V);    %Variable
write1([H | T], _, Ops) ->
	[$[, write1(H, 999, Ops), write1_tail(T, Ops), $]];
write1([], _, _) -> "[]";
write1({F, A}, Prec, #ops{op = true} = Ops) ->
	case erlog_parse:prefix_op(F) of
		{yes, OpP, ArgP} ->
			Out = [write1(F, 1200, Ops), $\s, write1(A, ArgP, Ops)],
			write1_prec(Out, OpP, Prec);
		no ->
			case erlog_parse:postfix_op(F) of
				{yes, ArgP, OpP} ->
					Out = [write1(A, ArgP, Ops), $\s, write1(F, 1200, Ops)],
					write1_prec(Out, OpP, Prec);
				no ->
					[write1(F, 1200, Ops), $(, write1(A, 999, Ops), $)]
			end
	end;
write1({',', A1, A2}, Prec, #ops{op = true} = Ops) ->
	%% Must special case , here.
	Out = [write1(A1, 999, Ops), ", ", write1(A2, 1000, Ops)],
	write1_prec(Out, 1000, Prec);
write1({F, A1, A2}, Prec, #ops{op = true} = Ops) ->
	case erlog_parse:infix_op(F) of
		{yes, Lp, OpP, Rp} ->
			Out = [write1(A1, Lp, Ops), $\s, write1(F, 1200, Ops),
				$\s, write1(A2, Rp, Ops)],
			write1_prec(Out, OpP, Prec);
		no ->
			[write1(F, 1200, Ops), $(, write1(A1, 999, Ops),
				$,, write1(A2, 999, Ops), $)]
	end;
write1(T, _, Ops) when is_tuple(T) ->
	[F, A1 | As] = tuple_to_list(T),
	[write1(F, 1200, Ops), $(, write1(A1, 999, Ops), write1_tail(As, Ops), $)];
write1(T, _, _) ->           %Else use default Erlang.
	io_lib:write(T).

%% write1_prec(OutString, OpPrecedence, Precedence) -> iolist().
%%  Encase OutString with (..) if op precedence higher than
%%  precedence.
write1_prec(Out, OpP, Prec) when OpP > Prec -> [$(, Out, $)];
write1_prec(Out, _, _) -> Out.

write1_tail([T | Ts], Ops) ->
	[$,, write1(T, 999, Ops) | write1_tail(Ts, Ops)];
write1_tail([], _) -> [];
write1_tail(T, Ops) -> [$|, write1(T, 999, Ops)].

write1_atom(A, Prec, #ops{q = false}) ->    %No quoting
	write1_atom_1(A, atom_to_list(A), Prec);
write1_atom(A, Prec, _) when A == '!'; A == ';' -> %Special atoms
	write1_atom_1(A, atom_to_list(A), Prec);
write1_atom(A, Prec, _) ->
	case atom_to_list(A) of
		[C | Cs] = Acs ->
			case (lower_case(C) andalso alpha_chars(Cs))
				orelse symbol_chars(Acs) of
				true -> write1_atom_1(A, Acs, Prec);
				false ->
					Qcs = quote_atom(Acs),
					write1_atom_1(A, Qcs, Prec)
			end;
		[] -> write1_atom_1(A, "''", Prec)
	end.

write1_atom_1(A, Acs, Prec) ->
	case erlog_parse:prefix_op(A) of
		{yes, OpP, _} when OpP > Prec -> [$(, Acs, $)];
		_ ->
			case erlog_parse:postfix_op(A) of
				{yes, _, OpP} when OpP > Prec -> [$(, Acs, $)];
				_ -> Acs
			end
	end.

quote_atom(Acs) -> [$', Acs, $'].      %Very naive as yet.

symbol_chars(Cs) -> lists:all(fun symbol_char/1, Cs).

symbol_char($-) -> true;
symbol_char($#) -> true;
symbol_char($$) -> true;
symbol_char($&) -> true;
symbol_char($*) -> true;
symbol_char($+) -> true;
symbol_char($.) -> true;
symbol_char($/) -> true;
symbol_char($\\) -> true;
symbol_char($:) -> true;
symbol_char($<) -> true;
symbol_char($=) -> true;
symbol_char($>) -> true;
symbol_char($?) -> true;
symbol_char($@) -> true;
symbol_char($^) -> true;
symbol_char($~) -> true;
symbol_char(_) -> false.

lower_case(C) -> (C >= $a) and (C =< $z).

alpha_chars(Cs) -> lists:all(fun alpha_char/1, Cs).

alpha_char($_) -> true;
alpha_char(C) when C >= $A, C =< $Z -> true;
alpha_char(C) when C >= $0, C =< $9 -> true;
alpha_char(C) -> lower_case(C).

format_error(Params) -> format_error("Error", Params).
format_error(Type, Params) ->
	B = lists:foldr(
		fun(Param, Acc) when is_list(Param) ->
			[Param | Acc];
			(Param, Acc) ->
				[io_lib:format("~p", [Param]) | Acc]
		end, ["\n"], [Type | Params]),
	string:join(B, ": ").