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
%% Structures   - {Functor,arg1, Arg2,...} where Functor is an atom
%% Variables    - {Name} where Name is an atom or integer
%% Lists        - Erlang lists
%% Atomic       - Erlang constants
%%
%% There is no problem with the representation of variables as Prolog
%% functors of arity 0 are atoms. This representation is much easier
%% to test for, and create new variables with than using funny atom
%% names like '$1' (yuch!), and we need LOTS of variables.

-module(erlog_io).

-export([scan_file/1,read_file/1,read/1,read/2,read_string/1,
         write_term/2,write_term/3,write_term1/2,
         write/1,write/2,write1/1,writeq/1,writeq/2,writeq1/1,
         write_canonical/1,write_canonical/2,write_canonical1/1]).

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

get_path() ->
    application:get_env(erlog, consult_path, ["."]).

read_file(File) ->
    Path = get_path(),
    case file:path_open(Path, File, [read]) of
        {ok,Fd, _} ->
            try
                {ok,read_stream(Fd, 1)}
            catch
                throw:Term      -> Term;
                error:Error     -> {error,einval,Error};
                exit:Exit       -> {exit,einval,Exit}
            after
                file:close(Fd)
            end;
        Error -> Error
    end.

read_stream(Fd, L0) ->
    case scan_erlog_term(Fd, '', L0) of
        {ok,Toks,L1} ->
            case erlog_parse:term(Toks, L0) of
                {ok,end_of_file} -> [];         %Prolog does this.
                {ok,Term} ->
                    [Term|read_stream(Fd, L1)];
                {error,What} -> throw({error,What})
            end;
        {error,Error,_} -> throw({error,Error});
        {eof,_} -> []
    end.

%% read([IoDevice,] Prompt) -> {ok,Term} | {error,Error}.
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
        {eof,_} -> {ok,end_of_file}             %Prolog does this
    end.

scan_erlog_term(Io, Prompt, Line) ->
    io:request(Io, {get_until,Prompt,erlog_scan,tokens,[Line]}).

%% read_string(String) -> {ok,Term} | {error,Error}.
%%  Read a string. We add an extra space to be kind.

read_string(Cs) ->
    case erlog_scan:string(Cs ++ " ", 1) of     %Ensure ending space
        {ok,Ts,_} ->
            case erlog_parse:term(Ts) of
                {ok,T} -> {ok,T};
                {error,Pe} -> {error,Pe}
            end;
        {error,Se,_} -> {error,Se}
    end.

%% write_term([IoDevice,] Term, WriteOptions) -> ok.
%% write([IoDevice,] Term) -> ok.
%% writeq([IoDevice,] Term) -> ok.
%% write_canonical([IoDevice,] Term) -> ok.
%%  A very simple write function. Does not pretty-print but can handle
%%  operators. The xxx1 verions return an iolist of the characters.

-record(ops, {ignore_ops=false,quoted=false}).

write_term(T, Opts) -> write_term(standard_io, T, Opts).

write_term(Io, T, Opts) ->
    io:put_chars(Io, write_term1(T, Opts)).

write_term1(T, Opts) ->
    Ops = #ops{ignore_ops=lists:member(ignore_ops, Opts),
               quoted=lists:member(quoted, Opts)},
    write_term1(T, 1200, Ops).

write(T) -> write_term(T, []).

write(Io, T) -> write_term(Io, T, []).

write1(T) -> write_term1(T, []).

writeq(T) -> write_term(T, [quoted]).

writeq(Io, T) -> write_term(Io, T, [quoted]).

writeq1(T) -> write_term1(T, [quoted]).

write_canonical(T) -> write_term(T, [ignore_ops,quoted]).

write_canonical(Io, T) -> write_term(Io, T, [ignore_ops,quoted]).

write_canonical1(T) -> write_term1(T, [ignore_ops,quoted]).

%% write_term1(Term, Precedence, Ops) -> iolist().
%%  The function which does the actual writing.

write_term1(T, Prec, Ops) when is_atom(T) -> write_atom1(T, Prec, Ops);
write_term1(T, _, _) when is_number(T) -> io_lib:write(T);
write_term1({V}, _, _) when is_integer(V) -> "_" ++ integer_to_list(V);
write_term1({V}, _, _) -> atom_to_list(V);           %Variable
write_term1([H|T], _, Ops) ->
    [$[,write_term1(H, 999, Ops),write_tail1(T, Ops),$]];
write_term1([], _, _) -> "[]";
write_term1({'{}',A}, _, #ops{ignore_ops=false}=Ops) ->
    [${,write_term1(A, 1200, Ops),$}];
write_term1({F,A}, Prec, #ops{ignore_ops=false}=Ops) ->
    case erlog_parse:prefix_op(F) of
        {yes,OpP,ArgP} ->
            Out = [write_term1(F, 1200, Ops),$\s,write_term1(A, ArgP, Ops)],
            write_prec1(Out, OpP, Prec);
        no ->
            case erlog_parse:postfix_op(F) of
                {yes,ArgP,OpP} ->
                    Out = [write_term1(A, ArgP, Ops),$\s,
                           write_term1(F, 1200, Ops)],
                    write_prec1(Out, OpP, Prec);
                no ->
                    [write_term1(F, 1200, Ops),$(,write_term1(A, 999, Ops),$)]
            end
    end;
write_term1({',',A1,A2}, Prec, #ops{ignore_ops=false}=Ops) ->
    %% Must special case , here.
    Out = [write_term1(A1, 999, Ops),", ",write_term1(A2, 1000, Ops)],
    write_prec1(Out, 1000, Prec);
write_term1({F,A1,A2}, Prec, #ops{ignore_ops=false}=Ops) ->
    case erlog_parse:infix_op(F) of
        {yes,Lp,OpP,Rp} ->
            Out = [write_term1(A1, Lp, Ops),$\s,write_term1(F, 1200, Ops),
                   $\s,write_term1(A2, Rp,Ops)],
            write_prec1(Out, OpP, Prec);
        no ->
            [write_term1(F, 1200, Ops),$(,write_term1(A1, 999, Ops),
                                    $,,write_term1(A2, 999, Ops),$)]
    end;
write_term1(T, _, Ops) when is_tuple(T) ->
    [F,A1|As] = tuple_to_list(T),
    [write_term1(F, 1200, Ops),
     $(,write_term1(A1, 999, Ops),write_tail1(As, Ops),$)];
write_term1(T, _, _) ->                           %Else use default Erlang.
    io_lib:write(T).

%% write_prec1(OutString, OpPrecedence, Precedence) -> iolist().
%%  Encase OutString with (..) if op precedence higher than
%%  precedence.

write_prec1(Out, OpP, Prec) when OpP > Prec -> [$(,Out,$)];
write_prec1(Out, _, _) -> Out.

write_tail1([T|Ts], Ops) ->
    [$,,write_term1(T, 999, Ops)|write_tail1(Ts, Ops)];
write_tail1([], _) -> [];
write_tail1(T, Ops) -> [$|,write_term1(T, 999, Ops)].

write_atom1(A, Prec, #ops{quoted=false}) ->     %No quoting
    do_write_atom1(A, atom_to_list(A), Prec);
write_atom1(A, Prec, _) when A == '!'; A == ';' -> %Special atoms
    do_write_atom1(A, atom_to_list(A), Prec);
write_atom1(A, Prec, _) ->
    case atom_to_list(A) of
        [C|Cs]=Acs ->
            case (lower_case(C) andalso alpha_chars(Cs))
                orelse symbol_chars(Acs) of
                true -> do_write_atom1(A, Acs, Prec);
                false ->
                    Qcs = quote_atom(Acs),
                    do_write_atom1(A, Qcs, Prec)
            end;
        [] -> do_write_atom1(A, "''", Prec)
    end.

do_write_atom1(A, Acs, Prec) ->
    case erlog_parse:prefix_op(A) of
        {yes,OpP,_} when OpP > Prec -> [$(,Acs,$)];
        _ ->
            case erlog_parse:postfix_op(A) of
                {yes,_,OpP} when OpP > Prec -> [$(,Acs,$)];
                _ -> Acs
            end
    end.

quote_atom(Acs) -> [$',Acs,$'].                 %Very naive as yet.

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
