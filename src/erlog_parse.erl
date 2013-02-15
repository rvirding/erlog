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

%% File    : erlog_parse.erl
%% Author  : Robert Virding
%% Purpose : Erlog parser
%%
%% Parses Erlog tokens into Erlog terms. Based on the Standard prolog
%% parser and directly coded from the parser description. To handle
%% back-tracking in the parser we use a continuation style using funs
%% where each fun handles one step of what follows. This allows
%% back-tracking. This may not be a specially efficient way of
%% parsing but it is simple and easy to derive from the
%%% description. No logical variables are necessary here.

-module(erlog_parse).

-export([term/1,term/2,format_error/1]).
-export([prefix_op/1,infix_op/1,postfix_op/1]).

%% -compile(export_all).

term(Toks) -> term(Toks, 1).

term(Toks, _) ->
    case term(Toks, 1200, fun(Ts, T) -> all_read(Ts, T) end) of
	{succeed,Term} -> {ok,Term};
	{fail,{Line,Error}} -> {error,{Line,?MODULE,Error}}
    end.

all_read([{dot,_}], Term) -> {succeed,Term};
all_read([{T,L}|_], _) -> syntax_error(L, {operator_expected,{ar,T}});
all_read([{_,L,V}|_], _) -> syntax_error(L, {operator_expected,{ar,V}});
all_read([], _) -> syntax_error(9999, premature_end).

syntax_error(Line, Error) -> {fail,{Line,Error}}.
%% syntax_error(Line, Error) ->
%%     io:fwrite("se: ~p\n", [{Line,Error}]), {fail,{Line,Error}}.

format_error(premature_end) -> "premature end";
format_error({operator_expected,T}) ->
    io_lib:fwrite("operator expected before: ~w", [T]);
format_error({illegal,T}) ->
    io_lib:fwrite("illegal token: ~w", [T]);
format_error(no_term) -> "missing term";
format_error({op_priority,Op}) ->
    io_lib:fwrite("operator priority clash: ~w", [Op]);
format_error({expected,T}) ->
    io_lib:fwrite("~w or operator expected", [T]).

%% term(Tokens, Precedence, Next) -> {succeed,Term} | {fail,Error}.

term([{number,_,N}|Toks], Prec, Next) -> rest_term(Toks, N, 0, Prec, Next);
term([{string,_,S}|Toks], Prec, Next) -> rest_term(Toks, S, 0, Prec, Next);
term([{'(',_}|Toks], Prec, Next) ->
    bracket_term(Toks, Prec, Next);
term([{' (',_}|Toks], Prec, Next) ->
    bracket_term(Toks, Prec, Next);
term([{'{',L},{'}',_}|Toks], Prec, Next) ->
    term([{atom,L,'{}'}|Toks], Prec, Next);
term([{'{',_}|Toks0], Prec, Next) ->
    term(Toks0, 1200,
	 fun (Toks1, Term) ->
		 expect(Toks1, '}', Term,
			fun (Toks2, Term) ->
				rest_term(Toks2, {'{}',Term}, 0, Prec, Next)
			end)
	 end);
term([{'[',_},{']',_}|Toks], Prec, Next) ->
    rest_term(Toks, [], 0, Prec, Next);
term([{'[',_}|Toks0], Prec, Next) ->
    term(Toks0, 999,
	 fun (Toks1, E) ->
		 list_elems(Toks1, [E],
			    fun (Toks2, List) ->
				    rest_term(Toks2, List, 0, Prec, Next)
			    end)
	 end);
term([{var,_,V}|Toks], Prec, Next) -> rest_term(Toks, {V}, 0, Prec, Next);
term([{atom,_,F},{'(',_}|Toks0], Prec, Next) ->
    %% Compound term in functional syntax.
    term(Toks0, 999,
	 fun (Toks1, A) ->
		 arg_list(Toks1, [A],
			  fun (Toks2, Args) ->
				  Term = list_to_tuple([F|Args]),
%				  %% Equivalence of '.'/2 and lists.
% 				  Term = case list_to_tuple([F|Args]) of
% 					     {'.',H,T} -> [H|T];
% 					     Other -> Other
% 					 end,
				  rest_term(Toks2, Term, 0, Prec, Next)
			  end)
	 end);
term([{atom,L,Op}|Toks0], Prec, Next) ->
    case prefix_op(Op) of
	{yes,OpP,ArgP} when Prec >= OpP ->
	    case possible_right_operand(Toks0) of
		true ->
		    %% First try as prefix op, then as atom.
		    Next1 = fun (Toks1, Arg) ->
				    rest_term(Toks1, {Op,Arg}, OpP, Prec, Next)
			    end,
		    cp([fun () -> term(Toks0, ArgP, Next1) end,
			fun () -> rest_term(Toks0, Op, 0, Prec, Next) end]);
		false -> rest_term(Toks0, Op, 0, Prec, Next)
	    end;
	{yes,_,_} ->
	    syntax_error(L, {op_priority,Op});
	no -> rest_term(Toks0, Op, 0, Prec, Next)
    end;
term([T|_], _, _) -> syntax_error(line(T), {illegal,T});
term([], _, _) -> syntax_error(9999, no_term).

%% possible_right_operand(Tokens) -> true | false.
%%  Test if there maybe a possible right operand.

possible_right_operand([{')',_}|_]) -> false;
possible_right_operand([{'}',_}|_]) -> false;
possible_right_operand([{']',_}|_]) -> false;
possible_right_operand([{',',_}|_]) -> false;
possible_right_operand([{'|',_}|_]) -> false;
possible_right_operand(_) -> true.

%% bracket_term(Tokens, Precedence, Next) ->
%%      {succeed,Term} | {fail,Error}.

bracket_term(Toks0, Prec, Next) ->
    term(Toks0, 1200,
	 fun (Toks1, Term) ->
		 expect(Toks1, ')', Term,
			fun (Toks2, Term) ->
				rest_term(Toks2, Term, 0, Prec, Next)
			end)
	 end).

%% rest_term(Tokens, Term, LeftPrec, Precedence, Next) ->
%%      {succeed,Term} | {fail,Error}.
%%  Have a term to the left, test if operator follows or just go on.

rest_term([{atom,L,Op}|Toks0], Term, Left, Prec, Next) ->
    cp([fun () -> infix_term(Op, L, Toks0, Term, Left, Prec, Next) end,
	fun () -> postfix_term(Op, L, Toks0, Term, Left, Prec, Next) end,
	fun () -> Next([{atom,L,Op}|Toks0], Term) end]);
rest_term([{',',L}|Toks0], Term, Left, Prec, Next) ->
    %% , is an operator as well as a separator.
    if  Prec >= 1000, Left < 1000 ->
	    term(Toks0, 1000,
		 fun (Toks1, RArg) ->
			 rest_term(Toks1, {',',Term,RArg}, 1000, Prec, Next)
		 end);
	true -> Next([{',',L}|Toks0], Term)
    end;
rest_term(Toks, Term, _, _, Next) ->
    Next(Toks, Term).

%% infix_term(Operator, Line, Tokens, Term, LeftPrec, Prec, Next) ->
%%      {succeed,Term} | {fail,Error}.
%%  Test if infix operator of correct priority, fail with
%%  operator_expected if not an operator to have some error.

infix_term(Op, L, Toks0, Term, Left, Prec, Next) ->
    case infix_op(Op) of
	{yes,LAP,OpP,RAP} when Prec >= OpP, Left =< LAP ->
	    term(Toks0, RAP,
		 fun (Toks1, Arg2) ->
			 rest_term(Toks1, {Op,Term,Arg2}, OpP, Prec, Next)
		 end);
	{yes,_,_,_} -> syntax_error(L, {op_priority,Op});
	no -> fail
    end.

%% postfix_term(Operator, Line, Tokens, Term, LeftPrec, Prec, Next) ->
%%      {succeed,Term} | {fail,Error}.
%%  Test if postfix operator of correct priority, fail with
%%  operator_expected if not an operator to have some error.

postfix_term(Op, L, Toks0, Term, Left, Prec, Next) ->
    case postfix_op(Op) of
	{yes,ArgP,OpP} when Prec >= OpP, Left =< ArgP ->
	    rest_term(Toks0, {Op,Term}, OpP, Prec, Next);
	{yes,_,_} -> syntax_error(L, {op_priority,Op});
	no -> fail
    end.

%% list_elems(Tokens, RevElems, Next) ->
%%      {succeed,Term} | {fail,Error}.

list_elems([{',',_}|Toks0], REs, Next) ->
    term(Toks0, 999,
	 fun (Toks1, E) ->
		 list_elems(Toks1, [E|REs], Next)
	 end);
list_elems([{'|',_}|Toks0], REs, Next) ->
    term(Toks0, 999,
	 fun (Toks1, E) ->
		 expect(Toks1, ']', lists:reverse(REs, E), Next)
	 end);
list_elems(Toks, REs, Next) ->
    expect(Toks, ']', lists:reverse(REs), Next).

%% arg_list(Tokens, RevArgs, Next) -> {succeed,Term} | {fail,Error}.

arg_list([{',',_}|Toks0], RAs, Next) ->
    term(Toks0, 999,
	 fun (Toks1, Arg) ->
		 arg_list(Toks1, [Arg|RAs], Next)
	 end);
arg_list(Toks, RAs, Next) ->
    expect(Toks, ')', lists:reverse(RAs), Next).

%% expect(Tokens, Token, Term, Next) -> {succeed,Term} | {fail,Error}.

expect([T|Toks], Tok, Term, Next) ->
    case type(T) of
	Tok -> Next(Toks, Term);
	_ -> syntax_error(line(T), {expected,Tok})
    end;
expect([], Tok, _, _) -> syntax_error(9999, {expected,Tok}).

%% cp(Choices) -> {succeed,Term} | {fail,_} | fail.
%%  Special choice point handler for parser. If all clauses fail then
%%  fail with first fail value, this usually gives better error report.

cp([C|Cs]) ->
    case C() of
	{succeed,Res} -> {succeed,Res};
	{fail,_}=Fail -> cp(Cs, Fail);		%Try rest with first fail
	fail -> cp(Cs)				%Stay till we get reason
    end.

cp([C|Cs], Fail) ->
    case C() of
	{succeed,Res} -> {succeed,Res};
	{fail,_} -> cp(Cs, Fail);		%Drop this fail, use first
	fail -> cp(Cs, Fail)
    end;
cp([], Fail) -> Fail.

%% type(Tok) -> Line.
%% line(Tok) -> Line.
%% val(Tok) -> Value.

type(Tok) -> element(1, Tok).
line(Tok) -> element(2, Tok).
val(Tok) -> element(3, Tok).

%% prefix_op(Op) -> {yes,Prec,ArgPrec} | no.

prefix_op('?-') -> {yes,1200,1199};		%fx 1200
prefix_op(':-') -> {yes,1200,1199};		%fx 1200
prefix_op('\\+') -> {yes,900,900};		%fy 900
prefix_op('+') -> {yes,200,200};		%fy 200
prefix_op('-') -> {yes,200,200};		%fy 200
prefix_op('\\') -> {yes,200,200};		%fy 200
prefix_op(_Op) -> no.				%The rest

%% postfix_op(Op) -> {yes,ArgPrec,Prec} | no.

postfix_op('+') -> {yes,500,500};
postfix_op('*') -> {yes,400,400};
postfix_op(_Op) -> no.

%% infix_op(Op) -> {yes,LeftArgPrec,Prec,RightArgPrec} | no.

infix_op(':-') -> {yes,1199,1200,1199};		%xfx 1200
infix_op('-->') -> {yes,1199,1200,1199};	%xfx 1200
infix_op(';') -> {yes,1099,1100,1100};		%xfy 1100
infix_op('->') -> {yes,1049,1050,1050};		%xfy 1050
infix_op(',') -> {yes,999,1000,1000};		%xfy 1000
infix_op('=') -> {yes,699,700,699};		%xfx 700
infix_op('\\=') -> {yes,699,700,699};		%xfx 700
infix_op('==') -> {yes,699,700,699};		%xfx 700
infix_op('@<') -> {yes,699,700,699};		%xfx 700
infix_op('@=<') -> {yes,699,700,699};		%xfx 700
infix_op('@>') -> {yes,699,700,699};		%xfx 700
infix_op('@>=') -> {yes,699,700,699};		%xfx 700
infix_op('=..') -> {yes,699,700,699};		%xfx 700
infix_op('is') -> {yes,699,700,699};		%xfx 700
infix_op('=:=') -> {yes,699,700,699};		%xfx 700
infix_op('=\\=') -> {yes,699,700,699};		%xfx 700
infix_op('<') -> {yes,699,700,699};		%xfx 700
infix_op('=<') -> {yes,699,700,699};		%xfx 700
infix_op('>') -> {yes,699,700,699};		%xfx 700
infix_op('>=') -> {yes,699,700,699};		%xfx 700
infix_op(':') -> {yes,599,600,600};		%xfy 600
infix_op('+') -> {yes,500,500,499};		%yfx 500
infix_op('-') -> {yes,500,500,499};		%yfx 500
infix_op('/\\') -> {yes,500,500,499};		%yfx 500
infix_op('\\/') -> {yes,500,500,499};		%yfx 500
infix_op('*') -> {yes,400,400,399};		%yfx 400
infix_op('/') -> {yes,400,400,399};		%yfx 400
infix_op('//') -> {yes,400,400,399};		%yfx 400
infix_op('rem') -> {yes,400,400,399};		%yfx 400
infix_op('mod') -> {yes,400,400,399};		%yfx 400
infix_op('<<') -> {yes,400,400,399};		%yfx 400
infix_op('>>') -> {yes,400,400,399};		%yfx 400
infix_op('**') -> {yes,199,200,199};		%xfx 200
infix_op('^') -> {yes,199,200,200};		%xfy 200
infix_op(_Op) -> no.
