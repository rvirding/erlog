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

%%% File    : erlog_scan.xrl
%%% Author  : Robert Virding
%%% Purpose : Token definitions for Erlog.

Definitions.
B	= (0|1)
O	= [0-7]
D	= [0-9]
H	= [0-9a-fA-F]
U	= [A-Z]
L	= [a-z]
A	= ({U}|{L}|{D}|_)
G	= [-#$&*+./\\:<=>?@^~]
S	= [](),[}{|]
WS	= ([\000-\s]|%.*)

Rules.
{D}+\.{D}+((E|e)(\+|\-)?{D}+)? :
			{token,{number,TokenLine,list_to_float(TokenChars)}}.
{D}+		:	{token,{number,TokenLine,list_to_integer(TokenChars)}}.
0b{B}+		:	base(TokenLine, string:substr(TokenChars, 3), 2).
0o{O}+		:	base(TokenLine, string:substr(TokenChars, 3), 8).
0x{H}+		:	base(TokenLine, string:substr(TokenChars, 3), 16).
0'(\\{O}+\\|\\x{H}+\\|\\.|.) :
			{token,{number,TokenLine,hd(chars(string:substr(TokenChars, 3)))}}.
{L}{A}*		:	{token,{atom,TokenLine,list_to_atom(TokenChars)}}.
!		:	{token,{atom,TokenLine,'!'}}.
;		:	{token,{atom,TokenLine,';'}}.
{G}+		:	{token,{atom,TokenLine,list_to_atom(TokenChars)}}.
'(\\{O}+\\|\\x{H}+\\|\\.|[^'])*' :
			%% Strip quotes.
			S = string:substr(TokenChars, 2, TokenLen - 2),
			case catch list_to_atom(chars(S)) of
			    {'EXIT',_} -> {error,"illegal atom " ++ TokenChars};
			    Atom -> {token,{atom,TokenLine,Atom}}
			end.
({U}|_){A}*	:	{token,{var,TokenLine,list_to_atom(TokenChars)}}.
"(\\{O}+\\|\\x{H}+\\|\\.|[^"])*" :
			%% Strip quotes.
			S = string:substr(TokenChars, 2, TokenLen - 2),
			{token,{string,TokenLine,chars(S)}}.
%% Must separate ( preceded by white space from those that aren't.
{WS}+\(		:	{token,{' (',TokenLine}}.
{S}		:	{token,{list_to_atom(TokenChars),TokenLine}}.
\.{WS}		:	{end_token,{dot,TokenLine}}.
{WS}+		:	skip_token.

Erlang code.

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

%%% File    : erlog_scan.erl
%%% Author  : Robert Virding
%%% Purpose : Token definitions for Erlog.
%%%
%%% (C)Robert Virding. This stuff is mine, distributed without
%%% warranty "as is" and may not be used commercially without written
%%% permission.

-import(string, [substr/2,substr/3]).

%% base(Line, Chars, Base) -> Integer.
%% Convert a string of Base characters into a number. We know that
%% the strings only contain the correct character.

base(L, Cs, B) ->
    case base1(Cs, B, 0) of
	{N,[]} -> {token,{number,L,N}};
	{_,_} -> {error,"illegal based number"}
    end.

base1([C|Cs], Base, SoFar) when C >= $0, C =< $9, C < Base + $0 ->
    Next = SoFar * Base + (C - $0),
    base1(Cs, Base, Next);
base1([C|Cs], Base, SoFar) when C >= $a, C =< $f, C < Base + $a - 10 ->
    Next = SoFar * Base + (C - $a + 10),
    base1(Cs, Base, Next);
base1([C|Cs], Base, SoFar) when C >= $A, C =< $F, C < Base + $A - 10 ->
    Next = SoFar * Base + (C - $A + 10),
    base1(Cs, Base, Next);
base1([C|Cs], _Base, SoFar) -> {SoFar,[C|Cs]};
base1([], _Base, N) -> {N,[]}.

%% chars(InputChars) -> Chars.
%% Convert an input string into the corresponding string
%% characters. We know that the input string is correct.

chars([$\\,$x,C|Cs0]) ->
    case hex_char(C) of
	true ->
	    case base1([C|Cs0], 16, 0) of
		{N,[$\\|Cs1]} -> [N|chars(Cs1)];
		_Other -> [escape_char($x)|chars([C|Cs0])]
	    end;
	false -> [escape_char($x)|chars([C|Cs0])]
    end;
chars([$\\,C|Cs0]) when C >= $0, C =< $7 ->
    case base1(Cs0, 8, C - $0) of
	{N,[$\\|Cs1]} -> [N|chars(Cs1)];
	_Other -> [escape_char(C)|chars(Cs0)]
    end;
chars([$\\,C|Cs]) -> [escape_char(C)|chars(Cs)];
chars([C|Cs]) -> [C|chars(Cs)];
chars([]) -> [].

hex_char(C) when C >= $0, C =< $9 -> true;
hex_char(C) when C >= $a, C =< $f -> true;
hex_char(C) when C >= $A, C =< $F -> true;
hex_char(_) -> false.

escape_char($n) -> $\n;				%\n = LF
escape_char($r) -> $\r;				%\r = CR
escape_char($t) -> $\t;				%\t = TAB
escape_char($v) -> $\v;				%\v = VT
escape_char($b) -> $\b;				%\b = BS
escape_char($f) -> $\f;				%\f = FF
escape_char($e) -> $\e;				%\e = ESC
escape_char($s) -> $\s;				%\s = SPC
escape_char($d) -> $\d;				%\d = DEL
escape_char(C) -> C.
