%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Июль 2014 16:29
%%%-------------------------------------------------------------------
-module(ec_term).
-author("tihon").

%% API
-export([term_instance/2, term_instance/3]).

%% term_instance(Term, VarNum) -> {Term,NewRepls,NewVarNum}.
%% term_instance(Term, Repls, VarNum) -> {Term,NewRepls,NewVarNum}.
%%  Generate a copy of a term with new, fresh unused variables. No
%%  bindings from original variables to new variables. It can handle
%%  replacing integer variables with overlapping integer ranges. Don't
%%  check Term as it should already be checked. Use orddict as there
%%  will seldom be many variables and it it fast to setup.
term_instance(A, Vn) -> term_instance(A, orddict:new(), Vn).

term_instance([], Rs, Vn) -> {[], Rs, Vn};
term_instance([H0 | T0], Rs0, Vn0) ->
	{H, Rs1, Vn1} = term_instance(H0, Rs0, Vn0),
	{T, Rs2, Vn2} = term_instance(T0, Rs1, Vn1),
	{[H | T], Rs2, Vn2};
term_instance({'_'}, Rs, Vn) -> {{Vn}, Rs, Vn + 1};  %Unique variable
term_instance({V0}, Rs0, Vn0) ->    %Other variables
	case orddict:find(V0, Rs0) of
		{ok, V1} -> {V1, Rs0, Vn0};
		error ->
			V1 = {Vn0},
			{V1, orddict:store(V0, V1, Rs0), Vn0 + 1}
	end;
%% Special case some smaller structures.
term_instance({Atom, Arg}, Rs0, Vn0) ->
	{CopyArg, Rs1, Vn1} = term_instance(Arg, Rs0, Vn0),
	{{Atom, CopyArg}, Rs1, Vn1};
term_instance({Atom, A1, A2}, Rs0, Vn0) ->
	{CopyA1, Rs1, Vn1} = term_instance(A1, Rs0, Vn0),
	{CopyA2, Rs2, Vn2} = term_instance(A2, Rs1, Vn1),
	{{Atom, CopyA1, CopyA2}, Rs2, Vn2};
term_instance(T, Rs0, Vn0) when is_tuple(T) ->
	As0 = tl(tuple_to_list(T)),
	{As1, Rs1, Vn1} = term_instance(As0, Rs0, Vn0),
	{list_to_tuple([element(1, T) | As1]), Rs1, Vn1};
term_instance(A, Rs, Vn) -> {A, Rs, Vn}.    %Constant