%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Июль 2014 11:20
%%%-------------------------------------------------------------------
-author("tihon").

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