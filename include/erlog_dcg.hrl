%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Июль 2014 11:20
%%%-------------------------------------------------------------------
-author("tihon").

-define(ERLOG_DCG,
	[
		{{expand_term, 2}, erlog_dcg, expand_term_2},
		{{phrase, 3}, erlog_dcg, phrase_3}
	]).