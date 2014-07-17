%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Июль 2014 11:19
%%%-------------------------------------------------------------------
-author("tihon").

-define(ERLOG_LISTS,
	[
		{{append, 3}, ?MODULE, append_3},
		{{insert, 3}, ?MODULE, insert_3},
		{{member, 2}, ?MODULE, member_2},
		{{memberchk, 2}, ?MODULE, memberchk_2},
		{{reverse, 2}, ?MODULE, reverse_2},
		{{sort, 2}, ?MODULE, sort_2}
	]).