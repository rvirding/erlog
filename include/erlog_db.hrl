%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Июль 2014 11:19
%%%-------------------------------------------------------------------
-author("tihon").

-define(ERLOG_DB,
	[
		{{db_abolish, 2}, ?MODULE, db_abolish_2},
		{{db_assert, 2}, ?MODULE, db_assert_2},
		{{db_asserta, 2}, ?MODULE, db_asserta_2},
		{{db_assertz, 2}, ?MODULE, db_assert_2},
		{{db_retract, 2}, ?MODULE, db_retract_2},
		{{db_retractall, 2}, ?MODULE, db_retractall_2}
	]
).