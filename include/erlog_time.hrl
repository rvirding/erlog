%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Июль 2014 11:19
%%%-------------------------------------------------------------------
-author("tihon").

-define(MONTHS, {"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
	"Aug", "Sep", "Oct", "Nov", "Dec"}).
-define(MONTH(X), element(X, ?MONTHS)).

-define(ERLOG_TIME,
	[
		{localtime, 1},
		{date_diff, 4},
		{add_time, 4},
		{date_print, 2},
		{date_parse, 2},
		{date, 2},
		{date, 4},
		{time, 2},
		{time, 4}
	]).