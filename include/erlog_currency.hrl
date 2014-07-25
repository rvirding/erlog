%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Июль 2014 20:06
%%%-------------------------------------------------------------------
-author("tihon").

-define(COURSE_URL, "https://api.privatbank.ua/p24api/pubinfo?jsonp&exchange&coursid=5").
-define(CHECK_PERIOD, 60000).


-define(ERLOG_CURRENCY,
	[
		{{exchange, 4}, ?MODULE, exchange_4}
	]
).

-record(currency,
{
	name,
	base_name,
	buy_course,
	sell_course
}).