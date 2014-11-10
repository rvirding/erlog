%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Авг. 2014 18:10
%%%-------------------------------------------------------------------
-module(erlog_et_logic).
-author("tihon").

-include("erlog_time.hrl").

%% API
-export([date_to_ts/1, date_string_to_data/1, check_var/2, data_to_ts/1, date_to_seconds/2, seconds_to_date/2, date_to_data/1, ts_to_date/1]).

%% Time in microseconds, atom for output format
-spec seconds_to_date(Time :: integer(), atom()) -> integer().
seconds_to_date(Time, day) -> Time / 86400; % day = 24 hours
seconds_to_date(Time, hour) -> Time / 3600; % hour = 60 min
seconds_to_date(Time, minute) -> Time / 60; % min = 60 sec
seconds_to_date(Time, sec) -> Time.

%% Converts day|hour|minute to seconds
-spec date_to_seconds(integer(), atom()) -> integer().
date_to_seconds(Time, day) -> Time * 86400;
date_to_seconds(Time, hour) -> Time * 3600;
date_to_seconds(Time, minute) -> Time * 60;
date_to_seconds(Time, sec) -> Time.

%% Converts string date representation to timestamp. Format MM DD YYYY hh:mm:ss
-spec date_string_to_data(string()) -> tuple().
date_string_to_data(DataStr) ->
	[MStr, DStr, YStr, HStr, MnStr, SStr] = string:tokens(DataStr, " :"),
	Month = erlog_ec_support:index_of(MStr, tuple_to_list(?MONTHS)),
	{{list_to_integer(YStr), Month, list_to_integer(DStr)},
		{list_to_integer(HStr), list_to_integer(MnStr), list_to_integer(SStr)}}.

%% Converts data tuple to timestamp
-spec data_to_ts(tuple()) -> integer().
data_to_ts(Data) ->
	calendar:datetime_to_gregorian_seconds(Data) - 62167219200.

%% Converts data tuple to date tuple {{YYYY,MM,DD},{hh,mm,ss}}
-spec date_to_data(tuple()) -> tuple().
date_to_data(Ts) ->
	calendar:now_to_universal_time(Ts).

%% Converts data tuple (part of timestamp: MegaSecs, Secs) to integer seconds
-spec date_to_ts(tuple()) -> integer().
date_to_ts({M1, S1}) ->
	TimeStr = lists:concat([M1, S1]),
	list_to_integer(TimeStr).

%% Converts timestamp to data tuple
-spec ts_to_date(integer()) -> tuple().
ts_to_date(Timestamp) ->
	TSStr = integer_to_list(Timestamp),
	{M1, S1} = lists:split(4, TSStr),
	{list_to_integer(M1), list_to_integer(S1), 0}.

%% Checks - if var is normal, or binded, or < 0 (if int). Returns var's value.
check_var({'-', Var}, Bs) ->
	case check_var(Var, Bs) of
		Res when is_integer(Res) -> -1 * Res;
		Res -> Res
	end;
check_var({Var}, Bs) -> check_var(erlog_ec_support:deref({Var}, Bs), Bs);
check_var(Var, _) -> Var.