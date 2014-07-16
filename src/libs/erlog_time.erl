%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Июль 2014 0:27
%%%-------------------------------------------------------------------
-module(erlog_time).
-author("tihon").

-include("erlog_int.hrl").

%% API
-export([load/1, localtime_1/2, datediff_4/2, dateadd_4/2, dateprint_2/2, dateparse_2/2]).

load(Db) ->
	lists:foreach(fun(Proc) -> erlog_memory:add_compiled_proc(Db, Proc) end, ?ERLOG_TIME).

%% Returns current time in date tuple.
localtime_1({localtime, Var}, Params = #param{next_goal = Next, bindings = Bs0}) ->
	{M, S, _} = os:timestamp(),
	Bs = ec_support:add_binding(Var, {M, S}, Bs0),
	ec_body:prove_body(Params#param{goal = Next, bindings = Bs}).

%% Calculates differense between two date tuples. Returns the result in specifyed format
datediff_4({datediff, {M1, S1}, {M2, S2}, Format, Res}, Params = #param{next_goal = Next, bindings = Bs0}) ->
	Diff = timer:now_diff({M1, S1, 0}, {M2, S2, 0}),
	Bs = ec_support:add_binding(Res, microseconds_to_date(Diff, Format), Bs0),
	ec_body:prove_body(Params#param{goal = Next, bindings = Bs}).

%% Adds number of seconds T2 in Type format to Time1. Returns the result in Type format
dateadd_4({dateadd, Time1, Type, T2, Res}, Params = #param{next_goal = Next, bindings = Bs0}) ->
	Ts1 = date_to_ts(Time1),
	Diff = Ts1 + date_to_seconds(T2, Type),
	Bs = ec_support:add_binding(Res, microseconds_to_date(Diff * 1000000, Type), Bs0),
	ec_body:prove_body(Params#param{goal = Next, bindings = Bs}).

%% Converts date tuple to human readable format
dateprint_2({dateprint, {M, N}, Res}, Params = #param{next_goal = Next, bindings = Bs0}) ->
	{{Year, Month, Day}, {Hour, Minute, Second}} = calendar:now_to_universal_time({M, N, 0}),
	DateStr = lists:flatten(io_lib:format("~2w ~2..0w ~4w ~2w:~2..0w:~2..0w", [Day, Month, Year, Hour, Minute, Second])),
	Bs = ec_support:add_binding(Res, DateStr, Bs0),
	ec_body:prove_body(Params#param{goal = Next, bindings = Bs}).

%% Parses date string and returnsdata tuple.
dateparse_2({dateparse, DataStr, Res}, Params = #param{next_goal = Next, bindings = Bs0}) ->
	[DStr, MStr, YStr, HStr, MnStr, SStr] = string:tokens(DataStr, " :"),
	Data = {{list_to_integer(YStr), list_to_integer(MStr), list_to_integer(DStr)},
		{list_to_integer(HStr), list_to_integer(MnStr), list_to_integer(SStr)}},
	Seconds = calendar:datetime_to_gregorian_seconds(Data) - 62167219200,
	Ts = {Seconds div 1000000, Seconds rem 1000000},
	Bs = ec_support:add_binding(Res, Ts, Bs0),
	ec_body:prove_body(Params#param{goal = Next, bindings = Bs}).

%% @private
%% Time in microseconds, atom for output format
-spec microseconds_to_date(Time :: integer(), atom()) -> integer().
microseconds_to_date(Time, day) -> Time / 86400000000; % day = 24 hours
microseconds_to_date(Time, hour) -> Time / 3600000000; % hour = 60 min
microseconds_to_date(Time, minute) -> Time / 60000000; % min = 60 sec
microseconds_to_date(Time, sec) -> Time / 1000000. % micro = 10^-6

%% @private
%% Converts day|hour|minute to seconds
-spec date_to_seconds(integer(), atom()) -> integer().
date_to_seconds(Time, day) -> Time * 86400;
date_to_seconds(Time, hour) -> Time * 3600;
date_to_seconds(Time, minute) -> Time * 60;
date_to_seconds(Time, sec) -> Time.

%% @private
%% Converts part of timestamp (MegaSecs, Secs) to integer seconds
-spec date_to_ts(tuple()) -> integer().
date_to_ts({M1, S1}) ->
	TimeStr = lists:concat([M1, S1]),
	list_to_integer(TimeStr).
