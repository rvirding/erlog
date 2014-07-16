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
-export([load/1, localtime_1/2, datediff_4/2, dateadd_4/2]).

load(Db) ->
	lists:foreach(fun(Proc) -> erlog_memory:add_compiled_proc(Db, Proc) end, ?ERLOG_TIME).

localtime_1({localtime, Var}, Params = #param{next_goal = Next, bindings = Bs0}) ->
	{M, S, _} = os:timestamp(),
	Bs = ec_support:add_binding(Var, {M, S}, Bs0),
	ec_body:prove_body(Params#param{goal = Next, bindings = Bs}).

datediff_4({datediff, {M1, S1}, {M2, S2}, Format, Res}, Params = #param{next_goal = Next, bindings = Bs0}) ->
	Diff = timer:now_diff({M1, S1, 0}, {M2, S2, 0}),
	Bs = ec_support:add_binding(Res, microseconds_to_date(Diff, Format), Bs0),
	ec_body:prove_body(Params#param{goal = Next, bindings = Bs}).

dateadd_4({dateadd, Time1, Type, T2, Res}, Params = #param{next_goal = Next, bindings = Bs0}) ->
	Ts1 = date_to_ts(Time1),
	Diff = Ts1 + date_to_seconds(T2, Type),
	Bs = ec_support:add_binding(Res, Diff, Bs0),
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
