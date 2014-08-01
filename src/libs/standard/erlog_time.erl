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

-include("erlog_core.hrl").
-include("erlog_time.hrl").

%% API
-export([load/1, localtime_1/2]).
-export([date_2/2, date_4/2, time_2/2, time_4/2]).
-export([datediff_4/2, add_time_4/2, dateprint_2/2, dateparse_2/2]).

load(Db) ->
	lists:foreach(fun(Proc) -> erlog_memory:add_compiled_proc(Db, Proc) end, ?ERLOG_TIME).

%% Returns current timestamp.
localtime_1({localtime, Var}, Params = #param{next_goal = Next, bindings = Bs0}) ->
	{M, S, _} = os:timestamp(),
	Bs = ec_support:add_binding(Var, date_to_ts({M, S}), Bs0),
	ec_body:prove_body(Params#param{goal = Next, bindings = Bs}).

%% Returns timestamp for data, ignoring time
date_2({date, DateString, Res}, Params = #param{next_goal = Next, bindings = Bs0}) ->
	{{Y, M, D}, _} = date_string_to_data(check_var(DateString, Bs0)),
	DataTS = data_to_ts({{Y, M, D}, {0, 0, 0}}),
	Bs = ec_support:add_binding(Res, DataTS, Bs0),
	ec_body:prove_body(Params#param{goal = Next, bindings = Bs}).

%% Returns timestamp for data, ignoring time
date_4({date, D, M, Y, Res}, Params = #param{next_goal = Next, bindings = Bs0}) ->
	DataTS = data_to_ts({{check_var(Y, Bs0), check_var(M, Bs0), check_var(D, Bs0)}, {0, 0, 0}}),
	Bs = ec_support:add_binding(Res, DataTS, Bs0),
	ec_body:prove_body(Params#param{goal = Next, bindings = Bs}).

%% Returns timestamp for data, ignoring data.
time_2({time, TimeString, Res}, Params = #param{next_goal = Next, bindings = Bs0}) ->
	{_, {H, M, S}} = date_string_to_data(check_var(TimeString, Bs0)),  %cut YMD
	TS = S * date_to_seconds(M, minute) * date_to_seconds(H, hour),
	Bs = ec_support:add_binding(Res, TS, Bs0),
	ec_body:prove_body(Params#param{goal = Next, bindings = Bs}).

%% Returns timestamp for data, ignoring data.
time_4({time, H, M, S, Res}, Params = #param{next_goal = Next, bindings = Bs0}) ->
	TS = check_var(S, Bs0) * date_to_seconds(check_var(M, Bs0), minute) * date_to_seconds(check_var(H, Bs0), hour),
	Bs = ec_support:add_binding(Res, TS, Bs0),
	ec_body:prove_body(Params#param{goal = Next, bindings = Bs}).

%% Calculates differense between two timestamps. Returns the result in specifyed format
datediff_4({date_diff, TS1, TS2, Format, Res}, Params = #param{next_goal = Next, bindings = Bs0}) ->
	Diff = timer:now_diff(ts_to_date(check_var(TS1, Bs0)), ts_to_date(check_var(TS2, Bs0))) / 1000000,
	Bs = ec_support:add_binding(Res, seconds_to_date(Diff, check_var(Format, Bs0)), Bs0),
	ec_body:prove_body(Params#param{goal = Next, bindings = Bs}).

%% Adds number of seconds T2 in Type format to Time1. Returns timestamp
add_time_4({add_time, Time1, Type, T2, Res}, Params = #param{next_goal = Next, bindings = Bs0}) ->
	Diff = check_var(Time1, Bs0) + date_to_seconds(check_var(T2, Bs0), check_var(Type, Bs0)),
	Bs = ec_support:add_binding(Res, Diff, Bs0),
	ec_body:prove_body(Params#param{goal = Next, bindings = Bs}).

%% Converts timestamp to human readable format
dateprint_2({date_print, TS1, Res}, Params = #param{next_goal = Next, bindings = Bs0}) ->
	{{Year, Month, Day}, {Hour, Minute, Second}} = date_to_data(ts_to_date(check_var(TS1, Bs0))),
	DateStr = lists:flatten(io_lib:format("~s ~2w ~4w ~2w:~2..0w:~2..0w", [?MONTH(Month), Day, Year, Hour, Minute, Second])),
	Bs = ec_support:add_binding(Res, DateStr, Bs0),
	ec_body:prove_body(Params#param{goal = Next, bindings = Bs}).

%% Parses date string and returns timestamp.
dateparse_2({date_parse, DataStr, Res}, Params = #param{next_goal = Next, bindings = Bs0}) ->
	Data = date_string_to_data(check_var(DataStr, Bs0)),
	Bs = ec_support:add_binding(Res, data_to_ts(Data), Bs0),
	ec_body:prove_body(Params#param{goal = Next, bindings = Bs}).

%% @private
%% Time in microseconds, atom for output format
-spec seconds_to_date(Time :: integer(), atom()) -> integer().
seconds_to_date(Time, day) -> Time / 86400; % day = 24 hours
seconds_to_date(Time, hour) -> Time / 3600; % hour = 60 min
seconds_to_date(Time, minute) -> Time / 60; % min = 60 sec
seconds_to_date(Time, sec) -> Time.

%% @private
%% Converts day|hour|minute to seconds
-spec date_to_seconds(integer(), atom()) -> integer().
date_to_seconds(Time, day) -> Time * 86400;
date_to_seconds(Time, hour) -> Time * 3600;
date_to_seconds(Time, minute) -> Time * 60;
date_to_seconds(Time, sec) -> Time.

%% @private
%% Converts string date representation to timestamp. Format MM DD YYYY hh:mm:ss
-spec date_string_to_data(string()) -> tuple().
date_string_to_data(DataStr) ->
	[MStr, DStr, YStr, HStr, MnStr, SStr] = string:tokens(DataStr, " :"),
	Month = ec_support:index_of(MStr, tuple_to_list(?MONTHS)),
	{{list_to_integer(YStr), Month, list_to_integer(DStr)},
		{list_to_integer(HStr), list_to_integer(MnStr), list_to_integer(SStr)}}.

%% @private
%% Converts data tuple to timestamp
-spec data_to_ts(tuple()) -> integer().
data_to_ts(Data) ->
	calendar:datetime_to_gregorian_seconds(Data) - 62167219200.

%% @private
%% Converts data tuple to date tuple {{YYYY,MM,DD},{hh,mm,ss}}
-spec date_to_data(tuple()) -> tuple().
date_to_data(Ts) ->
	calendar:now_to_universal_time(Ts).

%% @private
%% Converts data tuple (part of timestamp: MegaSecs, Secs) to integer seconds
-spec date_to_ts(tuple()) -> integer().
date_to_ts({M1, S1}) ->
	TimeStr = lists:concat([M1, S1]),
	list_to_integer(TimeStr).

%% @private
%% Converts timestamp to data tuple
-spec ts_to_date(integer()) -> tuple().
ts_to_date(Timestamp) ->
	TSStr = integer_to_list(Timestamp),
	{M1, S1} = lists:split(4, TSStr),
	{list_to_integer(M1), list_to_integer(S1), 0}.


%% @private
%% Checks - if var is normal, or binded, or < 0 (if int). Returns var's value.
check_var({'-', Var}, Bs) ->
	case check_var(Var, Bs) of
		Res when is_integer(Res) -> -1 * Res;
		Res -> Res
	end;
check_var(Var, Bs) -> check_var(ec_support:deref({Var}, Bs), Bs).