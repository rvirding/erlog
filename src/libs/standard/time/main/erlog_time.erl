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

-behaviour(erlog_stdlib).

-include("erlog_core.hrl").
-include("erlog_time.hrl").

%% API
-export([load/1]).
-export([prove_goal/1]).

load(Db) ->
	lists:foreach(fun(Proc) -> erlog_memory:load_kernel_space(Db, ?MODULE, Proc) end, ?ERLOG_TIME).

%% Returns current timestamp.
prove_goal(Params = #param{goal = {localtime, Var}, next_goal = Next, bindings = Bs0}) ->
	{M, S, _} = os:timestamp(),
	Bs = ec_support:add_binding(Var, et_logic:date_to_ts({M, S}), Bs0),
	ec_core:prove_body(Params#param{goal = Next, bindings = Bs});
%% Returns timestamp for data, ignoring time
prove_goal(Params = #param{goal = {date, DateString, Res}, next_goal = Next, bindings = Bs0}) ->
	{{Y, M, D}, _} = et_logic:date_string_to_data(et_logic:check_var(DateString, Bs0)),
	DataTS = et_logic:data_to_ts({{Y, M, D}, {0, 0, 0}}),
	Bs = ec_support:add_binding(Res, DataTS, Bs0),
	ec_core:prove_body(Params#param{goal = Next, bindings = Bs});
%% Returns timestamp for data, ignoring time
prove_goal(Params = #param{goal = {date, D, M, Y, Res}, next_goal = Next, bindings = Bs0}) ->
	DataTS = et_logic:data_to_ts({{et_logic:check_var(Y, Bs0), et_logic:check_var(M, Bs0), et_logic:check_var(D, Bs0)}, {0, 0, 0}}),
	Bs = ec_support:add_binding(Res, DataTS, Bs0),
	ec_core:prove_body(Params#param{goal = Next, bindings = Bs});
%% Returns timestamp for data, ignoring data.
prove_goal(Params = #param{goal = {time, TimeString, Res}, next_goal = Next, bindings = Bs0}) ->
	{_, {H, M, S}} = et_logic:date_string_to_data(et_logic:check_var(TimeString, Bs0)),  %cut YMD
	TS = S * et_logic:date_to_seconds(M, minute) * et_logic:date_to_seconds(H, hour),
	Bs = ec_support:add_binding(Res, TS, Bs0),
	ec_core:prove_body(Params#param{goal = Next, bindings = Bs});
%% Returns timestamp for data, ignoring data.
prove_goal(Params = #param{goal = {time, H, M, S, Res}, next_goal = Next, bindings = Bs0}) ->
	TS = et_logic:check_var(S, Bs0)
		* et_logic:date_to_seconds(et_logic:check_var(M, Bs0), minute)
		* et_logic:date_to_seconds(et_logic:check_var(H, Bs0), hour),
	Bs = ec_support:add_binding(Res, TS, Bs0),
	ec_core:prove_body(Params#param{goal = Next, bindings = Bs});
%% Calculates differense between two timestamps. Returns the result in specifyed format
prove_goal(Params = #param{goal = {date_diff, TS1, TS2, Format, Res}, next_goal = Next, bindings = Bs0}) ->
	Diff = timer:now_diff(et_logic:ts_to_date(et_logic:check_var(TS1, Bs0)), et_logic:ts_to_date(et_logic:check_var(TS2, Bs0))) / 1000000,
	Bs = ec_support:add_binding(Res, et_logic:seconds_to_date(Diff, et_logic:check_var(Format, Bs0)), Bs0),
	ec_core:prove_body(Params#param{goal = Next, bindings = Bs});
%% Adds number of seconds T2 in Type format to Time1. Returns timestamp
prove_goal(Params = #param{goal = {add_time, Time1, Type, T2, Res}, next_goal = Next, bindings = Bs0}) ->
	Diff = et_logic:check_var(Time1, Bs0) + et_logic:date_to_seconds(et_logic:check_var(T2, Bs0), et_logic:check_var(Type, Bs0)),
	Bs = ec_support:add_binding(Res, Diff, Bs0),
	ec_core:prove_body(Params#param{goal = Next, bindings = Bs});
%% Converts timestamp to human readable format
prove_goal(Params = #param{goal = {date_print, TS1, Res}, next_goal = Next, bindings = Bs0}) ->
	{{Year, Month, Day}, {Hour, Minute, Second}} = et_logic:date_to_data(et_logic:ts_to_date(et_logic:check_var(TS1, Bs0))),
	DateStr = lists:flatten(io_lib:format("~s ~2w ~4w ~2w:~2..0w:~2..0w", [?MONTH(Month), Day, Year, Hour, Minute, Second])),
	Bs = ec_support:add_binding(Res, DateStr, Bs0),
	ec_core:prove_body(Params#param{goal = Next, bindings = Bs});
%% Parses date string and returns timestamp.
prove_goal(Params = #param{goal = {date_parse, DataStr, Res}, next_goal = Next, bindings = Bs0}) ->
	Data = et_logic:date_string_to_data(et_logic:check_var(DataStr, Bs0)),
	Bs = ec_support:add_binding(Res, et_logic:data_to_ts(Data), Bs0),
	ec_core:prove_body(Params#param{goal = Next, bindings = Bs}).