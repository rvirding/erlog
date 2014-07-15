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
-export([load/1, localtime_1/2, datediff_4/2]).

load(Db) ->
	lists:foreach(fun(Proc) -> erlog_memory:add_compiled_proc(Db, Proc) end, ?ERLOG_TIME).

localtime_1({localtime, Var}, Params = #param{next_goal = Next, bindings = Bs0}) ->
	{M, S, _} = os:timestamp(),
	Bs = ec_support:add_binding(Var, {M, S}, Bs0),
	ec_body:prove_body(Params#param{goal = Next, bindings = Bs}).

datediff_4({datediff, {M1, S1}, {M2, S2}, Format, Res}, Params = #param{next_goal = Next, bindings = Bs0}) ->
	Diff = timer:now_diff({M1, S1, 0}, {M2, S2, 0}),
	Bs = ec_support:add_binding(Res, form_output(Diff, Format), Bs0),
	ec_body:prove_body(Params#param{goal = Next, bindings = Bs}).

%% @private
%% Time in microseconds, atom for output format
-spec form_output(Time :: integer(), atom()) -> integer().
form_output(Time, day) -> Time / 86400000000; % day = 24 hours
form_output(Time, hour) -> Time / 3600000000; % hour = 60 min
form_output(Time, minute) -> Time / 60000000; % min = 60 sec
form_output(Time, sec) -> Time / 1000000. % micro = 10^-6