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
  Now = erlog_et_logic:date_to_ts({M, S}),
  Value = to_integer(erlog_ec_support:deref(Var, Bs0)), %convert to integer, as it can be string, or binary.
  case erlog_ec_support:try_add(Now, Value, Bs0) of
    error -> erlog_errors:fail(Params);
    Bs -> erlog_ec_core:prove_body(Params#param{goal = Next, bindings = Bs})
  end;
%% Returns timestamp for data, ignoring time
prove_goal(Params = #param{goal = {date, DateString, Res}, next_goal = Next, bindings = Bs0}) ->
  {{Y, M, D}, _} = erlog_et_logic:date_string_to_data(erlog_et_logic:check_var(DateString, Bs0)),
  DataTS = erlog_et_logic:data_to_ts({{Y, M, D}, {0, 0, 0}}),
  Bs = erlog_ec_support:add_binding(Res, DataTS, Bs0),
  erlog_ec_core:prove_body(Params#param{goal = Next, bindings = Bs});
%% Returns timestamp for data, ignoring time
prove_goal(Params = #param{goal = {date, D, M, Y, Res}, next_goal = Next, bindings = Bs0}) ->
  DataTS = erlog_et_logic:data_to_ts({{erlog_et_logic:check_var(Y, Bs0), erlog_et_logic:check_var(M, Bs0), erlog_et_logic:check_var(D, Bs0)}, {0, 0, 0}}),
  Bs = erlog_ec_support:add_binding(Res, DataTS, Bs0),
  erlog_ec_core:prove_body(Params#param{goal = Next, bindings = Bs});
%% Returns timestamp for data, ignoring data.
prove_goal(Params = #param{goal = {time, TimeString, Res}, next_goal = Next, bindings = Bs0}) ->
  {_, {H, M, S}} = erlog_et_logic:date_string_to_data(erlog_et_logic:check_var(TimeString, Bs0)),  %cut YMD
  TS = S * erlog_et_logic:date_to_seconds(M, minute) * erlog_et_logic:date_to_seconds(H, hour),
  Bs = erlog_ec_support:add_binding(Res, TS, Bs0),
  erlog_ec_core:prove_body(Params#param{goal = Next, bindings = Bs});
%% Returns timestamp for data, ignoring data.
prove_goal(Params = #param{goal = {time, H, M, S, Res}, next_goal = Next, bindings = Bs0}) ->
  TS = erlog_et_logic:check_var(S, Bs0)
    * erlog_et_logic:date_to_seconds(erlog_et_logic:check_var(M, Bs0), minute)
    * erlog_et_logic:date_to_seconds(erlog_et_logic:check_var(H, Bs0), hour),
  Bs = erlog_ec_support:add_binding(Res, TS, Bs0),
  erlog_ec_core:prove_body(Params#param{goal = Next, bindings = Bs});
%% Calculates differense between two timestamps. Returns the result in specifyed format
prove_goal(Params = #param{goal = {date_diff, TS1, TS2, Format, Res}, next_goal = Next, bindings = Bs0}) ->
  Diff = timer:now_diff(erlog_et_logic:ts_to_date(erlog_et_logic:check_var(TS1, Bs0)), erlog_et_logic:ts_to_date(erlog_et_logic:check_var(TS2, Bs0))) / 1000000,
  Bs = erlog_ec_support:add_binding(Res, erlog_et_logic:seconds_to_date(Diff, erlog_et_logic:check_var(Format, Bs0)), Bs0),
  erlog_ec_core:prove_body(Params#param{goal = Next, bindings = Bs});
%% Adds number of seconds T2 in Type format to Time1. Returns timestamp
prove_goal(Params = #param{goal = {add_time, Time1, Type, T2, Res}, next_goal = Next, bindings = Bs0}) ->
  Diff = erlog_et_logic:check_var(Time1, Bs0) + erlog_et_logic:date_to_seconds(erlog_et_logic:check_var(T2, Bs0), erlog_et_logic:check_var(Type, Bs0)),
  Bs = erlog_ec_support:add_binding(Res, Diff, Bs0),
  erlog_ec_core:prove_body(Params#param{goal = Next, bindings = Bs});
%% Converts timestamp to human readable format
prove_goal(Params = #param{goal = {date_print, TS1, Res}, next_goal = Next, bindings = Bs0}) ->
  {{Year, Month, Day}, {Hour, Minute, Second}} = erlog_et_logic:date_to_data(erlog_et_logic:ts_to_date(erlog_et_logic:check_var(TS1, Bs0))),
  DateStr = lists:flatten(io_lib:format("~s ~2w ~4w ~2w:~2..0w:~2..0w", [?MONTH(Month), Day, Year, Hour, Minute, Second])),
  Bs = erlog_ec_support:add_binding(Res, DateStr, Bs0),
  erlog_ec_core:prove_body(Params#param{goal = Next, bindings = Bs});
%% Parses date string and returns timestamp.
prove_goal(Params = #param{goal = {date_parse, DataStr, Res}, next_goal = Next, bindings = Bs0}) ->
  Data = erlog_et_logic:date_string_to_data(erlog_et_logic:check_var(DataStr, Bs0)),
  Bs = erlog_ec_support:add_binding(Res, erlog_et_logic:data_to_ts(Data), Bs0),
  erlog_ec_core:prove_body(Params#param{goal = Next, bindings = Bs}).

%% @private
to_integer(V) when is_binary(V) -> binary_to_integer(V);
to_integer(V) when is_list(V) -> list_to_integer(V);
to_integer(V) -> V.