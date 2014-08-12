%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Июль 2014 20:06
%%%-------------------------------------------------------------------
-module(erlog_currency).
-author("tihon").

-include("erlog_currency.hrl").
-include("erlog_core.hrl").

%% API
-export([load/1, exchange_4/2]).

load(Db) ->
	start_sync_if_needed(),
	lists:foreach(fun(Proc) -> erlog_memory:load_library_space(Db, Proc) end, ?ERLOG_CURRENCY).

exchange_4({exchange, _, _, _, _} = Goal, Params = #param{next_goal = Next, bindings = Bs0}) ->
	{exchange, From, CurrencyTypeFrom, To, CurrencyTypeTo} = ec_support:dderef(Goal, Bs0),
	Course = lists:concat(lists:sort([CurrencyTypeFrom, CurrencyTypeTo])),
	ResultCurrency = case erlog_curr_sync:get_course_by_curr(Course) of
		                 error -> erlog_errors:erlog_error("Unknown currency type!");
		                 {ok, Currency} ->
			                 case Currency#currency.name of
				                 CurrencyTypeFrom -> From * Currency#currency.buy_course;
				                 CurrencyTypeTo -> From / Currency#currency.sell_course;
				                 _ -> erlog_errors:erlog_error("Unknown currency type!")
			                 end
	                 end,
	Bs = ec_support:add_binding(To, ResultCurrency, Bs0),
	ec_core:prove_body(Params#param{goal = Next, bindings = Bs}).


%% @private
%% Starts erlog_curr_sync server if it is not started.
%% Makes monitor to it.
start_sync_if_needed() ->
	case check_server(whereis(erlog_curr_sync)) of
		undefined -> start_server();
		_ -> ok
	end.

%% @private
%% Checks if server is registered and running
check_server(undefined) -> undefined;
check_server(Pid) -> process_info(Pid).

%% @private
%% Starts supervisor and currency sync server
start_server() ->
	%start deps if not started
	catch inets:start(),
	catch application:start(crypto),
	catch application:start(asn1),
	catch application:start(public_key),
	catch application:start(ssl),

	catch erlog_curr_sup:start_link(),
	erlog_curr_sup:start_sync_worker().