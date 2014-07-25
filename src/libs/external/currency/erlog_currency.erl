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
-export([load/1, exchange/2]).

load(Db) ->
	start_sync_if_needed(),
	lists:foreach(fun(Proc) -> erlog_memory:add_compiled_proc(Db, Proc) end, ?ERLOG_CURRENCY).

exchange({exchange, ValueFrom, CurrencyTypeFrom, ValueTo, CurrencyTypeTo}, Param = #param{next_goal = Next0,
	bindings = Bs, choice = Cps, database = Db, var_num = Vn}) ->

	From = ec_support:dderef(ValueFrom, Bs),
	To = ec_support:dderef(ValueTo, Bs),
	io:format("From ~p, To ~p~n", [From, To]),
	ok.


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
	ok = inets:start(),  %start inets if needed
	ok = application:start(crypto),
	ok = application:start(public_key),
	ok = application:start(ssl),
	catch erlog_curr_sup:start_link(),
	erlog_curr_sup:start_sync_worker().