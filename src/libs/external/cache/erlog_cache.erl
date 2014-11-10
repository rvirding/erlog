%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc  local cache library. Includes functions for operating data in ets,
%%% tied to calling process (assuming calling process is erlog gen_server).
%%%
%%% @end
%%% Created : 18. Авг. 2014 21:46
%%%-------------------------------------------------------------------
-module(erlog_cache).
-author("tihon").

-behaviour(erlog_exlib).

-include("erlog_core.hrl").
-include("erlog_cache.hrl").

%% API
-export([load/1,
	put_2/1,
	get_2/1]).

load(Db) ->
	case get(erlog_cache) of
		undefined ->
			Ets = ets:new(erlog_cache, []),
			put(erlog_cache, Ets);
		_ -> ok
	end,
	lists:foreach(fun(Proc) -> erlog_memory:load_native_library(Db, Proc) end, ?ERLOG_CACHE).

put_2(Params = #param{goal = {put, _, _} = Goal, next_goal = Next, bindings = Bs}) ->
	{put, Key, Value} = erlog_ec_support:dderef(Goal, Bs),
	case erlog_ec_support:is_bound(Value) of %Value must exists
		true -> case get(erlog_cache) of
			        undefined -> erlog_errors:fail(Params);
			        Ets ->
				        ets:insert(Ets, {Key, Value}),
				        erlog_ec_core:prove_body(Params#param{goal = Next})
		        end;
		false -> erlog_errors:fail(Params)
	end.

get_2(Params = #param{goal = {get, _, _} = Goal, bindings = Bs}) ->
	{get, Key, Result} = erlog_ec_support:dderef(Goal, Bs),
	case get(erlog_cache) of
		undefined -> erlog_errors:fail(Params);
		Ets -> check_value(ets:lookup(Ets, Key), Result, Params)
	end.


%% @private
check_value([], _, Params) -> erlog_errors:fail(Params);
check_value([{_, Value}], Result, Params = #param{next_goal = Next, bindings = Bs0}) ->
	case erlog_ec_support:is_bound(Result) of
		true -> %compare value from cache with result
			if Result == Value -> erlog_ec_core:prove_body(Params#param{goal = Next});
				true -> erlog_errors:fail(Params)
			end;
		false ->  %save value from cache to result
			Bs = erlog_ec_support:add_binding(Result, Value, Bs0),
			erlog_ec_core:prove_body(Params#param{goal = Next, bindings = Bs})
	end.

