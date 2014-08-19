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
	lists:foreach(fun(Proc) -> erlog_memory:load_library_space(Db, Proc) end, ?ERLOG_CACHE).

put_2(Params = #param{goal = {put, _, _} = Goal, next_goal = Next, bindings = Bs}) ->
	{put, Key, Value} = ec_support:dderef(Goal, Bs),
	case get(erlog_cache) of
		undefined -> erlog_errors:fail(Params);
		Ets ->
			ets:insert(Ets, {Key, Value}),
			ec_core:prove_body(Params#param{goal = Next})
	end.

get_2(Params = #param{goal = {get, _, _} = Goal, next_goal = Next, bindings = Bs0}) ->
	{get, Key, Result} = ec_support:dderef(Goal, Bs0),
	case get(erlog_cache) of
		undefined -> erlog_errors:fail(Params);
		Ets ->
			case ets:lookup(Ets, Key) of
				[{_, Value}] ->
					Bs = ec_support:add_binding(Result, Value, Bs0),
					ec_core:prove_body(Params#param{goal = Next, bindings = Bs});
				[] -> erlog_errors:fail(Params)
			end
	end.