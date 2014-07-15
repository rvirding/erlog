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
-export([load/1, localtime_1/2]).

load(Db) ->
	lists:foreach(fun(Proc) -> erlog_memory:add_compiled_proc(Db, Proc) end, ?ERLOG_TIME).

localtime_1({localtime, Var}, Params = #param{next_goal = Next, bindings = Bs0}) ->
	{M, S, _} = os:timestamp(),
	Bs = erlog_core:add_binding(Var, {M, S}, Bs0),
	erlog_core:prove_body(Params#param{goal = Next, bindings = Bs}).