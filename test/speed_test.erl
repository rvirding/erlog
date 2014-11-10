%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Сент. 2014 23:51
%%%-------------------------------------------------------------------
-module(speed_test).
-author("tihon").

-include_lib("eunit/include/eunit.hrl").

dict_test() ->
	Worker = create_and_load(erlog_dict),
	Before = os:timestamp(),
	Res = erlog:execute(Worker, "test_all."),
	?debugMsg(Res),
	?assertEqual(true, Res),
	After = os:timestamp(),
	?debugFmt("run dict_test for ~p seconds~n", [timer:now_diff(After, Before) / 1000000]).

ets_test() ->
	Worker = create_and_load(erlog_ets),
	Before = os:timestamp(),
	Res = erlog:execute(Worker, "test_all."),
	?debugMsg(Res),
	?assertEqual(true, Res),
	After = os:timestamp(),
	?debugFmt("run ets_test for ~p seconds~n", [timer:now_diff(After, Before) / 1000000]).

create_and_load(Module) ->
	{ok, ErlogWorker} = erlog:start_link([{database, Module}]),
	Res = erlog:execute(ErlogWorker, string:join(["consult(", filename:absname("test/" ++ "hanoy.pl"), ")."], "\"")),
	?debugMsg(Res),
	?assertEqual(true, Res),
	ErlogWorker.
