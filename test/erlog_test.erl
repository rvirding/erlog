%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Июль 2014 21:46
%%%-------------------------------------------------------------------
-module(erlog_test).
-author("tihon").

-include_lib("eunit/include/eunit.hrl").

run_all_test() ->
	Names = get_absolute_names(get_prolog_files()),
	?debugMsg(Names),
	lists:foreach(fun run_one/1, Names).

-spec run_one(File :: string()) -> ok.
run_one(File) ->
	{ok, ErlogWorker} = erlog:start_link(),
	?debugMsg(File),
	Res = erlog:execute(ErlogWorker, string:join(["consult(", File, ")."], "\"")),
	?debugMsg(Res),
	?assertEqual(<<"Yes">>, Res),
	Res1 = erlog:execute(ErlogWorker, "test_all."),
	?debugMsg(Res1),
	?assertEqual(<<"Yes">>, Res1),
	ok.

get_absolute_names(FileNames) ->
	lists:foldl(fun(Name, Acc) -> [filename:absname("test/prolog/" ++ Name) | Acc] end, [], FileNames).

-spec get_prolog_files() -> list().
get_prolog_files() ->
	{ok, FileNames} = file:list_dir("test/prolog"),
	FileNames.