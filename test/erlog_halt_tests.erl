-module(erlog_halt_tests).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

erlog_halt_test() ->
    {Pid,Ref} = spawn_monitor(fun() ->
				      {ok,Erlog} = erlog:new(),
				      erlog:prove({halt, test}, Erlog),
				      timer:sleep(300),
				      ok
				end),
    receive
	{'DOWN', Ref, process, Pid, R}  ->
	    ?assertEqual(test, R),
	    ok
    after 20 ->
	    ?debugVal("No Halt"),
	    ?assert(false)
    end.
