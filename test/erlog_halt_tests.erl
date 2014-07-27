-module(erlog_halt_tests).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

erlog_halt_test() ->
    {Pid,Ref} = spawn_monitor(fun() ->
				      Erlog = erlog:new(),
				      ?debugVal(Erlog({prove, {halt, test}})),
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
