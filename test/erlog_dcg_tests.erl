-module(erlog_dcg_tests).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-include("erlog_test.hrl").

finite_dcg_test() ->
    {ok, PID}   = erlog:start_link(),
    ok = erlog:consult(PID,"../test/finite_dcg.pl"),
    {succeed,_} = erlog:prove(PID,{s,[the,woman,shoots,the,man],[]}),
    {succeed,_} = erlog:prove(PID,{s,[the,man,shoots,a,man],[]}),
    case erlog:prove(PID, {s, {'X'},[]}) of
	{succeed, [{'X',List}]} ->
	    [the,woman,shoots,the,woman] =:= List;
	fail ->
	    false
    end.


%%DO SOMETHING HERE
prop_infinite_dcg() ->   
    true.
