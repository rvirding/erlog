-module(erlog_dcg_tests).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-include("erlog_test.hrl").

finite_dcg_test() ->
    {ok, ERLOG_STATE}		= erlog:new(),
    {ok, ERLOG_STATE1}		= erlog:consult(ERLOG_STATE,"../test/finite_dcg.pl"),
    {{succeed,_},ERLOG_STATE2}	= erlog:prove(ERLOG_STATE1,{s,[the,woman,shoots,the,man],[]}),
    {{succeed,_},ERLOG_STATE3}	= erlog:prove(ERLOG_STATE2,{s,[the,man,shoots,a,man],[]}),
    case erlog:prove(ERLOG_STATE3, {s, {'X'},[]}) of
	{{succeed, [{'X',List}]},_ERLOG_STATE4} ->
	    ?assertEqual([the,woman,shoots,the,woman], List);
	fail ->
	    false
    end.


