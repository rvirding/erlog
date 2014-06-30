-module(erlog_file_tests).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

prop_consult_no_file() ->
    {ok, PID}    = erlog:start_link(),
    ?assertEqual({error,enoent}, erlog:consult(PID, "no_file.pl")),
    ?assertEqual({error,enoent}, erlog:reconsult(PID, "no_file.pl")),
    erlog:halt(PID),
    true.

prop_consult_with_file()->
    {ok, PID}    = erlog:start_link(),
    ?assertEqual(ok, erlog:consult(PID,   "test/graph.pl")),
    ?assertEqual(ok, erlog:reconsult(PID, "test/graph.pl")),
    erlog:halt(PID),
    true.
    
