-module(erlog_file_tests).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("erlog_test.hrl").
-compile(export_all).


consult_no_file_test() ->
    {ok, ERLOG}    = erlog:new(),
    ?assertMatch({error,enoent}, erlog:consult(ERLOG, "no_file.pl")),
    ?assertMatch({error,enoent}, erlog:reconsult(ERLOG, "no_file.pl")),
   
    true.

consult_with_file_test()->
    {ok, ERLOG}    = erlog:new(),
    {ok, ERLOG1} =  erlog:consult(ERLOG,   "../test/graph.pl"),
    {ok, ERLOG2} =  erlog:reconsult(ERLOG1, "../test/graph.pl"),
    ?assert(is_record(ERLOG2,est)),
    true.
    
