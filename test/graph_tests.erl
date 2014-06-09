-module(graph_tests).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
partially_ordered_set_test() ->
    {ok, PID}   =				erlog:start_link(),
    ok          =				erlog:consult(PID, "../test/po_set.pl"),    
    ?assertEqual({succeed, []},			erlog:prove(PID, {connected, a, b})),
    ?assertEqual(fail,				erlog:prove(PID, {connected, b,c})),
    ?assertEqual({succeed, []},			erlog:prove(PID, {ancestor, a, f})),
    ?assertEqual({succeed, [{'Ancestor', d}]},	erlog:prove(PID, {ancestor, {'Ancestor'}, f})),
    ?assertEqual({succeed, [{'Ancestor', b}]},	erlog:next_solution(PID)),
    ?assertEqual({succeed, [{'p', [a,b,f]}]},	erlog:prove(PID,{path, a, f, {p}})),
    ?assertEqual({succeed, [{'p', [a,c,d,f]}]}, erlog:next_solution(PID)),
    ok.

gnode() ->
    {edge, char(),char()}.

gnodes() ->
    non_empty(list(gnode())).

%%TODO ADD TIMEOUT HERE
prop_travel() ->
        ?FORALL({Nodes},
		{gnodes()},
		?TIMEOUT(1000,
			 begin
			     {ok, PID}   = erlog:start_link(),
			     ok          = erlog:consult(PID, "../test/graph.pl"),
			     [erlog:prove(PID, {assertz, Node})|| Node <- Nodes],
			     
			     true = lists:all(fun({edge,Start,_})->
						      {succeed, R} = erlog:prove(PID, {path, Start, {'End'},{'Path'}}),
						      End  = proplists:get_value('End',  R),
						      Path = proplists:get_value('Path', R ),
						      {succeed, []} =:= erlog:prove(PID, {path, Start, End, Path})
					      
					      end, Nodes)
			 end)).


out(P) ->
   on_output(fun(S,F) -> io:format(user, S, F) end,P).

run_test_() ->
    Props = [
	     fun prop_travel/0
             ],    
    [
     begin
         P = out(Prop()),
         ?_assert(quickcheck(numtests(500,P)))
     end
     || Prop <- Props].

