-module(graph_tests).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).



prop_partially_ordered_set() ->
    {ok, PID}   =				erlog:start_link(),
    ok          =				erlog:consult(PID, "test/po_set.pl"),    
    ?assertEqual({succeed, []},			erlog:prove(PID, {connected, a, b})),
    ?assertEqual(fail,				erlog:prove(PID, {connected, b,c})),
    ?assertEqual({succeed, []},			erlog:prove(PID, {ancestor, a, f})),
    ?assertEqual({succeed, [{'Ancestor', d}]},	erlog:prove(PID, {ancestor, {'Ancestor'}, f})),
    ?assertEqual({succeed, [{'Ancestor', b}]},	erlog:next_solution(PID)),
    ?assertEqual({succeed, [{'p', [a,b,f]}]},	erlog:prove(PID,{path, a, f, {p}})),
    ?assertEqual({succeed, [{'p', [a,c,d,f]}]}, erlog:next_solution(PID)),
    true.

gnode() ->
    {edge, char(),char()}.

gnodes() ->
    non_empty(list(gnode())).


prop_travel() ->
        ?FORALL({Nodes},
		{gnodes()},
		
		begin
		    E   = erlog:new(),
		    {ok,E1}  = E({consult, "test/graph.pl"}),
		    E2  = lists:foldr(fun(Node, EI) ->
					      {{succeed, _},E2} = EI({prove, {assertz,Node}}),
					      E2
				      end, E1,Nodes),

		    true = lists:all(fun({edge,Start,_})->
					     {{succeed, R},_}  = E2({prove, {path, Start, {'End'},{'Path'}}}),
					     End  = proplists:get_value('End',  R),
					     Path = proplists:get_value('Path', R),
					     {{succeed, []},_} = E2({prove, {path, Start, End, Path}}),
					     true
				     end, Nodes)
			 end).




