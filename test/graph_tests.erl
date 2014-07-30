-module(graph_tests).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-include("erlog_test.hrl").


partially_ordered_set_test() ->
    {ok, ERLOG}   =                               erlog:new(),
    {ok, ERLOG1 }         =                        erlog:consult(ERLOG, "../test/po_set.pl"),    
    ?assertMatch({{succeed, []},#est{}},                 erlog:prove(ERLOG1, {connected, a, b})),
    ?assertMatch({fail,#est{}},                          erlog:prove(ERLOG1, {connected, b,c})),
    ?assertMatch({{succeed, []},#est{}},                 erlog:prove(ERLOG1, {ancestor, a, f})),
    ?assertMatch({{succeed, [{'Ancestor', d}]},#est{}},  erlog:prove(ERLOG1, {ancestor, {'Ancestor'}, f})),
 %   ?assertMatch({{succeed, [{'Ancestor', b}]},#est{}},  erlog:next_solution(ERLOG2)),
    ?assertMatch({{succeed, [{'p', [a,b,f]}]}, #est{}},  erlog:prove(ERLOG1,{path, a, f, {p}})),
%    ?assertMatch({{succeed, [{'p', [a,c,d,f]}]}, #est{}},erlog:next_solution(ERLOG3)),
    true.

gnode() ->
    {edge, char(),char()}.

gnodes() ->
    non_empty(list(gnode())).


prop_travel() ->
    ?FORALL({Nodes},
	    {gnodes()},
	    begin
		{ok,E}   = erlog:new(),
		{ok,E1}  = erlog:consult(E, "../test/graph.pl"),
		E2  = lists:foldr(fun(Node, EI) ->
					  {{succeed, _},E2} = erlog:prove(EI, {assertz,Node}),
					  E2
				  end, E1,Nodes),
		
		true = lists:all(fun({edge,Start,_})->
					 {{succeed, R},_}  = erlog:prove(E2, {path, Start, {'End'},{'Path'}}),
					 End  = proplists:get_value('End',  R),
					 Path = proplists:get_value('Path', R),
					 {{succeed, []},_} = erlog:prove(E2, {path, Start, End, Path}),
					 true
				 end, Nodes)
	    end).




