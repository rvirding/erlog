-module(graph_tests).
%% Copyright (c) 2014 Zachary Kessin
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

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




