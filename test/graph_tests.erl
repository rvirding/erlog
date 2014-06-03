-module(graph_tests).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").

gnode() ->
    {edge, char(),char()}.

gnodes() ->
    non_empty(list(gnode())).

prop_travel() ->
        ?FORALL({Nodes},
            {gnodes()},
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
		end).


out(P) ->
   on_output(fun(S,F) -> io:format(user, S, F) end,P).

run_test_() ->
    Props = [
	     fun prop_travel/0
             ],    
    [
     begin
         P = out(Prop()),
         ?_assert(quickcheck(numtests(250,P)))
     end
     || Prop <- Props].

