-module(erlog_ets_tests).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").


    
     
gnode() ->
    {edge, char(),char()}.

gnodes() ->
    non_empty(list(gnode())).

prop_load_ets() ->
    ?FORALL({Nodes},
            {gnodes()},
            begin
                {ok, PID}   = erlog:start_link(),
		ok = erlog:load(PID,erlog_ets),
		TabId = ets:new(test_ets_table, [bag]),
		ets:insert(TabId, Nodes),

                case  erlog:prove(PID, {ets_match, TabId, {'X'}}) of
                    {succeed,[{'X', M}]} -> 
			lists:member(M,Nodes);
                    R           -> 
			false
                end
            end).


out(P) ->
   on_output(fun(S,F) -> io:format(user, S, F) end,P).

run_test_() ->
    Props = [
	     fun prop_load_ets/0
             ],    
    [
     begin
         P = out(Prop()),
         ?_assert(quickcheck(numtests(500,P)))
     end
     || Prop <- Props].

