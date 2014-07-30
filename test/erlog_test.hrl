-include("../src/erlog_int.hrl").

get_quickcheck_properties() ->
    Funs  = ?MODULE:module_info(functions),
    Funs1 = [P || {P, 0} <- Funs],
    Props = lists:filter(fun(Fun) ->
				 FnName = atom_to_list(Fun),
				 "prop_" =:= string:sub_string(FnName, 1,5)
			 end, Funs1),
    Props.

   
run_quickcheck_properties_test_() ->
    run_quickcheck(get_quickcheck_properties()).

run_quickcheck(Tests) ->
    run_quickcheck(Tests,100).

run_quickcheck(Tests, _Count) ->
    begin
	[begin
	     P1 = ?MODULE:Prop(),
	     P2 = out(P1),
	     ?_assert(eqc:quickcheck(P2))
	 end  || Prop<-Tests]
    end.





%--------------------------------------------------------------------------------

out(P) ->
   on_output(fun(S,F) -> io:format(user, S, F) end,P).
