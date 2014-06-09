-module(erlog_bips_tests).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
cops() ->
    oneof([{'=:=', fun (I,J) ->
			   I == J
		   end},
	   {'=\\=', fun(I,J) ->
			   I /= J
		   end},
	   {'<', fun(I, J) ->
			 I < J
		 end},
	   {'>', fun(I,J) ->
			 I > J
		 end},
	   {'>=', fun(I, J) ->
			  I >= J
		  end},
	   {'=<', fun(I,J) ->
			  I =< J
		  end}]).

prop_comp() ->
    ?FORALL({I, J, {Op,C}},
	    {oneof([int(),real()]), int(), cops()},
	    begin
                {ok, PID}    = erlog:start_link(),
                case erlog:prove(PID, {Op, I, J}) of
		    {succeed, _} -> 
			C(I,J);
		    fail ->
			not(C(I,J))
		    end
		end).


prop_equals() ->
    ?FORALL(I, int(),
            begin
                {ok, PID}    = erlog:start_link(),
                {succeed, [{'X',I}]} =:= erlog:prove(PID, {'=', I, {'X'}})
            end).
prop_not_equals() ->
    ?FORALL(I, int(),
            begin
                {ok, PID}    = erlog:start_link(),
                fail =:= erlog:prove(PID, {'=', I, I + 1})
            end).

prop_float()->
    ?FORALL(I,real(),
            begin
                {ok, PID}    = erlog:start_link(),
                {succeed, _} = erlog:prove(PID, {float, I}),
                true
            end).

prop_integer()->
    ?FORALL(I,int(),
            begin
                {ok, PID}    = erlog:start_link(),
                {succeed, _} = erlog:prove(PID, {integer, I}),
                true
            end).
prop_number()->
    ?FORALL(I,oneof([int(),real()]),
            begin
                {ok, PID}    = erlog:start_link(),
                {succeed, _} = erlog:prove(PID, {number, I}),
                true
            end).


out(P) ->
   on_output(fun(S,F) -> io:format(user, S, F) end,P).

run_test_() ->
    Props = [
             fun prop_integer/0,
             fun prop_number/0,
             fun prop_float/0,
             fun prop_equals/0,
             fun prop_not_equals/0,
	     fun prop_comp/0

             ],
    [
     begin
         P = out(Prop()),
         ?_assert(quickcheck(numtests(500,P)))
     end
     || Prop <- Props].
