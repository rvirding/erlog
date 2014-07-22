-module(erlog_bips_tests).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("erlog_test.hrl").
-compile(export_all).

cops() ->
    oneof([{'=:=', fun (I,J) ->
			   I == J
		   end},
	   {'==', fun (I,J) ->
			   I == J
		   end},

	   {'=\\=', fun(I,J) ->
			   I /= J
		   end},
	   {'\\==', fun(I,J) ->
			   I /= J
		   end},

	   {'\\=', fun(I,J) ->
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

atom()->
    elements(['a','b','c','d','e','f','g','A',' ','_']).

is_atomic(A) when is_list(A) ->
    false;
is_atomic(A) when is_tuple(A) ->
    false;
is_atomic(_) ->
    true.

prop_atom() ->
    ?FORALL(MaybeAtom,
	    oneof([int(),atom()]),
	    begin
		{ok, PID}    = erlog:start_link(),
                case {erlog:prove(PID, {atom,  MaybeAtom}), is_atom(MaybeAtom)} of
		    {{succeed,_}, true} -> true;
		    {fail, false} -> true;
		    _  -> false
		end
	    end).


prop_is_integer() ->
    ?FORALL(MaybeAtom,
	    oneof([int(),atom(), real()]),
	    begin
		{ok, PID}    = erlog:start_link(),
                case {erlog:prove(PID, {integer,  MaybeAtom}), is_integer(MaybeAtom)} of
		    {{succeed,_}, true} -> true;
		    {fail, false} -> true;
		    _  -> false
		end
	    end).


prop_atomic_and_compound() ->
    ?FORALL(Atom,
	    oneof([int(),atom(),real(),binary(),non_empty(list(int())),{atom(), int()}]),
	    begin
		{ok, PID}    = erlog:start_link(),
                case {erlog:prove(PID, {atomic,  Atom}),
		      erlog:prove(PID, {compound,Atom})}
		      of
		    {{succeed,_},fail} ->
			is_atomic(Atom);
		    {fail,{succeed,_}} ->
			not(is_atomic(Atom))
		end
	    end).


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


