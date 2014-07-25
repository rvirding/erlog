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
		Erlog    = erlog:new(),
                case {Erlog({prove, {atom,  MaybeAtom}}), is_atom(MaybeAtom)} of
		    {{{succeed,_},_}, true} -> true;
		    {{fail,_}, false} -> true;
		    _  -> false
		end
	    end).


prop_is_integer() ->
    ?FORALL(MaybeInt,
	    oneof([int(),atom(), real()]),
	    begin
		Erlog    = erlog:new(),
                case {Erlog({prove, {integer,  MaybeInt}}), is_integer(MaybeInt)} of
		    {{{succeed,_},_}, true} -> true;
		    {{fail,_}, false} -> true;
		    _  -> false
		end
	    end).


prop_atomic_and_compound() ->
    ?FORALL(Atom,
	    oneof([int(),atom(),real(),binary(),non_empty(list(int())),{atom(), int()}]),
	    begin
		Erlog    = erlog:new(),
                case {Erlog({prove, {atomic,  Atom}}),
		      Erlog({prove, {compound,Atom}})}
		      of
		    {{{succeed,_},_},{fail,_}} ->
			is_atomic(Atom);
		    {{fail,_},{{succeed,_},_}} ->
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

any() ->
    oneof([int(),atom(), binary(),list(char())]).

prop_equals() ->
    ?FORALL(I, any(),
            begin
                Erlog    = erlog:new(),
		?assertMatch({{succeed, [{'X',I}]},_}, Erlog({prove, {'=', I,     {'X'}}})),
		?assertMatch({{succeed, [{'X',I}]},_}, Erlog({prove, {'=', {'X'}, I}})),
                ?assertMatch({{succeed, []},_},        Erlog({prove, {'=', I,     I}})),
		true
            end).
prop_not_equals() ->
    ?FORALL({I,J}, {any(),any()},
	    ?IMPLIES(I /= J,
            begin
                Erlog    = erlog:new(),
                ?assertMatch({fail,_}, Erlog({prove, {'=', I, J}})),
		true
            end)).

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


prop_arg() ->
    ?FORALL(T,
	    non_empty(list(int())),
	    ?FORALL(Place, choose(1,length(T)),
		    begin
			Erlog = erlog:new(),
			P  = list_to_tuple([tuple|T]),
			{{succeed, [{'El',El}]},_} = Erlog({prove, {arg, Place, P, {'El'}}}),
			?assertEqual(element(Place + 1, P), El),
			true

		    end)).
