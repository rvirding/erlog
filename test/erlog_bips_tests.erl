-module(erlog_bips_tests).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("erlog_test.hrl").
-compile(export_all).

prop_arg() ->
    ?FORALL(T,
	    non_empty(list(oneof([binary(),int(), bool(), char(), real()]))),
	    ?FORALL(Place, choose(1,length(T)),
		    begin
			Erlog = erlog:new(),
			P  = list_to_tuple([tuple|T]),

			{{succeed, [{'El',El}]},_} = Erlog({prove, {arg, Place, P, {'El'}}}),
			?assertEqual(element(Place + 1, P), El),
			true

		    end)).


