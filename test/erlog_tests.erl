-module(erlog_tests).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).


atom() ->
    elements(['a','b', 'X', 'Path','c']).
    
simple_term(0) ->
    oneof([atom(), 
	   {atom()}]);
simple_term(N) ->
    oneof([atom(), 
	   [],

	   char(),
	   binary(),
	   int(),
	   real(),
	   {atom()},
	   {'_'},
	   {atom(), simple_term(N - 1)},
	   {atom(), simple_term(N - 1), simple_term(N - 1)},
	   {atom(), simple_term(N - 1), simple_term(N - 1), simple_term(N - 1)}
	  ]).

term() ->
    oneof([simple_term(4), list(simple_term(4))]).

prop_is_legal_term() ->
    ?FORALL(PLTerm,
	    term(),
	    begin
		erlog:is_legal_term(PLTerm)
	    end).
