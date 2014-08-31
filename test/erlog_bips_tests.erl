-module(erlog_bips_tests).
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
		{ok,Erlog}    = erlog:new(),
                case {erlog:prove(Erlog, {atom,  MaybeAtom}), is_atom(MaybeAtom)} of
		    {{{succeed,_},_}, true} -> true;
		    {{fail,_}, false} -> true;
		    _  -> false
		end
	    end).


prop_is_integer() ->
    ?FORALL(MaybeInt,
	    oneof([int(),atom(), real()]),
	    begin
		{ok,Erlog}    = erlog:new(),
                case {erlog:prove(Erlog, {integer,  MaybeInt}), is_integer(MaybeInt)} of
		    {{{succeed,_},_}, true} -> true;
		    {{fail,_}, false} -> true;
		    _  -> false
		end
	    end).


prop_atomic_and_compound() ->
    ?FORALL(Atom,
	    oneof([int(),atom(),real(),binary(),non_empty(list(int())),{atom(), int()}]),
	    begin
		{ok,Erlog}    = erlog:new(),
                case {erlog:prove(Erlog, {atomic,  Atom}),
		      erlog:prove(Erlog, {compound,Atom})}
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
                {ok, ERLOG}    = erlog:new(),
                case erlog:prove(ERLOG, {Op, I, J}) of
		    {{succeed, _},#est{}} -> 
			C(I,J);
		    {fail,#est{}} ->
			not(C(I,J))
		    end
		end).

any() ->
    oneof([int(),atom(), binary(),list(char())]).

prop_equals() ->
    ?FORALL(I, any(),
            begin
                {ok,Erlog}    = erlog:new(),
		?assertMatch({{succeed, [{'X',I}]},_}, erlog:prove(Erlog, {'=', I,     {'X'}})),
		?assertMatch({{succeed, [{'X',I}]},_}, erlog:prove(Erlog, {'=', {'X'}, I})),
                ?assertMatch({{succeed, []},_},        erlog:prove(Erlog, {'=', I,     I})),
		true
            end).
prop_not_equals() ->
    ?FORALL({I,J}, {any(),any()},
	    ?IMPLIES(I /= J,
            begin
                {ok,Erlog}    = erlog:new(),
                ?assertMatch({fail,_}, erlog:prove(Erlog, {'=', I, J})),
		true
            end)).

prop_float()->
    ?FORALL(I,real(),
            begin
                {ok, ERLOG}    = erlog:new(),
                {{succeed, _},#est{}} = erlog:prove(ERLOG, {float, I}),
                true
            end).

prop_integer()->
    ?FORALL(I,int(),
            begin
                {ok, ERLOG}    = erlog:new(),
                {{succeed, _},#est{}} = erlog:prove(ERLOG, {integer, I}),
                true
            end).
prop_number()->
    ?FORALL(I,oneof([int(),real()]),
            begin
                {ok, ERLOG}    = erlog:new(),
                {{succeed, _},#est{}} = erlog:prove(ERLOG, {number, I}),
                true
            end).



prop_arg() ->
    ?FORALL(T,
	    non_empty(list(oneof([binary(),int(), bool(), char(), real()]))),
	    ?FORALL(Place, choose(1,length(T)),
		    begin
			{ok,Erlog} = erlog:new(),
			P  = list_to_tuple([tuple|T]),

			{{succeed, [{'El',El}]},_} = erlog:prove(Erlog, {arg, Place, P, {'El'}}),
			?assertEqual(element(Place + 1, P), El),
			true

		    end)).


clause_test() ->
    {ok,E}		= erlog:new(),
    {ok, E1}            = erlog:consult(E,"../stdlib/erlang.pl"),
    {{succeed, A1},E2}	= erlog:prove(E1, {clause, {record, {'X'},{'Y'}}, {'Z'}}),
    {{succeed, A2},_E3} = erlog:next_solution(E2),
    ?assertEqual(3,length(A1)),
    ?assertEqual(3,length(A2)),
    ?assertEqual('!', proplists:get_value('Z', A1)),
   % ?assertMatch({record,{_},{_},1}, proplists:get_value('Z',A2)),
    true.


