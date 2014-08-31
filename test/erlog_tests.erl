-module(erlog_tests).
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
