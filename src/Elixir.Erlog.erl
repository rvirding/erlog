%% Copyright (c) 2022 Robert Virding
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

%% File    : erlog.erl
%% Author  : Robert Virding
%% Purpose : Elixir style wrappers for erlog.erl
%%
%% This module just contains functions that forward to erlog.erl, but
%% place the Erlog State arguments in the first position rather than
%% the last. This better matches Elixir conventions and allows for
%% using the Elixir pipe operator '|>' to chain Erlog function calls.

-module('Elixir.Erlog').

%% Basic evaluator interface.
-export([new/0,new/2,
	 prove/2,next_solution/1,
	 consult/2,reconsult/2,load/2,
	 get_db/1,set_db/2,set_db/3]).

%% User utilities.
-export([is_legal_term/1,vars_in/1]).

new() ->
    erlog:new().

new(DbMod, DbArg) ->
    erlog:new(DbMod, DbArg).

prove(Erl, Goal) ->
    erlog:prove(Goal, Erl).    

next_solution(Erl) ->
    erlog:next_solution(Erl).

consult(Erl, File) ->
    erlog:consult(File, Erl).

reconsult(Erl, File) ->
    erlog:reconsult(File, Erl).

load(Erl, Module) ->
    erlog:load(Module, Erl).

get_db(Erl) ->
    erlog:get_db(Erl).

set_db(Erl, Ref) ->
    erlog:set_db(Ref, Erl).

set_db(Erl, Mod, Ref) ->
    erlog:set_db(Mod, Ref, Erl).

is_legal_term(Goal) ->
    erlog:is_legal_term(Goal).

vars_in(Term) ->
    erlog:vars_in(Term).
