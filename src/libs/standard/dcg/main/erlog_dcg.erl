%% Copyright (c) 2008-2013 Robert Virding
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

%% File    : erlog_dcg.erl
%% Author  : Robert Virding
%% Purpose : DCG conversion and procedures.

-module(erlog_dcg).

-include("erlog_core.hrl").
-include("erlog_dcg.hrl").

-behaviour(erlog_stdlib).

-export([load/1]).
-export([prove_goal/1]).

load(Db) ->
	lists:foreach(fun(Proc) -> erlog_memory:load_kernel_space(Db, ?MODULE, Proc) end, ?ERLOG_DCG).

prove_goal(Params = #param{goal = {expand_term, _, _} = Goal, bindings = Bs, var_num = Vn0}) ->
	{expand_term, DCGRule, A2} = ec_support:dderef(Goal, Bs),
	{Exp, Vn1} = ed_logic:expand_term(DCGRule, Vn0),
	ec_body:unify_prove_body(A2, Exp, Params#param{var_num = Vn1});
prove_goal(Params = #param{goal = {phrase, A, B}}) ->
	ed_logic:phrase(Params#param{goal = {phrase, A, B, []}});
prove_goal(Params = #param{goal = {phrase, _, _, _}}) ->
	ed_logic:phrase(Params).