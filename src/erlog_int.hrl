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

%% File    : erlog_int.erl
%% Author  : Robert Virding
%% Purpose : Basic interpreter of a Prolog definitions.

%% Some standard type macros.

%% The old is_constant/1 ?
-define(IS_CONSTANT(T), (not (is_tuple(T) orelse is_list(T)))).

%% -define(IS_ATOMIC(T), (is_atom(T) orelse is_number(T) orelse (T == []))).
-define(IS_ATOMIC(T), (not (is_tuple(T) orelse (is_list(T) andalso T /= [])))).
-define(IS_FUNCTOR(T), (is_tuple(T) andalso (tuple_size(T) >= 2) andalso is_atom(element(1, T)))).

%% Define the interpreter state record.
-record(est, {cps,				%Choice points
	      bs,				%Bindings
	      vn,				%Var num
	      db,				%Database
	      fs				%Flags
	     }).
-record(db, {mod,				%Database module
	     ref,				%Database reference
	     loc				%Local database
	    }).

%% Define the choice point record.
-record(cp, {type,label,data,next,bs,vn}).
-record(cut, {label,next}).
