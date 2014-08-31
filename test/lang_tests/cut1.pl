%-*-prolog-*-
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

sum_to(1,1) :- !.
sum_to(N, Res) :-
	N1 is N - 1,
	sum_to(N1, Res1),
	Res is Res1 + N.

test(_File) :-
	sum_to(1,1),
	sum_to(5,15).

