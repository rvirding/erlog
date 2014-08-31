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

				%
% The N queesns problem, for N = 8, there should be 92 solutions (Well known result)
%

solution(Queens) :-
	permutation([1,2,3,4,5,6,7,8], Queens),
	safe(Queens).
 
permutation([],[]).
 
permutation([Head|Tail],PermList) :-
	permutation(Tail,PermTail),
	del(Head,PermList,PermTail).
 
del(Item,[Item|List],List).
 
del(Item,[First|List],[First|List1]) :-
	del(Item,List,List1).
 
safe([]).
 
safe([Queen|Others]) :-
	safe(Others),
	
	noattack(Queen,Others,1).
 
noattack(_,[],_).
 
noattack(Y,[Y1|Ylist],Xdist) :-
	Y1 - Y  =\= Xdist,
	Y  - Y1 =\= Xdist,
	Dist1 is Xdist + 1,
	noattack(Y,Ylist,Dist1).

test(_) :-
	length(S,8),
	solution(S).





	
