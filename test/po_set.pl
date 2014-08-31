/* -*-Prolog -*- */
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


export(path/3).
edge(a,b).
edge(a,c).
edge(c,d).
edge(b,e).
edge(d,f).
edge(b,f).

connected(A,B) :-
	edge(A,B).

child(A) :-
	\+edge(A,_).

sib(A,B) :-
	path(C,A,_),
	path(C,B,_),
	\+path(A,B,_),
	\+path(B,A,_).

ancestor(A,B) :-
	path(A,B,_).

descendent(A,B) :-
	path(B,A,_).

path(A,B,Path) :-
       travel(A,B,[A],Q), 
       reverse(Q,Path).

travel(A,B,P,[B|P]) :- 
       connected(A,B).
travel(A,B,Visited,Path) :-
       connected(A,C),           
       C \== B,
       \+member(C,Visited),
       travel(C,B,[C|Visited],Path). 


