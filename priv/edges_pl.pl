/* -*-Prolog -*- */



%--------------------------------------------------------------------------------
erl_export(sib/3).
erl_export(path/3).
erl_export(add_edge/3).

add_edge(A,B,ok) :-
	appenda(edge(A,B)).

sib(A,B,ok) :-
	sib(A,B).

%--------------------------------------------------------------------------------
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


