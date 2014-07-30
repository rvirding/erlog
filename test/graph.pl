/* -*-Prolog -*- */

connected(A,B) :-
	edge(A,B);
	edge(B,A).


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


