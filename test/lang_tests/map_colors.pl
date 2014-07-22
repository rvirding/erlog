%-*-Prolog-*-
%% Map coloring example
adjacent(1,2).
adjacent(1,3).
adjacent(1,4).
adjacent(1,5).
adjacent(2,1). 
adjacent(2,3).
adjacent(2,4).
adjacent(3,1). 
adjacent(3,2). 
adjacent(3,4).
adjacent(4,1). 
adjacent(4,2). 
adjacent(4,3). 
adjacent(4,5).
adjacent(5,1). 
adjacent(5,4).

color(1,red,a).    color(1,red,b). 
color(2,blue,a).   color(2,blue,b). 
color(3,green,a).  color(3,green,b). 
color(4,yellow,a). color(4,blue,b). 
color(5,blue,a).   color(5,green,b).

conflict(Coloring) :- 
	adjacent(X,Y), 
	color(X,Color,Coloring), 
	color(Y,Color,Coloring).

test(_) :-
	conflict(b),
	\+conflict(a).
