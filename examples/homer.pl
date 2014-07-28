%% Homer Simpson, after a stop at Moe’s, went to the Springfield Mall to buy
%% Marge, Lisa, Bart and Maggie a gift in anticipation that they will be upset 
%% with him when he gets home. He bought 4 gifts: a green dress for Marge, a 
%% saxophone book for Lisa, a slingshot for Bart and a pacifier for Maggie. He 
%% recalls buying the gifts at: The Leftorium, Sprawl-Mart, Try-N-Save, and King
%% Toots. Somewhere along the way, Homer lost his car keys and had to walk home 
%% carrying the gifts. Wanting to retrace his steps and find his lost car keys, 
%% the family asks Homer where he bought the gifts and the order in which he 
%% bought the gifts. Being partly inebriated however, Homer couldn’t remember 
%% which stores he bought the gifts at and in which order he visited the
%% stores.
%% 
%% After some interrogation, Homer does remember:
%% 
%% · He bought the saxophone book at King Toot’s
%% · The store he visited just after buying the slingshot was not Sprawl-Mart
%% · The Leftorium was his second stop
%% · Two stops after leaving Try-N-Save, he bought the pacifier
%% 
%% Can you help drunken Homer and the Simpson family figure out the order that 
%% Homer bought the gifts and where he bought them?

%% All stores and presents. (Not really used)

store(leftorium).
store(sprawl_mart).
store(try_n_save).
store(king_toots).

present(green_dress).
present(saxophone_book).
present(slingshot).
present(pacifier).

solve(Solution) :-
	%% Generate all possible combinations.
	perm([leftorium,sprawl_mart,try_n_save,king_toots], [S1,S2,S3,S4]),
	perm([green_dress,saxophone_book,slingshot,pacifier], [P1,P2,P3,P4]),
	%% This is the solution.
	S = [bought(P1, S1),bought(P2, S2),bought(P3, S3),bought(P4, S4)],
	%% Now add facts.
	%% He bought the saxophone book at King Toot’s.
	member(bought(saxophone_book, king_toots), S),
	%% The store he visited just after buying the slingshot was
	%% not Sprawl-Mart.
	index(bought(slingshot, _), S, I21),
	index(bought(_, Sa), S, I22),
	I22 is I21 + 1,
	Sa \= sprawl_mart,
	%% The Leftorium was his second stop.
	index(bought(_, leftorium), S, 2),
	%% Two stops after leaving Try-N-Save, he bought the pacifier.
	index(bought(_, try_n_save), S, I41),
	index(bought(pacifier, _), S, I42),
	I42 is I41 + 2,
	%% We have our solution, now export it.
	Solution = S.

%% Utilities.

%% index(Term, List, -Index).
%%  Find the index of Term in List, backtracking finds all possible ones.

index(X, L, I) :- index(X, L, 1, I).

index(X, [X|_], I, I).
index(X, [_|L], I, R) :- I1 is I+1, index(X, L, I1, R).

%% perm(+List, ?Perm).
%%  Generate permutations of List, backtracking generates all.

%% perm([], []).
%% perm([X|Xs], Ys1) :- perm(Xs, Ys), insert(Ys, X, Ys1).

%% insert(X, Y, [Y|X]).
%% insert([A|B], C, [A|D]) :- insert(B, C, D).
