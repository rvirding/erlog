%   File   : TREES.PL
%   Author : R.A.O'Keefe
%   Updated: 25 November 1983
%   Purpose: Updatable binary trees.

% /*  These are the routines I meant to describe in DAI-WP-150, but the
%     wrong version went in.  We have
% 	list_to_tree : O(N)
% 	tree_to_list : O(N)
% 	tree_size    : O(N)
% 	map_tree     : O(N)
% 	get_label    : O(lg N)
% 	put_label    : O(lg N)
%     where N is the number of elements in the tree.  The way get_label
%     and put_label work is worth noting: they build up a pattern which
%     is matched against the whole tree when the position number finally
%     reaches 1.  In effect they start out from the desired node and
%     build up a path to the root.  They still cost O(lg N) time rather
%     than O(N) because the patterns contain O(lg N) distinct variables,
%     with no duplications.  put_label simultaneously builds up a pattern
%     to match the old tree and a pattern to match the new tree.
% */

% :- public
% 	get_label/3,
% 	list_to_tree/2,
% 	map_tree/3,
% 	put_label/4,
% 	tree_size/2,
% 	tree_to_list/2.

% :- mode
% 	get_label(+, +, ?),
% 	    find_node(+, +, +),
% 	list_to_tree(+, -),
% 	    list_to_tree(+, +, -),
% 		list_to_tree(+),
% 	map_tree(+, +, -),
% 	put_label(+, +, +, -),
% 	    find_node(+, +, +, -, +),
% 	tree_size(+, ?),
% 	    tree_size(+, +, -),
% 	tree_to_list(+, -),
% 	    tree_to_list(+, -, -).


%   get_label(Index, Tree, Label)
%   treats the tree as an array of N elements and returns the Index-th.
%   If Index < 1 or > N it simply fails, there is no such element.

get_label(N, Tree, Label) :-
	find_node(N, Tree, t(Label,_,_)).


	find_node(1, Tree, Tree) :- !.
	find_node(N, Tree, Node) :-
		N > 1,
		0 is N mod 2,
		M is N //  2, !,
		find_node(M, Tree, t(_,Node,_)).
	find_node(N, Tree, Node) :-
		N > 2,
		1 is N mod 2,
		M is N //  2, !,
		find_node(M, Tree, t(_,_,Node)).



%   list_to_tree(List, Tree)
%   takes a given List of N elements and constructs a binary Tree
%   where get_label(K, Tree, Lab) <=> Lab is the Kth element of List.

list_to_tree(List, Tree) :-
	list_to_tree(List, [Tree|Tail], Tail).


	list_to_tree([Head|Tail], [t(Head,Left,Right)|Qhead], [Left,Right|Qtail]) :-
		list_to_tree(Tail, Qhead, Qtail).
	list_to_tree([], Qhead, []) :-
		list_to_tree(Qhead).
	

		list_to_tree([t|Qhead]) :-
			list_to_tree(Qhead).
		list_to_tree([]).



%   map_tree(Pred, OldTree, NewTree)
%   is true when OldTree and NewTree are binary trees of the same shape
%   and Pred(Old,New) is true for corresponding elements of the two trees.
%   In fact this routine is perfectly happy constructing either tree given
%   the other, I have given it the mode I have for that bogus reason
%   "efficiency" and because it is normally used this way round.  This is
%   really meant more as an illustration of how to map over trees than as
%   a tool for everyday use.

map_tree(Pred, t(Old,OLeft,ORight), t(New,NLeft,NRight)) :-
	apply(Pred, [Old,New]),
	map_tree(Pred, OLeft, NLeft),
	map_tree(Pred, ORight, NRight).
map_tree(_, t, t).



%   put_label(Index, OldTree, Label, NewTree)
%   constructs a new tree the same shape as the old which moreover has the
%   same elements except that the Index-th one is Label.  Unlike the
%   "arrays" of Arrays.Pl, OldTree is not modified and you can hang on to
%   it as long as you please.  Note that O(lg N) new space is needed.

put_label(N, Old, Label, New) :-
	find_node(N, Old, t(_,Left,Right), New, t(Label,Left,Right)).


	find_node(1, Old, Old, New, New) :- !.
	find_node(N, Old, OldSub, New, NewSub) :-
		N > 1,
		0 is N mod 2,
		M is N //  2, !,
		find_node(M, Old, t(Label,OldSub,Right), New, t(Label,NewSub,Right)).
	find_node(N, Old, OldSub, New, NewSub) :-
		N > 2,
		1 is N mod 2,
		M is N //  2, !,
		find_node(M, Old, t(Label,Left,OldSub), New, t(Label,Left,NewSub)).



%   tree_size(Tree, Size)
%   calculates the number of elements in the Tree.  All trees made by
%   list_to_tree that are the same size have the same shape.

tree_size(Tree, Size) :-
	tree_size(Tree, 0, Total), !,
	Size = Total.


	tree_size(t(_,Left,Right), SoFar, Total) :-
		tree_size(Right, SoFar, M),
		N is M+1, !,
		tree_size(Left, N, Total).
	tree_size(t, Accum, Accum).



%   tree_to_list(Tree, List)
%   is the converse operation to list_to_tree.  Any mapping or checking
%   operation can be done by converting the tree to a list, mapping or
%   checking the list, and converting the result, if any, back to a tree.
%   It is also easier for a human to read a list than a tree, as the
%   order in the tree goes all over the place.

tree_to_list(Tree, List) :-
	tree_to_list([Tree|Tail], Tail, List).


	tree_to_list([], [], []) :- !.
	tree_to_list([t|_], _, []) :- !.
	tree_to_list([t(Head,Left,Right)|Qhead], [Left,Right|Qtail], [Head|Tail]) :-
		tree_to_list(Qhead, Qtail, Tail).


