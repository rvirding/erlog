s  -->  np,vp.
np  -->  det,n.
vp  -->  v,np.
vp  -->  v.
det  -->  [the].
det  -->  [a].
n  -->  [woman].
n  -->  [man].
v  -->  [shoots].

test(_) :-
	np([a, woman],[]),
	s([a,woman,shoots,a,man],[]),
	\+vp([a,woman],[]).
