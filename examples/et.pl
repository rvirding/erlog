%%% -*- mode: prolog -*-

et((H0 --> B0), (H1 :- B1)) :-
	et(H0, H1, V1, V2),
	et(B0, B1, V1, V2).
et(T, T).

et(T, E, V1, V2) :- var(T), E = phrase(T, V1, V2).
et(!, !, V1, V2) :- V1 = V2.
et(\+ T, \+ E, V1, V2) :- et(T, E, V1, V3), V1 = V2.
et((L0,R0), (L1,R1), V1, V2) :-
	et(L0, L1, V1, V3),
	et(R0, R1, V3, V2).
et((L0->R0), (L1->R1), V1, V2) :-
	et(L0, L1, V1, V3),
	et(R0, R1, V3, V2).
et((L0;R0), (L1;R1), V1, V2) :-
	et(L0, L1, V1, V2),
	et(R0, R1, V1, V2).
et([], V1 = V2, V1, V2).
et([T], 'C'(V1, T, V2), V1, V2).
et([T|Ts], ('C'(V1, T, V3),Et), V1, V2) :-
    et(Ts, Et, V3, V2).
et(F0, F1, V1, V2) :-
	F0 =.. L0,
	append(L0, [V1,V2], L1),
	F1 =.. L1.
