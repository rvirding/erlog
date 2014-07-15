%%%%%%%%%%%%%%
%% there is
%% types.pl
%% https://drive.google.com/folderview?id=0B3uCF8tBzpM4bzBfaFBzNVFiRUk&usp=sharing
%%%%%%%%%%%%%%

%�������� �����

%�����

plus(A,B,C):- nonvar(A), nonvar(B), C is A+B.

plus(A,B,C):- var(A),nonvar(B),A is C-B.

plus(A,B,C):- var(B),nonvar(A),B is C-A.

filter(a,X):-atom(X).

filter(i,X):-integer(X).

filter(at,X):-atomic(X).

filter_list([],[],_).

filter_list([X|T],[X|Tt],F) :- filter(F,X),!, filter_list(T,Tt,F).

filter_list([_|T],Tt,F) :- filter_list(T,Tt,F).

%������������ ��������

fib(1,1).

fib(2,1).

fib(X,Y):- X>2, X1 is X-1, X2 is X-2,

 fib(X1,F1),

 fib(X2,F2), Y is F1+F2.

range(X,Y,X):-X=<Y.

range(X,Y,Z):-X<Y, X1 is X+1, range(X1,Y,Z).

%������� �����

simple(X) :- not((range(2,X,Y),X\=Y, 0=:=X mod Y)).

simple2(X):-not((sbase(Y),Y\=X,0=:=X mod Y)),!,assert(sbase(X)).

simples2(A,B):- assert(sbase(2)),range(A,B,X), simple2(X),write(X),write(' '),false,!;true.

% �������� ��������

simple3(N,N,L,L):-!.

simple3(N,M,L,R) :- N1 is N+1, checknum(N1,M,L,R).

checknum(N1,M1,L,R):- member(L,N), N1 mod N =:=0,!, simple3(N1,M1,L,R).

checknum(N1,M1,L,R):- simple3(N1,M1,[N1|L],R).

member([X|_],X).

member([_|L],X):-member(L,X).

test1 :- plus(X,1,6), writeln(x=X),

 plus(1,5,Y), writeln(x=Y),

 Ls=[aa,1,1.5,b,2,1+2,c,4,45,6,[1,3],day(monday)],

 %%Ls=[aa,1,b,2,1+2,c,4,1.5,45,6,str(1,3),day(monday)],

 write(list=Ls),nl,

 filter_list(Ls,A,a),writeln(atoms=A),

 filter_list(Ls,In,i),writeln(ints=In),

 filter_list(Ls,At,at),writeln(atomics=At).

test1m :-
 plus(X,1,6), X == 5,

 plus(1,5,Y), Y = 6,

 Ls=[aa,1,1.5,b,2,1+2,c,4,45,6,[1,3],day(monday)],
 filter_list(Ls,A,a),
 A = [aa,b,c],
 filter_list(Ls,In,i), In = [1,2,4,45,6],
 filter_list(Ls,At,at), At = [aa,1,1.5,b,2,c,4,45,6].

test2:- write('fibonachi: '),

 range(1,20,Nx),fib(Nx,Xx),write(Xx),write(','),false;

 nl.

test2m:-

 range(1,20,Nx),fib(Nx,Xx),false;

 true.

test3m:-
	simple3(1,100,[],Lst),
	Lst = [97,89,83,79,73,71,67,61,59,53,47,43,41,37,31,29,23,19,17,13,11,7,5,3,2],
	test3m([], L),
	L = Lst,
	simples2m(2, 100, Lst).
test3m(Lst, L):-
	range(2,100,N),
	simple(N),
	\+(append(_, [N|_], Lst)),
	!,
	test3m([N|Lst], L).
test3m(Lst, Lst).	

simples2m(A, B, Lst):-
	assert(sbase(2)),
	simples2m(A, B, [], L),
	L = Lst.

simples2m(A, B, Lst, L):-
	range(A,B,X), simple2(X),
	\+(append(_, [X|_], Lst)),
	simples2m(A, B, [X|Lst], L).
simples2m(A, B, Lst, Lst).

test3 :-write('simple1: '),simple3(1,100,[],Lst), writeln(Lst),false.

test3 :-write('simple2: '),range(2,100,N), simple(N),write(N),write(' '),false.

test3 :-nl,write('simple3: '),simples2(2,100),nl.

test_all_old:-

 test1, writeln('---ok---'),!,

 test2, writeln('---ok---'),!,

 test3, writeln('---ok---'),!.

test_all:-

 test1m,

 test2m,

 test3m,!.


%%:-test2.
