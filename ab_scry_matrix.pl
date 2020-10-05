:- use_module(library(clpz)).

maplist(_, [], []).
maplist(Pred_2, [A|As], [B|Bs]) :-
	        call(Pred_2, A, B),
		        maplist(Pred_2, As, Bs).

maplist(_, [], []).
maplist(Pred_2, [A|As]) :-
	        call(Pred_2, A),
		        maplist(Pred_2, As).
maplist(_, [], [], []).
maplist(Pred_2, [A|As], [B|Bs], [C|Cs]) :-
	        call(Pred_2, A, B, C),
		        maplist(Pred_2, As, Bs, Cs).
transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).

transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
       lists_firsts_rests(Ms, Ts, Ms1),
       transpose(Rs, Ms1, Tss).

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        lists_firsts_rests(Rest, Fs, Oss).

% N is the dot product of lists V1 and V2.
dot(V1, V2, N) :- maplist(product,V1,V2,P), my_sumlist(P,N).
product(N1,N2,N3) :- N3 #= N1*N2.

my_scamul(K, [], []).

my_scamul(K, [R|T], [R2|T2]):-
    my_rowmul(K, R, R2),
    my_scamul(K, T, T2).

my_rowmul(K, [], []).
my_rowmul(K, [V|T], [V2|T2]):-
    K * V #= V2,
    my_rowmul(K, T, T2).

addmat([], [], []).
addmat([HA|TA], [HB|TB], [HC|TC]):-
    addrow(HA, HB, HC),
    addmat(TA, TB, TC).

addrow([], [], []).
addrow([HA|TA], [HB|TB], [HC|TC]):-
    HA + HB #= HC,
    addrow(TA, TB, TC).

nn(NN) :-
    NN #= 4.

isMat([], 0).
isMat([H|T], N):-
    nn(NN),
    isRow(H, NN),
    isMat(T, Ns),
    Ns #= N - 1.

isRow([], 0).
isRow([H|T], N):-
    H in 1..9,
    isRow(T, Ns),
    Ns #= N - 1.

det([[A1, A2], [A3, A4]], 2, D):-
    D #= A1 * A4 - A2 * A3.

%minor(MM, I, J, Mm):-
    

my_sumlist([], 0).
my_sumlist([H|T], N) :- my_sumlist(T, X), N #= X + H. 

% Matrix multiplication with matrices represented
% as lists of lists. M3 is the product of M1 and M2
mmult(M1, M2, M3) :- transpose(M2,MT), maplist(mm_helper(MT), M1, M3).
mm_helper(M2, I1, M3) :- maplist(dot(I1), M2, M3).


problem(1, A, B):-
    nn(NN),
    isMat(A, NN),
    isMat(B, NN),
    my_scamul(10, A, AX),
    addmat(AX, B, C),
    mmult(A, B, C).
