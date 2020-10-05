:- use_module(library(clpfd)).

% N is the dot product of lists V1 and V2.
dot(V1, V2, N) :- maplist(product,V1,V2,P), my_sumlist(P,N).
product(N1,N2,N3) :- N3 #= N1*N2.

my_sumlist([], 0).
my_sumlist([H|T], N) :- my_sumlist(T, X), N #= X + H. 

my_scamul(_, [], []).

my_scamul(K, [R|T], [R2|T2]):-
    my_rowmul(K, R, R2),
    my_scamul(K, T, T2).

my_rowmul(_, [], []).
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

det([[E_0, E_1, E_2, E_3],
     [E_4, E_5, E_6, E_7],
     [E_8, E_9, E_10, E_11],
     [E_12, E_13, E_14, E_15]], 4, D):-
	   D #= E_1*E_11*E_14*E_4 - E_1*E_10*E_15*E_4 - E_11*E_13*E_2*E_4 + E_10*E_13*E_3*E_4 - E_0*E_11*E_14*E_5 + E_0*E_10*E_15*E_5 + E_11*E_12*E_2*E_5 - E_10*E_12*E_3*E_5 - E_1*E_11*E_12*E_6 + E_0*E_11*E_13*E_6 + E_1*E_10*E_12*E_7 - E_0*E_10*E_13*E_7 - E_15*E_2*E_5*E_8 + E_14*E_3*E_5*E_8 + E_1*E_15*E_6*E_8 - E_13*E_3*E_6*E_8 - E_1*E_14*E_7*E_8 + E_13*E_2*E_7*E_8 + E_15*E_2*E_4*E_9 - E_14*E_3*E_4*E_9 - E_0*E_15*E_6*E_9 + E_12*E_3*E_6*E_9 + E_0*E_14*E_7*E_9 - E_12*E_2*E_7*E_9.
    
% Matrix multiplication with matrices represented
% as lists of lists. M3 is the product of M1 and M2
mmult(M1, M2, M3) :- transpose(M2,MT), maplist(mm_helper(MT), M1, M3).
mm_helper(M2, I1, M3) :- maplist(dot(I1), M2, M3).

problem(A, B, C, ABf):-
    nn(NN),
    isMat(A, NN),
    isMat(B, NN),
    my_scamul(10, A, AX),
    addmat(AX, B, C),
    mmult(A, B, C),
    append(A, B, AB),
    flatten(AB, ABf).

main(Seed):-
    problem(A, B, C, ABf),
    labeling([ff, random_value(Seed)], ABf),
    det(A, 4, Determinant),
    write_ln(A),
    write_ln(B),
    write_ln(C),
    write_ln(Determinant),
    Determinant #\= 0.
