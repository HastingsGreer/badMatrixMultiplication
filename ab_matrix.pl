:- use_module(library(clpfd)).

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

identity(I):-
	I = [[1, 0, 0, 0],
	      [0, 1, 0, 0],
	      [0, 0, 1, 0],
	      [0, 0, 0, 1]].

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


smolMat([], 0).
smolMat([H|T], N):-
    nn(NN),
    smolRow(H, NN),
    smolMat(T, Ns),
    Ns #= N - 1.

smolRow([], 0).
smolRow([H|T], N):-
    H in 1..4,
    smolRow(T, Ns),
    Ns #= N - 1.


det([[A1, A2], [A3, A4]], 2, D):-
    D #= A1 * A4 - A2 * A3.


det([[E_0, E_1, E_2], [E_3, E_4, E_5], [E_6, E_7, E_8]], 3, D):-
    D #= -(E_5*E_7 - E_4*E_8)*E_0 + (E_2*E_7 - E_1*E_8)*E_3 - (E_2*E_4 - E_1*E_5)*E_6.

det([[E_0, E_1, E_2, E_3],
 [E_4, E_5, E_6, E_7],
  [E_8, E_9, E_10, E_11],
   [E_12, E_13, E_14, E_15]], 4, D):-
	   D #= E_1*E_11*E_14*E_4 - E_1*E_10*E_15*E_4 - E_11*E_13*E_2*E_4 + E_10*E_13*E_3*E_4 - E_0*E_11*E_14*E_5 + E_0*E_10*E_15*E_5 + E_11*E_12*E_2*E_5 - E_10*E_12*E_3*E_5 - E_1*E_11*E_12*E_6 + E_0*E_11*E_13*E_6 + E_1*E_10*E_12*E_7 - E_0*E_10*E_13*E_7 - E_15*E_2*E_5*E_8 + E_14*E_3*E_5*E_8 + E_1*E_15*E_6*E_8 - E_13*E_3*E_6*E_8 - E_1*E_14*E_7*E_8 + E_13*E_2*E_7*E_8 + E_15*E_2*E_4*E_9 - E_14*E_3*E_4*E_9 - E_0*E_15*E_6*E_9 + E_12*E_3*E_6*E_9 + E_0*E_14*E_7*E_9 - E_12*E_2*E_7*E_9.
%minor(MM, I, J, Mm):-
    
cofactors([[E_0, E_1, E_2, E_3],
   [E_4, E_5, E_6, E_7],
   [E_8, E_9, E_10, E_11],
   [E_12, E_13, E_14, E_15]], 4, C):-

 maplist(maplist(#=), C, [[E_11*E_13*E_6 - E_10*E_13*E_7 - (E_11*E_14 - E_10*E_15)*E_5 - (E_15*E_6 - E_14*E_7)*E_9,
  E_1*E_11*E_14 - E_1*E_10*E_15 - E_11*E_13*E_2 + E_10*E_13*E_3 + (E_15*E_2 - E_14*E_3)*E_9,
  -(E_15*E_2 - E_14*E_3)*E_5 + (E_1*E_15 - E_13*E_3)*E_6 - (E_1*E_14 - E_13*E_2)*E_7,
  -E_1*E_11*E_6 + E_1*E_10*E_7 + (E_11*E_2 - E_10*E_3)*E_5 + (E_3*E_6 - E_2*E_7)*E_9],
 [-E_11*E_12*E_6 + E_10*E_12*E_7 + (E_11*E_14 - E_10*E_15)*E_4 + (E_15*E_6 - E_14*E_7)*E_8,
  -E_0*E_11*E_14 + E_0*E_10*E_15 + E_11*E_12*E_2 - E_10*E_12*E_3 - (E_15*E_2 - E_14*E_3)*E_8,
  (E_15*E_2 - E_14*E_3)*E_4 - (E_0*E_15 - E_12*E_3)*E_6 + (E_0*E_14 - E_12*E_2)*E_7,
  E_0*E_11*E_6 - E_0*E_10*E_7 - (E_11*E_2 - E_10*E_3)*E_4 - (E_3*E_6 - E_2*E_7)*E_8],
 [-E_11*E_13*E_4 + E_11*E_12*E_5 - (E_15*E_5 - E_13*E_7)*E_8 + (E_15*E_4 - E_12*E_7)*E_9,
  -E_1*E_11*E_12 + E_0*E_11*E_13 + (E_1*E_15 - E_13*E_3)*E_8 - (E_0*E_15 - E_12*E_3)*E_9,
  -(E_1*E_15 - E_13*E_3)*E_4 + (E_0*E_15 - E_12*E_3)*E_5 + (E_1*E_12 - E_0*E_13)*E_7,
  E_1*E_11*E_4 - E_0*E_11*E_5 + (E_3*E_5 - E_1*E_7)*E_8 - (E_3*E_4 - E_0*E_7)*E_9],
 [E_10*E_13*E_4 - E_10*E_12*E_5 + (E_14*E_5 - E_13*E_6)*E_8 - (E_14*E_4 - E_12*E_6)*E_9,
  E_1*E_10*E_12 - E_0*E_10*E_13 - (E_1*E_14 - E_13*E_2)*E_8 + (E_0*E_14 - E_12*E_2)*E_9,
  (E_1*E_14 - E_13*E_2)*E_4 - (E_0*E_14 - E_12*E_2)*E_5 - (E_1*E_12 - E_0*E_13)*E_6,
  -E_1*E_10*E_4 + E_0*E_10*E_5 - (E_2*E_5 - E_1*E_6)*E_8 + (E_2*E_4 - E_0*E_6)*E_9]]).


my_sumlist([], 0).
my_sumlist([H|T], N) :- my_sumlist(T, X), N #= X + H. 

% Matrix multiplication with matrices represented
% as lists of lists. M3 is the product of M1 and M2
mmult(M1, M2, M3) :- transpose(M2,MT), maplist(mm_helper(MT), M1, M3).
mm_helper(M2, I1, M3) :- maplist(dot(I1), M2, M3).

not_equal([A1, A2, A3, A4],[B1, B2, B3, B4]):-
    #\ ((A1 #= B1) #/\
        (A2 #= B2) #/\
	(A3 #= B3) #/\
        (A4 #= B4) ).

matrix_does_not_contain([], _).
matrix_does_not_contain([H|T], R):-
	not_equal(H, R),
	matrix_does_not_contain(T, R).

naive_full_rank([[_, _, _, _]]).
naive_full_rank([H|T]):-
	naive_full_rank(T),
	matrix_does_not_contain(T, H).

problem(1, A, B, C, ABf):-
    nn(NN),
    smolMat(A, NN),
    isMat(B, NN),
    %naive_full_rank(A),
    %transpose(A, AT),
    %naive_full_rank(AT),
    my_scamul(10, A, AX),
    addmat(AX, B, C),
    mmult(A, B, C),
    append(A, B, AB),
    flatten(AB, ABf).
