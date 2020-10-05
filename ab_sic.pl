:- use_module(library(clpfd)).

flatten([], []).
flatten([L|Ls], FlatL) :-
    flatten(Ls, NewLs),
    append(L, NewLs, FlatL).


maplist(_, [], []).
maplist(Pred_2, [A|As], [B|Bs]) :-
	        call(Pred_2, A, B),
		        maplist(Pred_2, As, Bs).

maplist(_, []).
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
    H in 0..9,
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

not_equal([A1, A2, A3, A4],[B1, B2, B3, B4]):-
    #\ ((A1 #= B1) #/\
        (A2 #= B2) #/\
	(A3 #= B3) #/\
        (A4 #= B4) ).
not_equal([A1, A2, A3, A4, A5],[B1, B2, B3, B4, B5]):-
    #\ ((A1 #= B1) #/\
        (A2 #= B2) #/\
	(A3 #= B3) #/\
	(A4 #= B4) #/\
        (A5 #= B5) ).

matrix_does_not_contain([], _).
matrix_does_not_contain([H|T], R):-
	not_equal(H, R),
	matrix_does_not_contain(T, R).

naive_full_rank([[A, B, C, D, E]]):-
        not_equal([A, B, C, D, E], [0, 0, 0, 0, 0]).
	
naive_full_rank([H|T]):-
	naive_full_rank(T),
        not_equal(H, [0, 0, 0, 0, 0]),
        matrix_does_not_contain(T, H).
problem(A, B, C, ABf):-
    nn(NN),
    isMat(A, NN),
    naive_full_rank(A),
    transpose(A, AT),
    naive_full_rank(AT),
    isMat(B, NN),
    my_scamul(10, A, AX),
    addmat(AX, B, C),
    mmult(A, B, C),
    append(A, B, AB),
    flatten(AB, ABf).

main(Seed, A, B):-
    problem(A, B, C, ABf),
    labeling([ff], ABf).
    %naive_full_rank(A),
    %transpose(A, AT),
    %naive_full_rank(AT).
    %det(A, 4, Determinant),
    %Determinant #\= 0.
