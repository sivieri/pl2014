% Genealogy
%
%                              James I
%                                 |
%                                 |
%                +----------------+-----------------+
%                |                                  |
%             Charles I                          Elizabeth
%                |                                  |
%                |                                  |
%     +----------+------------+                     |
%     |          |            |                     |
% Catherine   Charles II   James II               Sophia
%                                                   |
%                                                   |
%                                                   |
%                                                George I
male(james1).
male(charles1).
male(charles2).
male(james2).
male(george1).
female(catherine).
female(elizabeth).
female(sophia).

parent(james1, charles1).
parent(james1, elizabeth).
parent(charles1, charles2).
parent(charles1, catherine).
parent(charles1, james2).
parent(elizabeth, sophia).
parent(sophia, george1).

father(F, C) :- male(F), parent(F, C).
mother(M, C) :- female(M), parent(M, C).
grandfather(G, C) :- father(G, X), parent(X, C).
grandmother(G, C) :- mother(G, X), parent(X, C).
son(S, P) :- male(S), parent(P, S).
daughter(D, P) :- female(D), parent(P, D).
siblings(A, B) :- parent(P, A), parent(P, B), A \= B.
uncle(U, N) :- male(U), siblings(U, X), parent(X, N).
aunt(A, N) :- female(A), siblings(A, X), parent(X, N).

descendant(D, A) :- parent(A, D).
descendant(D, A) :- parent(X, D), descendant(X, A).

ancestor(A, D) :- descendent(D, A).

% Reverse
rev([], []).
rev([H|T], Rev) :- rev(T, RT), append(RT, [H], Rev).

rev2(L, RL) :- rev2_acc(L, [], RL).

rev2_acc([], Acc, Acc).
rev2_acc([H|T], Acc, Rev) :- rev2_acc(T, [H|Acc], Rev).

% Takeout
% 
% Usage 1: take out an element
% - when X is taken out of [X|R], R results
% - when X is taken out of the tail of [X|R], [X|S] results, where S is the result of taking X out of R
% takeout(X,[1,2,3],L).
% 
% Usage 2: add an element
% - insert X into W to produce Z
% takeout(3,W,[a,b,c]).
takeout(X, [X|R], R).
takeout(X, [F|R], [F|S]) :- takeout(X, R, S).

% Permutation
% 
% - Z is a permutation of [X|Y] provided W is a permutation of Y and then X is put into W to produce Z
% - [] is the (only) permutation of []
perm([],[]).
perm([X|Y],Z) :- perm(Y,W), takeout(X,Z,W).   

% Length
len([], 0).
len([H|T], N) :- len(T, N1), N is N1 +1.

% Map
% map(double, [1, 2, 3], X).
map(_, [], []).
map(F, [X|XS], [Y|YS]) :- call(F, X, Y), map(F, XS, YS).

double(N, R) :- R is N * N.

% Pack
pack([], []).
pack([X|XS], [Z|ZS]) :- pack_helper(X, XS, YS, Z), pack(YS, ZS).

pack_helper(X, [], [], [X]).
pack_helper(X, [Y|YS], [Y|YS], [X]) :- X \= Y.
pack_helper(X, [X|XS], YS, [X|ZS]) :- pack_helper(X, XS, YS, ZS).

% Encode
encode(L1, L2) :- pack(L1, L), encode_helper(L, L2).

encode_helper([], []).
encode_helper([[X|XS]|YS], [[N|[X]]|ZS]) :- length([X|XS], N), encode_helper(YS, ZS).

% Right triangles
right_triangles(N, A, B, C) :- between(1, N, C), between(1, C, B), between(1, B, A), C^2 =:= A^2 + B^2.
