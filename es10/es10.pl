% Foldl
foldl(Goal, List, V0, V) :-
    foldl_(List, Goal, V0, V).

foldl_([], _, V, V).
foldl_([H|T], Goal, V0, V) :-
    call(Goal, H, V0, V1),
    foldl_(T, Goal, V1, V).

sum(A, B, R) :- R is A + B.

% Binary trees
tree_singleton(N, R) :- R = tree(N, empty, empty).

tree_insert(N, empty, tree(N, empty, empty)).
tree_insert(N, tree(N, L, R), tree(N, L, R)).
tree_insert(N, tree(Cur, L, R), tree(Cur, NewL, R)) :- N < Cur, tree_insert(N, L, NewL).
tree_insert(N, tree(Cur, L, R), tree(Cur, L, NewR)) :- N > Cur, tree_insert(N, R, NewR).

tree_elem(_N, empty, false).
tree_elem(N, tree(N, _, _), true).
tree_elem(N, tree(Cur, L, _R), Ret) :- N < Cur, tree_elem(N, L, Ret).
tree_elem(N, tree(Cur, _L, R), Ret) :- N > Cur, tree_elem(N, R, Ret).

tree_sum(empty, 0).
tree_sum(tree(N, L, R), Res) :- tree_sum(L, N1), tree_sum(R, N2), Res is N + N1 + N2.

tree_values(empty, []).
tree_values(tree(N, L, R), [N|Res]) :- tree_values(L, X1), tree_values(R, X2), append(X1, X2, Res).

% Hanoi
hanoi(N) :- dohanoi(N, a, b, c).

dohanoi(0, _, _, _) :- !.
dohanoi(N, A, B, C) :- !, N1 is N - 1, dohanoi(N1, A, B, C), moveit(A, B), dohanoi(N1, C, B, A).

moveit(F, T) :- print(F), print(--->), print(T), nl.

% Sudoku
all(Cond, Action) :- \+ (Cond, \+ Action).

related(vt(R,_), vt(R,_)).
related(vt(_,C), vt(_,C)).
related(vt(R,C), vt(R1,C1)) :- 
    A  is ((R  - 1) // 3) * 3 + ((C  - 1) // 3) + 1, 
    A1 is ((R1 - 1) // 3) * 3 + ((C1 - 1) // 3) + 1, 
    A = A1.

colors(C) :- C = [1, 2, 3, 4, 5, 6, 7, 8, 9].

sudoku_solve([], _, Solution, Solution).
sudoku_solve([V | Vr], Hints, Solution, Result) :-
    member(s(V,Ch), Hints),
    sudoku_solve(Vr, Hints, [s(V,Ch) | Solution], Result).
sudoku_solve([V | Vr], Hints, Solution, Result) :-
    colors(Cols), member(C,Cols),
    forall(member(s(Vs1,Cs1), Hints),    
           (Cs1 \= C ; not(related(V, Vs1)))), 
    forall(member(s(Vs2,Cs2), Solution), 
           (Cs2 \= C ; not(related(V, Vs2)))),
    sudoku_solve(Vr, Hints, [s(V,C) | Solution], Result). 

board(B) :-
    colors(C),
    def_row_board(C, C, [], B).

def_row_board([], _, S, S).
def_row_board([R | Rs], C, B, S) :-
    def_col_board(R, C, B, S1),
    def_row_board(Rs, C, S1, S).

def_col_board(_, [], S, S).
def_col_board(R, [C | Cs], B, S) :-
    def_col_board(R, Cs, [vt(R, C) | B], S).

hints(H) :- H = [s(vt(1,1), 5), s(vt(1,2), 3), s(vt(1,5), 7),
                 s(vt(2,1), 6), s(vt(2,4), 1), s(vt(2,5), 9),
                 s(vt(2,6), 5), s(vt(3,2), 9), s(vt(3,3), 8),
                 s(vt(3,8), 6), s(vt(4,1), 8), s(vt(4,5), 6),
                 s(vt(4,9), 3), s(vt(5,1), 4), s(vt(5,4), 8),
                 s(vt(5,6), 3), s(vt(5,9), 1), s(vt(6,1), 7),
                 s(vt(6,5), 2), s(vt(6,9), 6), s(vt(7,2), 6),
                 s(vt(7,7), 2), s(vt(7,8), 8), s(vt(8,4), 4),
                 s(vt(8,5), 1), s(vt(8,6), 9), s(vt(8,9), 5),
                 s(vt(9,5), 8), s(vt(9,8), 7), s(vt(9,9), 9)].
