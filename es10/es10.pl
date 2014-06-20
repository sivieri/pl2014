% Foldl
% [5, 3, 7, 1, 4, 6, 8]
foldl(_, [], Acc, Acc).
foldl(F, [H|T], Acc, R) :-
    call(F, H, Acc, Acc1),
    foldl(F, T, Acc1, R).

sum(A, B, R) :- R is A + B.

% Cut
s(X, Y) :- q(X, Y).
s(0, 0).

% q(X, Y) :- i(X), !, j(Y).
q(X, Y) :- i(X), j(Y).

i(1).
i(2).

j(1).
j(2).
j(3).

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

% Hanoi - Compare with known solving algorithms
hanoi(N) :- dohanoi(N, left, right, center).

dohanoi(1, X, Y, _) :-  
    write('Move top disk from '), 
    write(X), 
    write(' to '), 
    write(Y), 
    nl. 
dohanoi(N, X, Y, Z) :- 
    N > 1, 
    M is N - 1, 
    dohanoi(M, X, Z, Y), 
    dohanoi(1, X, Y, _), 
    dohanoi(M, Z, Y, X). 

% 8-queens
% queens([1/C1, 2/C2, 3/C3, 4/C4, 5/C5, 6/C6, 7/C7, 8/C8]).
queens([]). 
queens([Row/Col | Rest]) :-
            queens(Rest), % place all the queens in the rest of the rows
            member(Col, [1,2,3,4,5,6,7,8]), % choose a position for this row's queen
            safe(Row/Col, Rest). % check this position against all the rows below

safe(_, []). 
safe(Row/Col, [Row1/Col1 | Rest]) :-
            Col =\= Col1, % same column
            Col1 - Col =\= Row1 - Row, % diagonal 1
            Col1 - Col =\= Row - Row1, % diagonal 2
            safe(Row/Col, Rest). % next row
 
% Sudoku
% vt(X, Y) is a cell in the game board
% s(vt(X, Y), N) is the value N for the cell vt

% This checks if two cells are related (same row, column or region)
related(vt(R,_), vt(R,_)). % same row
related(vt(_,C), vt(_,C)). % same column
related(vt(R,C), vt(R1,C1)) :- % same region
    A  is ((R  - 1) // 3) * 3 + ((C  - 1) // 3) + 1, 
    A1 is ((R1 - 1) // 3) * 3 + ((C1 - 1) // 3) + 1, 
    A = A1.

% Possible values (in a relation just as a shortcut when used in other functions
colors(C) :- C = [1, 2, 3, 4, 5, 6, 7, 8, 9].

% board(B), existing(E), sudoku(B, E, [], S), print(S).
% Board: all cells of the board
% Existing: pre-populated values in the board
sudoku([], _, Acc, Acc). % all board values have been assigned, we are done
sudoku([V | Vr], Existing, Acc, Result) :-
    member(s(V,Ch), Existing), % the current cell already has a value...
    sudoku(Vr, Existing, [s(V,Ch) | Acc], Result). % ... so we simply add it to the accumulator
sudoku([V | Vr], Existing, Acc, Result) :-
% the following line could be written as:
% member(C, [1, 2, 3, 4, 5, 6, 7, 8, 9])
% as we did for the 8 queens problem
    colors(Cols), member(C,Cols), % choose a value for the current cell
    forall(member(s(Vs1,Cs1), Existing), % check that it does not conflict with pre-populated values
           (Cs1 \= C ; not(related(V, Vs1)))), % by checking that it is a different number, or the same number in a non-related cell
    forall(member(s(Vs2,Cs2), Acc), % idem, avoid conflicts with the solution found so far
           (Cs2 \= C ; not(related(V, Vs2)))),
    sudoku(Vr, Existing, [s(V,C) | Acc], Result). % if everything went well, add it to the accumulator

% Produce the board
board(B) :-
    colors(C),
    def_row_board(C, C, [], B).

% for i in rows
def_row_board([], _, S, S).
def_row_board([R | Rs], C, B, S) :-
    def_col_board(R, C, B, S1),
    def_row_board(Rs, C, S1, S).

% for j in cols: add vt(i, j) to the accumulator
def_col_board(_, [], S, S).
def_col_board(R, [C | Cs], B, S) :-
    def_col_board(R, Cs, [vt(R, C) | B], S).

% The pre-populated cells of the Wikipedia example (slides are also in this folder)
existing(E) :- E = [s(vt(1,1), 5), s(vt(1,2), 3), s(vt(1,5), 7),
                    s(vt(2,1), 6), s(vt(2,4), 1), s(vt(2,5), 9),
                    s(vt(2,6), 5), s(vt(3,2), 9), s(vt(3,3), 8),
                    s(vt(3,8), 6), s(vt(4,1), 8), s(vt(4,5), 6),
                    s(vt(4,9), 3), s(vt(5,1), 4), s(vt(5,4), 8),
                    s(vt(5,6), 3), s(vt(5,9), 1), s(vt(6,1), 7),
                    s(vt(6,5), 2), s(vt(6,9), 6), s(vt(7,2), 6),
                    s(vt(7,7), 2), s(vt(7,8), 8), s(vt(8,4), 4),
                    s(vt(8,5), 1), s(vt(8,6), 9), s(vt(8,9), 5),
                    s(vt(9,5), 8), s(vt(9,8), 7), s(vt(9,9), 9)].

% Check the current printing limits, and increase them (to see the entire solution):
% current_prolog_flag(toplevel_print_options, V).
% set_prolog_flag(toplevel_print_options, [quoted(true), portray(true), max_depth(100), priority(699)]).
