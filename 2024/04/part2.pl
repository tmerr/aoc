% Day 4 part 2 in swi prolog. Run with `swipl part2.pl`.
% Then `solution(Count).` from the interactive prompt.
% Character code reference:
%   CR: 13, LF: 10, M: 77, A: 65, S: 83.

% First part of the code: reading the data from the file.
% The grid is represented as a flat list of (Character, X, Y).
file_contents(Codes) :- read_file_to_codes("input.txt", Codes, []).
coords([], [], _).
coords([10|T], O, (_, Y)) :- Y1 is Y + 1, coords(T, O, (0, Y1)).
coords([13|T], O, X) :- coords(T, O, X).
coords([H|T], [(H, X, Y)|O], (X, Y)) :- H \= 10, H \= 13, X1 is X + 1, coords(T, O, (X1, Y)).

grid_from_file(Grid) :- file_contents(Codes), coords(Codes, Grid, (0, 0)).

:- table is_member/1.  % memoize.
is_member(A) :- grid_from_file(G), member(A, G).

rotated90(X, Y, OX, OY) :-
    OX = Y,
    OY is -X.

% An X-MAS is some rotation of
% M M
%  A
% S S
xmas(X, Y) :-
    is_member((65, X, Y)),
    ((DX1, DY1) = (-1, -1);
     (DX1, DY1) = (-1, 1);
     (DX1, DY1) = (1, -1);
     (DX1, DY1) = (1, 1)),
    rotated90(DX1, DY1, DX2, DY2),
    rotated90(DX2, DY2, DX3, DY3),
    rotated90(DX3, DY3, DX4, DY4),
    X1 is X + DX1,
    X2 is X + DX2,
    X3 is X + DX3,
    X4 is X + DX4,
    Y1 is Y + DY1,
    Y2 is Y + DY2,
    Y3 is Y + DY3,
    Y4 is Y + DY4,
    is_member((77, X1, Y1)),
    is_member((77, X2, Y2)),
    is_member((83, X3, Y3)),
    is_member((83, X4, Y4)).

solution(Count) :- aggregate_all(count, xmas(_, _), Count).