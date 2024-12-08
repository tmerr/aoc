% Day 4 part 1 in swi prolog. Run with `swipl part1.pl`.
% Then `solution(Count).` from the interactive prompt.
% Character code reference:
%   CR: 13, LF: 10, X: 88, M: 77, A: 65, S: 83.

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

xmas_streak(X, Y, DX, DY) :-
    is_member((88, X, Y)),
    is_member((77, X1, Y1)),
    X1 is X + 1*DX,
    Y1 is Y + 1*DY,
    is_member((65, X2, Y2)),
    X2 is X + 2*DX,
    Y2 is Y + 2*DY,
    is_member((83, X3, Y3)),
    X3 is X + 3*DX,
    Y3 is Y + 3*DY.

% Look for XMAS in every direction.
% Reminder: semicolon is OR. comma is AND. \+ is NOT.
xmas(X, Y, DX, DY) :-
    ((DX = -1; DX = 0; DX = 1), (DY = -1; DY = 0; DY = 1)),
    \+ (DX = 0, DY = 0),
    xmas_streak(X, Y, DX, DY).

solution(Count) :- aggregate_all(count, xmas(_, _, _, _), Count).