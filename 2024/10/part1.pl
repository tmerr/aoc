:- use_module(library(clpfd)).

coords([], [], _).
coords([10|T], O, (_, Y)) :- Y1 is Y + 1, coords(T, O, (0, Y1)).
coords([13|T], O, X) :- coords(T, O, X).
coords([H|T], [grid([X, Y], N)|O], (X, Y)) :-
    H \= 10, H \= 13,
    number_codes(N, [H]),
    X1 is X + 1,
    coords(T, O, (X1, Y)).

assert_facts :-
    read_file_to_codes("input.txt", Codes, []),
    coords(Codes, Grid, (0, 0)),
    maplist(assertz, Grid).

:- assert_facts.

edge([X1, Y1], [X2, Y2]) :-
    grid([X1, Y1], N),
    grid([X2, Y2], N1),
    N1 #= N + 1,
    DX #= abs(X1 - X2),
    DY #= abs(Y1 - Y2),
    DX + DY #= 1.

% memoize to avoid infinite recursion.
:- table reachable/2.
reachable(U, W) :-
    edge(U, W).
reachable(U, W) :-
    reachable(U, V),
    reachable(V, W).

solution(I) :-
    findall(
        [U, V],
        (
            grid(U, 0),
            grid(V, 9),
            reachable(U, V)
        ),
        Pairs
    ),
    list_to_set(Pairs, S),
    length(S, I).