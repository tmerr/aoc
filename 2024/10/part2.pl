:- use_module(library(clpfd)).
:- use_module(library(reif)).

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

path(Vs, N) :-
    if_(N = 0,
        Vs = [_],
        (
            [U,V|T] = Vs,
            edge(U, V),
            N1 #= N - 1,
            path([V|T], N1)
        )).

solution(I) :-
    findall(
        Path,
        path(Path, 9),
        Paths
    ),
    list_to_set(Paths, S),
    length(S, I).