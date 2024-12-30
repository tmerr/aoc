:- use_module(library(clpfd)).

coords([], [], _).
coords([10|T], O, (_, Y)) :- Y1 is Y + 1, coords(T, O, (0, Y1)).
coords([13|T], O, X) :- coords(T, O, X).
coords([H|T], [grid([X, Y], H)|O], (X, Y)) :-
    H \= 10, H \= 13,
    X1 is X + 1,
    coords(T, O, (X1, Y)).

assert_facts :-
    read_file_to_codes("input.txt", Codes, []),
    coords(Codes, Grid, (0, 0)),
    maplist(assertz, Grid).

:- assert_facts.

edge([X1, Y1], [X2, Y2]) :-
    grid([X1, Y1], N),
    grid([X2, Y2], N),
    DX #= abs(X1 - X2),
    DY #= abs(Y1 - Y2),
    DX + DY #= 1.

unvisited_neighbors(U, Visited, Vs) :-
    findall(
        V,
        (
            edge(U, V),
            \+ rb_lookup(V, true, Visited)
        ),
        Vs
    ).

mark_visited(U, Visited, Visited1) :-
    rb_insert(Visited, U, true, Visited1).

% A level-by-level BFS.
closure([], [], Visited, Visited).
closure([], [H|T], Visited, VisitedOut) :-
    closure([H|T], [], Visited, VisitedOut).
closure([U|Frontier], NextLayer, Visited, VisitedOut) :-
    unvisited_neighbors(U, Visited, Vs),
    append(Vs, NextLayer, NextLayer1),
    foldl(mark_visited, Vs, Visited, Visited1),
    closure(Frontier, NextLayer1, Visited1, VisitedOut).
closure(Start, VisitedOut) :-
    rb_empty(Visited),
    closure([Start], [], Visited, VisitedOut).

% solution(I) :-