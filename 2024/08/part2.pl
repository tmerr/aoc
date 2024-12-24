:- use_module(library(clpfd)).

coords([], [], _).
coords([10|T], O, (_, Y)) :- Y1 is Y + 1, coords(T, O, (0, Y1)).
coords([13|T], O, X) :- coords(T, O, X).
coords([H|T], [grid([X, Y], H)|O], (X, Y)) :- H \= 10, H \= 13, X1 is X + 1, coords(T, O, (X1, Y)).

assert_facts :-
    read_file_to_codes("input.txt", Codes, []),
    coords(Codes, Grid, (0, 0)),
    maplist(assertz, Grid).

:- assert_facts.

vec2_add([X1, Y1], [X2, Y2], [X3, Y3]) :-
    X3 #= X1 + X2,
    Y3 #= Y1 + Y2.

vec2_add_multiple([X1, Y1], K, [X2, Y2], [X3, Y3]) :-
    X3 #= X1 + K * X2,
    Y3 #= Y1 + K * Y2.

solution(I) :-
    findall(
        V1,
        (
            dif(V2, V3),
            vec2_add(V2, Delta, V3),
            vec2_add_multiple(V1, _, Delta, V2),
            dif(C, 0'.),
            grid(V2, C),
            grid(V3, C),
            grid(V1, _)
        ),
        Vs
    ),
    list_to_set(Vs, Vs1),
    length(Vs1, I).