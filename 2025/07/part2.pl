:- use_module(library(reif)).
:- use_module(library(clpfd)).
:- use_module('../../2024/lib/asciigrid').
:- use_module('../../2024/lib/util').

start(G, Coord) :-
    grid_tile(G, Coord, 0'S).

:- table worlds/3.
worlds(G, Coord, N) :-
    G = grid(_, [_, H]),
    H1 #= H - 1,
    if_(Coord = [_, H1],
        N = 1,
        (
            vec_add(Coord, [0, 1], Down),
            grid_tile(G, Down, Ch),
            if_(Ch = 0'.,
                worlds(G, Down, N),
                (
                    Ch = 0'^,
                    vec_add(Down, [-1, 0], DownLeft),
                    vec_add(Down, [1, 0], DownRight),
                    worlds(G, DownLeft, N1),
                    worlds(G, DownRight, N2),
                    N #= N1 + N2
                )))).

solution(Total) :-
    phrase_from_file(grid_grammar(G), "input.txt"),
    start(G, StartCoord),
    worlds(G, StartCoord, Total).