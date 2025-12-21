:- use_module('../../2024/lib/asciigrid').
:- use_module('../../2024/lib/util').

start(G, Coord) :-
    grid_tile(G, Coord, 0'S).

:- table beam/2.
beam(G, Coord) :-
    grid_tile(G, Coord, 0'.),
    (
        (
            (
                vec_add(Coord, [-1, 0], LeftOrRight)
                ;
                vec_add(Coord, [1, 0], LeftOrRight)
            ),
            active_splitter(G, LeftOrRight)
        )
        ;
        (
            vec_add(Coord, [0, -1], Above),
            (start(G, Above); beam(G, Above))
        )
    ).

:- table active_splitter/2.
active_splitter(G, Coord) :-
    grid_tile(G, Coord, 0'^),
    vec_add(Coord, [0, -1], Above),
    beam(G, Above).

solution(Total) :-
    phrase_from_file(grid_grammar(G), "input.txt"),
    aggregate_all(count, active_splitter(G, Coord), Total).