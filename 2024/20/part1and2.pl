:- use_module(library(clpfd)).
:- use_module('../lib/asciigrid.pl').
:- use_module('../lib/util.pl').

edge(U, V) :-
    member(Delta, [[0, 1], [0, -1], [1, 0], [-1, 0]]),
    vec_add(U, Delta, V),
    gridtile(U, 0'.),
    gridtile(V, 0'.).

:- table shortest_path_cost(_, _, _, min).
shortest_path_cost(_, S1, S1, 0).
shortest_path_cost(EdgePred, S1, S2, 1) :-
    call(EdgePred, S1, S2).
shortest_path_cost(EdgePred, S1, S3, N) :-
    call(EdgePred, S2, S3),
    shortest_path_cost(EdgePred, S1, S2, N1),
    N #= N1 + 1.

assert_tile(item(Coord, Char)) :-
    assertz(gridtile(Coord, Char)).

assert_facts :-
    phrase_from_file(grid_grammar(G), "input.txt"),
    grid_tile(G, Start, 0'S),
    grid_tile(G, End, 0'E),
    replace_grid_tile(G, Start, 0'., G1),
    replace_grid_tile(G1, End, 0'., G2),
    grid_enumerate(G2, Items),
    maplist(assert_tile, Items),
    assertz(start_tile(Start)),
    assertz(end_tile(End)).

% Push the grid into Prolog's DB for better performance.
:- assert_facts.

manhattan_distance([X1, Y1], [X2, Y2], D) :-
    D #= abs(X2 - X1) + abs(Y2 - Y1).

solution(CheatDuration, N) :-
    start_tile(Start),
    end_tile(End),
    shortest_path_cost(edge, Start, End, Baseline),
    findall(
        [U, V],
        (
            gridtile(U, 0'.),
            manhattan_distance(U, V, Manhattan),
            Manhattan #=< CheatDuration,
            gridtile(V, 0'.),
            shortest_path_cost(edge, Start, U, Cost1),
            shortest_path_cost(edge, End, V, Cost2),
            Savings #= Baseline - (Cost1 + Manhattan + Cost2),
            Savings #>= 100
        ),
        Result),
    sort(Result, Sorted), % dedup.
    length(Sorted, N).

part1(N) :- solution(2, N).
part2(N) :- solution(20, N).