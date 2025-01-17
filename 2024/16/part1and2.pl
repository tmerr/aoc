:- use_module(library(clpfd)).
:- use_module(library(reif)).
:- use_module('../lib/asciigrid').
:- use_module('../lib/util').
:- use_module('../lib/graphs').

vec_add([X1, Y1], [X2, Y2], [X3, Y3]) :-
    X3 #= X1 + X2,
    Y3 #= Y1 + Y2.

rot_right([A, B], [NegB, A]) :- NegB #= -B.

legal_state(G, edge(state(U, _), _), Bool) :-
    if_(in_bounds_bool(G, U),
    (
        grid_tile(G, U, Char),
        =(Char, 0'., Bool)
    ),
    Bool = false).

edges(G, state(Pos, Facing), Edges) :-
    rot_right(Facing, Facing1),
    rot_right(Facing2, Facing),
    vec_add(Pos, Facing, Ahead),
    vec_add(Pos, Facing1, Ahead1),
    vec_add(Pos, Facing2, Ahead2),
    Candidates = [
        edge(state(Ahead, Facing), 1),
        edge(state(Ahead1, Facing1), 1001),
        edge(state(Ahead2, Facing2), 1001)
    ],
    tfilter(legal_state(G), Candidates, Edges).

inputs(Grid, StartState, EndState) :-
    phrase_from_file(grid_grammar(G), "input.txt"),
    grid_tile(G, StartPos, 0'S),
    grid_tile(G, EndPos, 0'E),
    replace_grid_tile(G, StartPos, 0'., G1),
    replace_grid_tile(G1, EndPos, 0'., Grid),
    StartState = state(StartPos, [1, 0]),
    EndState = state(EndPos, [0, -1]).

part1(Cost) :-
    inputs(G, Start, End),
    dijkstras(edges(G), Start, End, Cost).

state_pos(state(Pos, _), Pos).

part2(NumTiles) :-
    inputs(G, Start, End),
    dijkstras_all_states_in_a_best_path(edges(G), Start, End, States),
    maplist(state_pos, States, Ps),
    sort(Ps, Ps1), % dedup.
    length(Ps1, NumTiles).