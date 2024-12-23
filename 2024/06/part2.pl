:- use_module(library(clpfd)).
:- use_module(library(reif)).
:- use_module(arraytree).
:- use_module(asciigrid).
:- use_module(part1).

start_position(Grid, Pos) :-
    grid_tile(Grid, Pos, 0'^).

rot90([X, Y], [X1, X]) :- 
    X1 #= -Y.

component_add(C1, C2, C3) :-
    C3 #= C1 + C2.

vec_add(U, V, W) :-
    maplist(component_add, U, V, W).

det(facing_ix/2, visited_key/3).
facing_ix([0, X], R) :-
    if_(X = 1,
        R = 0,
        (X = -1, R = 1)).
facing_ix([1, 0], 2).
facing_ix([-1, 0], 3).
visited_key(Pos, Facing, [FacingIx|Pos]) :-
    facing_ix(Facing, FacingIx).

walk(Grid, Pos, Facing, Visited, HasCycle) :-
    visited_key(Pos, Facing, VisitedKey),
    grid_tile(Visited, VisitedKey, VisitedTile),
    if_(VisitedTile = 1,
        HasCycle = 1,
        (
            replace_grid_tile(Visited, VisitedKey, 1, Visited1),
            vec_add(Pos, Facing, Ahead),
            if_(fast_impure_in_bounds_bool(Grid, Ahead),
                (
                    grid_tile(Grid, Ahead, AheadTile),
                    if_(AheadTile = 0'#,
                        (
                            rot90(Facing, Facing1),
                            walk(Grid, Pos, Facing1, Visited1, HasCycle)
                        ),
                        walk(Grid, Ahead, Facing, Visited1, HasCycle))
                ),
                HasCycle = 0
            )
        )
    ).

obstacle_candidates(Grid, StartPos, Candidates) :-
    grid(_, [Width, Height]) = Grid,
    VisitedLen #= Width * Height,
    length(VisitedList, VisitedLen),
    maplist(=(0), VisitedList),
    list_as_grid(VisitedList, [Width, Height], Visited),
    part1_walk(Grid, StartPos, [0, -1], Visited, VisitedOut),
    findall(
        Coord,
        grid_tile(VisitedOut, Coord, 1),
        Candidates
    ).

test_position(G, StartPos, Visited, ObstaclePos, HasCycle) :-
    write("testing position:\n"),
    write(ObstaclePos),
    write("\n"),
    replace_grid_tile(G, ObstaclePos, 0'#, G1),
    walk(G1, StartPos, [0, -1], Visited, HasCycle).

solution(Sum) :-
    file_contents(Codes),

    codes_as_grid(Codes, G),
    grid(_, [Width, Height]) = G,

    VisitedDims = [4, Width, Height],
    VisitedLen #= Width * Height * 4,
    length(VisitedList, VisitedLen),
    maplist(=(0), VisitedList),
    list_as_grid(VisitedList, VisitedDims, Visited),

    start_position(G, Pos),
    write("found start position\n"),
    obstacle_candidates(G, Pos, Candidates),
    write("found obstacle candidates\n"),
    maplist(test_position(G, Pos, Visited), Candidates, HasCycle),
    sum(HasCycle, #=, Sum).