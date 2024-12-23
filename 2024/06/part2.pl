:- use_module(library(clpfd)).
:- use_module(library(reif)).
:- use_module(arraytree).
:- use_module(asciigrid).

start_position(Grid, Pos) :-
    grid_tile(Grid, Pos, 0'^).

rot90([X, Y], [X1, X]) :- 
    X1 #= -Y.

component_add(C1, C2, C3) :-
    C3 #= C1 + C2.

vec_add(U, V, W) :-
    maplist(component_add, U, V, W).

facing_ix([0, 1], 0).
facing_ix([0, -1], 1).
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

distribute(_, [], []).
distribute(X, [Y|Ys], [[X, Y]|Zs]) :-
    distribute(X, Ys, Zs).

cartesian_product([], _, []).
cartesian_product([X|Xs], Ys, Zs) :-
    distribute(X, Ys, Z),
    append(Z, Zs1, Zs),
    cartesian_product(Xs, Ys, Zs1).

/*
    MaxX #= X - 1, MaxY #= Y - 1,
    numlist(0, MaxX, Xs),
    numlist(0, MaxY, Ys),
    cartesian_product(Xs, Ys, Positions),
*/

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
    Width1 #= Width - 1,
    Height1 #= Height - 1,
    findall(
        [X, Y],
        (
            X in 0..20, Y in 0,
            label([X, Y]),
            replace_grid_tile(G, [X, Y], 0'#, G1),
            walk(G1, Pos, [0, -1], Visited, /* HasCycle */ 1)
        ),
        Sum
    ).