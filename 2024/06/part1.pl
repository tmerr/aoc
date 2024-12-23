:- module(part1, [
    part1_walk/5
]).
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

part1_walk(Grid, Pos, Facing, Visited, VisitedOut) :-
    vec_add(Pos, Facing, Ahead),
    if_(in_bounds_bool(Grid, Ahead),
        (
            grid_tile(Grid, Ahead, AheadTile),
            if_(AheadTile = 0'#,
                (
                    rot90(Facing, Facing1),
                    part1_walk(Grid, Pos, Facing1, Visited, VisitedOut)
                ),
                (
                    replace_grid_tile(Visited, Pos, 1, Visited1),
                    part1_walk(Grid, Ahead, Facing, Visited1, VisitedOut)
                ))
        ),
        replace_grid_tile(Visited, Pos, 1, VisitedOut)).

solution(I) :-
    file_contents(Codes),

    codes_as_grid(Codes, G),
    grid(_, [Width, Height]) = G,
    VisitedLen #= Width * Height,
    length(VisitedList, VisitedLen),
    maplist(=(0), VisitedList),
    list_as_grid(VisitedList, [Width, Height], Visited),

    start_position(G, Pos),
    part1_walk(G, Pos, [0, -1], Visited, VisitedOut),
    length(VisitedOutList, VisitedLen),
    list_as_grid(VisitedOutList, [Width, Height], VisitedOut),
    sum(VisitedOutList, #=, I).