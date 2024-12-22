:- use_module(library(clpfd)).
:- use_module(library(reif)).
:- use_module(arraytree).
:- use_module(asciigrid).

start_position(Grid, X, Y) :-
    grid_tile(Grid, X, Y, 0'^).

rot90(X, Y, X1, X) :- 
    X1 #= -Y.

walk(Grid, X, Y, DX, DY, Path) :-
    AheadX #= X + DX,
    AheadY #= Y + DY,
    if_(in_bounds_bool(Grid, AheadX, AheadY),
        (
            grid_tile(Grid, AheadX, AheadY, AheadTile),
            if_(AheadTile = 0'#,
                (
                    rot90(DX, DY, DX1, DY2),
                    walk(Grid, X, Y, DX1, DY2, Path)
                ),
                (
                    Path = [(X, Y)|Path1],
                    walk(Grid, AheadX, AheadY, DX, DY, Path1)
                ))
        ),
        Path = [(X, Y)]).

coord_as_index_helper(Width, Height, (X, Y), Index) :-
    coord_as_index(Width, Height, X, Y, Index).

solution(I) :-
    file_contents(Codes),
    list_as_grid(Codes, G),
    start_position(G, X, Y),
    walk(G, X, Y, 0, -1, Path),
    grid(_, Width, Height) = G,
    % A quick hack: convert grid coordinates to integers and get the
    % size of the set.
    maplist(coord_as_index_helper(Width, Height), Path, Indices),
    list_to_fdset(Indices, Fdset),
    fdset_size(Fdset, I).