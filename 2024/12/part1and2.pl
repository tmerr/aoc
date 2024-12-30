% I model the problem with constraints.
% Each grid tile is associated with a partition ID.
% Adjacent tiles have the same partition ID iff they are adjacent.
% Take some valid assignment of partition IDs that satisfy these constraints.

:- use_module(library(clpfd)).
:- use_module(library(reif)).
:- use_module(asciigrid).
:- use_module(util).

adjacents(G, [X, Y], Out) :-
    X1 #= X + 1,
    X2 #= X - 1,
    Y1 #= Y + 1,
    Y2 #= Y - 1,
    tfilter(fast_impure_in_bounds_bool(G),
            [[X, Y1], [X, Y2], [X1, Y], [X2, Y]],
            Out).

adjacent_constraints(G, PG, Coord1, Coord2) :-
    grid_tile(G, Coord1, T1),
    grid_tile(G, Coord2, T2),
    grid_tile(PG, Coord1, P1),
    grid_tile(PG, Coord2, P2),
    if_(T1 = T2,
        P1 = P2,
        dif(P1, P2)).

constraints_for_coord(G, PG, Coord) :-
    adjacents(G, Coord, Adj),
    maplist(adjacent_constraints(G, PG, Coord), Adj).

new_partition_id_grid(W, H, Out) :-
    Len #= W * H,
    length(Xs, Len),
    list_as_grid(Xs, [W, H], Out).

adjacent_fence(PG, Coord1, Coord2, B) :-
    grid_tile(PG, Coord1, P1),
    grid_tile(PG, Coord2, P2),
    if_(P1 = P2, B = 0, B = 1).

fences(PG, Coord, N) :-
    adjacents(PG, Coord, Adj),
    maplist(adjacent_fence(PG, Coord), Adj, M),
    sum(M, #=, N1),
    % Also count out of bounds edges.
    length(Adj, NumAdj),
    N #= N1 + (4 - NumAdj).

partition_contribution(PG, Coords, N) :-
    length(Coords, Area),
    maplist(fences(PG), Coords, X),
    sum(X, #=, TotalFences),
    N #= TotalFences * Area.

count_corners([_], 0).
count_corners([B1,B2,B3|T], N) :-
    if_([B1,B3] = [false, false],
        Delta = 1,
        if_([B1, B2, B3] = [true, false, true],
            Delta = 1,
            Delta = 0)),
    N #= N1 + Delta,
    count_corners([B3|T], N1).

% B is true when value at Coord is same as WantV, false otherwise.
eq_as_bool(PG, WantV, Coord, B) :-
    impure_grid_tile_or(PG, -1, Coord, V),
    =(V, WantV, B).

vec_add([X1, Y1], [X2, Y2], [X3, Y3]) :-
    X3 #= X1 + X2,
    Y3 #= Y1 + Y2.

fences2(PG, Coord, N) :-
    % Note: first element is also at the end.
    DeltaAdjacent8 = [[1, 0], [1, 1], [0, 1], [-1, 1], [-1, 0], [-1, -1], [0, -1], [1, -1], [1, 0]],
    maplist(vec_add(Coord), DeltaAdjacent8, Adjacent8),
    grid_tile(PG, Coord, WantV),
    maplist(eq_as_bool(PG, WantV), Adjacent8, Bools),
    count_corners(Bools, N).

partition_contribution2(PG, Coords, N) :-
    length(Coords, Area),
    maplist(fences2(PG), Coords, X),
    sum(X, #=, TotalFences),
    N #= TotalFences * Area.

solution(Part1, Part2) :-
    file_contents(Codes),
    codes_as_grid(Codes, G),
    G = grid(_, [W, H]),
    new_partition_id_grid(W, H, PG),
    num_range(0, W, Xs),
    num_range(0, H, Ys),
    cartesian_product(Xs, Ys, Coords),
    maplist(constraints_for_coord(G, PG), Coords),

    % Oops, so far this is a graph coloring program.
    % To avoid reuse of colors, unify each distinct var with an integer.
    list_as_grid(L, [W, H], PG),
    term_variables(L, DistinctVars),
    num_range(0, _, DistinctVars),

    % Group by partition ID.
    findall(
        PartitionID-Coord,
        grid_tile(PG, Coord, PartitionID),
        Found),
    sort(Found, Sorted),
    group_pairs_by_key(Sorted, CoordsByID),
    pairs_values(CoordsByID, CoordGroups),
    maplist(partition_contribution(PG), CoordGroups, Contributions),
    sum(Contributions, #=, Part1),
    maplist(partition_contribution2(PG), CoordGroups, Contributions2),
    sum(Contributions2, #=, Part2).