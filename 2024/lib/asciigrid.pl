:- module(asciigrid, [
    grid_grammar/3,
    list_as_grid/3,
    grid_tile/3,
    grid_enumerate/2,
    replace_grid_tile/4,
    in_bounds/2,
    in_bounds_bool/3,
    grid_tile_or/4,
    coord_as_index/3
]).

:- use_module(library(clpfd)).
:- use_module(library(reif)).
:- use_module(arraytree).
:- use_module(util).

grid_grammar(grid(T, [W, H])) -->
    {
        % trick for termination when grid is given.
        (integer(H) -> length(L2D, H); true),
        (integer(W) -> maplist(revlength(W), L2D) ; true)
    },
    grid_grammar_2(L2D, W),
    {
        length(L2D, H),
        maplist(revlength(W), L2D),
        append(L2D, L),
        list_as_grid(L, [W, H], grid(T, [W, H]))
    }.
revlength(Len, L) :- length(L, Len).
grid_grammar_2([], _) --> [].
grid_grammar_2([H|T], W) --> grid_row(H, W), grid_grammar_2(T, W).
grid_row(Res, Width, [Char|F], H) :-
    % trick to make grid_row det in both directions.
    (Res == [] -> Char = 0'\n ; true),
    if_(Char = 0'\n,
        (
            Res = [],
            Width = 0,
            F = H
        ),
        (
            Res = [Char|T],
            Width #= Width1 + 1,
            grid_row(T, Width1, F, H)
        )).

list_as_grid(List, Dims, grid(Tree, Dims)) :-
    list_as_tree(List, Tree).

component_in_bounds(Dim, Component) :-
    Component #>= 0, Component #< Dim.

calc_index([], _, [], Acc, Acc).
calc_index([D|Dims], Factor, [Ix|Ixes], Acc, Out) :-
    Acc1 #= Acc + Factor * Ix,
    Factor1 #= D * Factor,
    calc_index(Dims, Factor1, Ixes, Acc1, Out).

coord_as_index(Dims, Coord, Index) :-
    maplist(component_in_bounds, Dims, Coord),
    calc_index(Dims, 1, Coord, 0, Index).

grid_tile_or(G, Or, Coord, Code) :-
    if_(in_bounds_bool(G, Coord),
        grid_tile(G, Coord, Code),
        Code = Or).

grid_tile(grid(Tree, Dims), Coord, Code) :-
    coord_as_index(Dims, Coord, Index),
    label(Coord),
    label([Index]),
    tree_nth0(Tree, Index, Code).

replace_grid_tile(grid(T1, Dims), Coord, Code, grid(T2, Dims)) :-
    coord_as_index(Dims, Coord, Index),
    replace_nth0(T1, Index, Code, T2).

build_bounds_check([D], [C], (C #>= 0) #/\ (C #< D)).
build_bounds_check([D,D2|Dims], [C,C2|Coord], ((C #>= 0) #/\ (C #< D)) #/\ T) :-
    build_bounds_check([D2|Dims], [C2|Coord], T).

in_bounds(grid(_, Dims), Coord) :-
    maplist(component_in_bounds, Dims, Coord).

bin_as_bool(0, false).
bin_as_bool(1, true).
in_bounds_bool(grid(_, Dims), Coord, Result) :-
    (ground(Coord), ground(Dims))
    -> (
        % Fast path because CLPFD reification is slow.
        (maplist(component_in_bounds, Dims, Coord) 
        -> Result = true
        ; Result = false)
    )
    ; (
        build_bounds_check(Dims, Coord, ClpdExpression),
        ClpdExpression #<==> B,
        bin_as_bool(B, Result)
    ).

enum_item(G, Coord, item(Coord, Char)) :-
    grid_tile(G, Coord, Char).
grid_enumerate(Grid, Result) :-
    grid(_, [W, H]) = Grid,
    num_range(0, W, XRange),
    num_range(0, H, YRange),
    cartesian_product(XRange, YRange, Coords),
    maplist(enum_item(Grid), Coords, Result).
