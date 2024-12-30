:- module(asciigrid, [
    file_contents/1,
    codes_as_grid/2,
    list_as_grid/3,
    grid_tile/3,
    replace_grid_tile/4,
    in_bounds_bool/3,
    fast_impure_in_bounds_bool/3,
    impure_grid_tile_or/4,
    coord_as_index/3,
    list_as_2d_list/3,
    print_grid_2d/1
]).

:- use_module(library(clpfd)).
:- use_module(library(reif)).
:- use_module(arraytree).

file_contents(Codes) :-
    read_file_to_codes("input.txt", Codes, []).

remove_newlines([], [], _, _).
remove_newlines([10|T], O, Acc, Acc) :-
    remove_newlines(T, O, 0, Acc).
remove_newlines([H|T], [H|O], Acc, LineLength) :-
    H #\= 10,
    Acc1 #= Acc + 1,
    remove_newlines(T, O, Acc1, LineLength).

codes_as_grid(Codes, G) :-
    remove_newlines(Codes, FlatList, 0, Width),
    length(FlatList, Len),
    Height #= Len // Width,
    list_as_grid(FlatList, [Width, Height], G).

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

impure_grid_tile_or(G, Or, Coord, Code) :-
    if_(fast_impure_in_bounds_bool(G, Coord),
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

bin_as_bool(0, false).
bin_as_bool(1, true).
in_bounds_bool(grid(_, Dims), Coord, Result) :-
    build_bounds_check(Dims, Coord, ClpdExpression),
    ClpdExpression #<==> B,
    bin_as_bool(B, Result).

% A hack because CLPFD reification is really slow.
fast_impure_in_bounds_bool(grid(_, Dims), Coord, Result) :-
    (maplist(component_in_bounds, Dims, Coord) 
    -> Result = true
    ; Result = false).

length_swapped(Len, Xs) :- length(Xs, Len).

list_as_2d_list(L, [W, H], Out) :-
    length(Out, H),
    maplist(length_swapped(W), Out),
    append(Out, L).

write_line(Xs) :-
    write("  "),
    write(Xs),
    write("\n").

print_grid_2d(grid(T, Dims)) :-
    write("printed grid:\n"),
    list_as_grid(L, Dims, grid(T, Dims)),
    list_as_2d_list(L, Dims, L2),
    maplist(write_line, L2).