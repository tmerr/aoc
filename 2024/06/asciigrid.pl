:- module(asciigrid, [
    file_contents/1,
    list_as_grid/2,
    grid_tile/4,
    in_bounds_bool/4,
    coord_as_index/5
]).

:- use_module(library(clpfd)).
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

list_as_grid(Codes, grid(Tree, Width, Height)) :-
    remove_newlines(Codes, FlatList, 0, Width),
    length(FlatList, Len),
    Height #= Len // Width,
    list_as_tree(FlatList, Tree).

coord_as_index(Width, Height, X, Y, Index) :-
    X #>= 0, X #< Width,
    Y #>= 0, Y #< Height,
    Index #= Y * Width + X.

grid_tile(grid(Tree, Width, Height), X, Y, Code) :-
    coord_as_index(Width, Height, X, Y, Index),
    tree_nth0(Tree, Index, Code).

bin_as_bool(0, false).
bin_as_bool(1, true).
in_bounds_bool(grid(_, Width, Height), X, Y, Result) :-
    ((X #>= 0) #/\ (X #< Width) #/\ (Y #>= 0) #/\ (Y #< Height)) #<==> B,
    bin_as_bool(B, Result).
