% Supports converting a list to/from a binary tree representation
% that has O(log(n)) indexing.
%
% Usage tips
% - length(Ls, <some int>), list_as_tree(Ls, T) determines the binary
%   tree's shape.
% - Determine the tree shape before using tree_nth0 to avoid
%   multiple solutions.
%
% That aside, list_as_tree and tree_nth0 work bidirectionally.
%
% author: Trevor Merrifield

:- module(arraytree, [
    list_as_tree/2,
    tree_nth0/3,
    replace_nth0/4
]).

:- use_module(library(clpfd)).

nest([], []).
nest([A], [t(A)]).
nest([A|[B|C]], [t(A, B)|Nested]) :-
    nest(C, Nested).

repeated_nest([t(A, B)], t(A, B), 0).
repeated_nest([t(A)], t(A), 0).
repeated_nest([A|B], Tree, Depth) :-
    Depth #> 0,
    nest([A|B], Nested),
    Depth1 #= Depth - 1,
    repeated_nest(Nested, Tree, Depth1).

depth_for_length(1, 1).
depth_for_length(Len, Depth) :-
    Len #> 1,
    Len #> 2 ^ (Depth - 1),
    Len #=< 2 ^ Depth,
    % Solve for variables if enough info is given.
    (integer(Depth) -> label([Len]); true),
    (integer(Len) -> Depth in 0..Len, label([Depth]); true).

list_as_tree([], arraytree([], 0)).
list_as_tree(List, arraytree(Tree, Depth)) :-
    depth_for_length(Len, Depth),
    length(List, Len),
    repeated_nest(List, Tree, Depth).

tree_nth0(L, 0, 0, L).
tree_nth0(t(L), Depth, Index, Value) :-
    Depth #> 0,
    Depth1 #= Depth - 1,
    Index #< 2 ^ (Depth - 1),
    tree_nth0(L, Depth1, Index, Value).
tree_nth0(t(L, R), Depth, Index, Value) :-
    Depth #> 0,
    Depth1 #= Depth - 1,
    Index1 #= Index - (2 ^ Depth1),
    (
        (
            Index1 #< 0,
            tree_nth0(L, Depth1, Index, Value)
        )
        ;
        (
            Index1 #>= 0,
            tree_nth0(R, Depth1, Index1, Value)
        )
    ).
tree_nth0(arraytree(T, Depth), Index, Value) :-
    tree_nth0(T, Depth, Index, Value).

% replace_nth0(T, Index, Value, T1) is true if T1 is T with the given element replaced.
replace_nth0(arraytree(T, Depth), Index, Value, arraytree(T1, Depth)) :-
    replace_nth0(T, Depth, Index, Value, T1).
replace_nth0(_, 0, 0, V, V).
replace_nth0(t(L), Depth, Index, Value, t(L1)) :-
    Depth #> 0,
    Depth1 #= Depth - 1,
    Index #< 2 ^ (Depth - 1),
    replace_nth0(L, Depth1, Index, Value, L1).
replace_nth0(t(L, R), Depth, Index, Value, t(L1, R1)) :-
    Depth #> 0,
    Depth1 #= Depth - 1,
    Index1 #= Index - (2 ^ Depth1),
    (
        (
            Index1 #< 0,
            R1 = R,
            replace_nth0(L, Depth1, Index, Value, L1)
        )
        ;
        (
            Index1 #>= 0,
            L1 = L,
            replace_nth0(R, Depth1, Index1, Value, R1)
        )
    ).