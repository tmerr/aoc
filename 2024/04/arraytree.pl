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
    tree_nth0/3
]).

:- use_module(library(clpfd)).

nest([], []).
nest([A], [[A]]).
nest([A|[B|C]], [[A, B]|Nested]) :-
    nest(C, Nested).

repeated_nest([Tree], Tree, 0).
repeated_nest([A|B], Tree, N1) :-
    N1 #> 0,
    nest([A|B], Nested),
    N #= N1 - 1,
    repeated_nest(Nested, Tree, N).

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

tree_nth0(arraytree(L, 0), 0, L).
tree_nth0(arraytree([L], Depth), Index, Value) :-
    Depth #> 0,
    Depth1 #= Depth - 1,
    Index #< 2 ^ (Depth - 1),
    tree_nth0(arraytree(L, Depth1), Index, Value).
tree_nth0(arraytree([L, R], Depth), Index, Value) :-
    Depth #> 0,
    Depth1 #= Depth - 1,
    Index1 #= Index - (2 ^ Depth1),
    (
        (
            Index1 #< 0,
            tree_nth0(arraytree(L, Depth1), Index, Value)
        )
        ;
        (
            Index1 #>= 0,
            tree_nth0(arraytree(R, Depth1), Index1, Value)
        )
    ).