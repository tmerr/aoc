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
:- use_module(library(reif)).

nest([], []).
nest([A|T], [Node|Nested]) :-
    if_(T = [],
        (Node = t(A), nest(T, Nested)),
        ([B|T1] = T, Node = t(A, B), nest(T1, Nested))).

repeated_nest(Ls, Tree, Depth) :-
    Depth #>= 0,
    if_(Depth = 0,
        [Tree] = Ls,
        (
            nest(Ls, Nested),
            Depth1 #= Depth - 1,
            repeated_nest(Nested, Tree, Depth1)
        )).

depth_for_length(Len, Depth) :-
    if_(
        Len = 1,
        Depth = 1,
        (
            Len #> 1,
            Len #> 2 ^ (Depth - 1),
            Len #=< 2 ^ Depth,
            % Solve for variables if enough info is given.
            (integer(Depth) -> label([Len]); true),
            (integer(Len) -> Depth in 0..Len, label([Depth]); true)
        )
    ).

% Deterministic when list is given, nondet in the other direction.
list_as_tree([], arraytree([], 0)).
list_as_tree([H|T], arraytree(Tree, Depth)) :-
    depth_for_length(Len, Depth),
    length([H|T], Len),
    repeated_nest([H|T], Tree, Depth).

% Deterministic when Index is given.
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
    zcompare(Cmp, Index1, 0),
    if_(Cmp = <,
        tree_nth0(L, Depth1, Index, Value),
        tree_nth0(R, Depth1, Index1, Value)).
tree_nth0(arraytree(T, Depth), Index, Value) :-
    tree_nth0(T, Depth, Index, Value).

% replace_nth0(T, Index, Value, T1) is true if T1 is T with the given element replaced.
% Deterministic when Index is given.
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
    zcompare(Cmp, Index1, 0),
    if_(Cmp = <,
        (R1 = R, replace_nth0(L, Depth1, Index, Value, L1)),
        (L1 = L, replace_nth0(R, Depth1, Index1, Value, R1))).