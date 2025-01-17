:- use_module(library(clpfd)).
:- use_module(library(reif)).
:- use_module(library(dcg/basics)).
:- use_module('../lib/asciigrid').
:- use_module('../lib/util').
:- use_module('../lib/graphs').

point([X, Y]) --> integer(X), ",", integer(Y), blanks.
parsed([]) --> [].
parsed([[X,Y]|T]) --> point([X, Y]), parsed(T).

vec_add([X1, Y1], [X2, Y2], [X3, Y3]) :-
    X3 #= X1 + X2,
    Y3 #= Y1 + Y2.

weight1edge(U, edge(U, 1)).

enterable(G, U, B) :-
    if_(in_bounds_bool(G, U),
        (
            grid_tile(G, U, Code),
            =(Code, 0'., B)
        ),
        B = false).

edges(G, U, Edges) :-
    Deltas = [[0, -1], [0, 1], [-1, 0], [1, 0]],
    maplist(vec_add(U), Deltas, Vs),
    tfilter(enterable(G), Vs, Vs1),
    maplist(weight1edge, Vs1, Edges).

corrupt(V, Grid, Grid1) :-
    replace_grid_tile(Grid, V, 0'#, Grid1).

prepare_problem(Grid, AllVs) :-
    W = 71,
    Area #= W * W,
    repeat(0'., Area, Ls),
    list_as_grid(Ls, [W, W], Grid),
    phrase_from_file(parsed(AllVs), "input.txt").

part1(Cost) :-
    prepare_problem(Grid, AllVs),
    prefix(1024, Vs, AllVs),
    foldl(corrupt, Vs, Grid, Grid1),
    dijkstras(edges(Grid1), [0, 0], [70, 70], Cost).

% Pred says whether whether to go < (left), =, or >.
:- meta_predicate binary_search(2, ?, ?, ?).
binary_search(Pred, L, R, Out) :-
    M #= (L + R)//2,
    call(Pred, M, Cmp),
    if_(Cmp = (=),
        Out = M,
        if_(Cmp = <,
            (M1 #= M - 1, binary_search(Pred, L, M1, Out)),
            (M1 #= M + 1, binary_search(Pred, M1, R, Out)))).

end_is_reachable(Grid, AllVs, N, B) :-
    prefix(N, Vs, AllVs),
    foldl(corrupt, Vs, Grid, Grid1),
    dijkstras(edges(Grid1), [0, 0], [70, 70], Cost),
    if_(Cost = inf, B = false, B = true).

search_pred_cmp(true, true, >).
search_pred_cmp(true, false, =).
search_pred_cmp(false, false, <).
search_pred(Grid, AllVs, N, Cmp) :-
    N1 #= N - 1,
    end_is_reachable(Grid, AllVs, N1, B),
    end_is_reachable(Grid, AllVs, N, B1),
    search_pred_cmp(B, B1, Cmp).

part2(Coord) :-
    W = 71,
    Area #= W * W,
    repeat(0'., Area, Ls),
    list_as_grid(Ls, [W, W], Grid),

    phrase_from_file(parsed(AllVs), "input.txt"),
    length(AllVs, Len),
    binary_search(search_pred(Grid, AllVs), 1, Len, Ix),
    Ix1 #= Ix - 1,
    nth0(Ix1, AllVs, Coord).
