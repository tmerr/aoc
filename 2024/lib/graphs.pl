:- module(graphs, [
    dijkstras/4,
    dijkstras_all_states_in_a_best_path/4
]).
:- use_module(library(clpfd)).
:- use_module(library(heaps)).

expand_frontier(Parent, BaseCost, edge(Child, Weight), Q, Q1) :-
    Cost #= BaseCost + Weight,
    add_to_heap(Q, Cost, node(Child, Parent), Q1).

:- meta_predicate dijkstras_(0, ?, ?, ?).
dijkstras_(EdgesPred, Frontier, Parents, ParentsOut) :-
    get_from_heap(Frontier, Cost, node(S, Parent), Frontier1)
    -> (
        rb_lookup(S, _, Parents)
        -> dijkstras_(EdgesPred, Frontier1, Parents, ParentsOut)
        ; (
            rb_insert_new(Parents, S, (Parent, Cost), Parents1),
            call(EdgesPred, S, Edges),
            foldl(expand_frontier(S, Cost), Edges, Frontier1, Frontier2),
            dijkstras_(EdgesPred, Frontier2, Parents1, ParentsOut)
        )
    ) ; ParentsOut = Parents.

:- meta_predicate dijkstras(0, ?, ?, ?).
dijkstras(EdgesPred, Start, End, Cost) :-
    singleton_heap(Frontier, 0, node(Start, null)),
    rb_empty(Parents),
    dijkstras_(EdgesPred, Frontier, Parents, ParentsOut),
    rb_in(End, (_, Cost), ParentsOut).

:- meta_predicate dijkstras_all_paths_(0, ?, ?, ?).
dijkstras_all_paths_(EdgesPred, Frontier, Parents, ParentsOut) :-
    get_from_heap(Frontier, Cost, node(S, Parent), Frontier1)
    -> (
        (
            (\+ rb_lookup(S, _, Parents), SParents = [])
            ; (
                rb_lookup(S, (SParents, Cost), Parents),
                \+ member(Parent, SParents)
            )
        ) -> (
            rb_insert(Parents, S, ([Parent|SParents], Cost), Parents1),
            call(EdgesPred, S, Edges),
            foldl(expand_frontier(S, Cost), Edges, Frontier1, Frontier2),
            dijkstras_all_paths_(EdgesPred, Frontier2, Parents1, ParentsOut)
        ) ; dijkstras_all_paths_(EdgesPred, Frontier1, Parents, ParentsOut)
    ) ; ParentsOut = Parents.

walk_parents_(Parents, S, OutStates) :-
    S == null
    -> OutStates = []
    ; (
        rb_lookup(S, (SParents, _), Parents),
        maplist(walk_parents_(Parents), SParents, OutList),
        append(OutList, Out1),
        Out2 = [S|Out1],
        % deduplicate.
        sort(Out2, OutStates)
    ).

:- meta_predicate dijkstras_all_states_in_a_best_path(0, ?, ?, ?).
dijkstras_all_states_in_a_best_path(EdgesPred, Start, End, States) :-
    singleton_heap(Frontier, 0, node(Start, null)),
    rb_empty(EmptyParents),
    dijkstras_all_paths_(EdgesPred, Frontier, EmptyParents, Parents),
    walk_parents_(Parents, End, States).