:- use_module(asciigrid).

test1 :-
    phrase(asciigrid:gridrow([1, 2, 3], 3), [1, 2, 3, 10]).
test2 :-
    phrase(asciigrid:gridrow([1, 2, 3], 3), _).
test3 :-
    phrase(asciigrid:gridrow(_, _), [1, 2, 3, 10]).
