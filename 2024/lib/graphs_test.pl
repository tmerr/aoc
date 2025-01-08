:- use_module(graphs).

adjacent(a, [edge(b, 1), edge(c, 1)]).
adjacent(b, [edge(d, 1), edge(e, 2)]).
adjacent(c, [edge(e, 1)]).
adjacent(d, []).
adjacent(e, []).

:- dijkstras(adjacent, a, e, 2).