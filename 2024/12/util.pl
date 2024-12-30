:- module(setproduct, [
    cartesian_product/3,
    num_range/3
]).

:- use_module(library(clpfd)).
:- use_module(library(reif)).

distribute(_, [], []).
distribute(X, [Y|Ys], [[X, Y]|Zs]) :-
    distribute(X, Ys, Zs).

cartesian_product([], _, []).
cartesian_product([X|Xs], Ys, Zs) :-
    distribute(X, Ys, Z),
    append(Z, Zs1, Zs),
    cartesian_product(Xs, Ys, Zs1).

% num_range(?Lo, ?HiExclusive, ?Out)
% Deterministic if Lo and HiExclusive are given.
% More versatile to between/3 in the standard lib.
num_range(Lo, HiExclusive, Out) :-
    Lo #=< HiExclusive,
    if_(Lo = HiExclusive,
        Out = [],
        (
            Lo1 #= Lo + 1,
            Out = [Lo|T],
            num_range(Lo1, HiExclusive, T)
        )).