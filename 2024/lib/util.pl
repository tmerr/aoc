:- module(setproduct, [
    cartesian_product/3,
    num_range/3,
    repeat/3,
    prefix/3
]).

:- use_module(library(clpfd)).
:- use_module(library(reif)).

distribute(d(_), [], []).
distribute(d(X), [Y|Ys], [[X, Y]|Zs]) :-
    distribute(d(X), Ys, Zs).

cartesian_product([], _, []).
cartesian_product([X|Xs], Ys, Zs) :-
    distribute(d(X), Ys, Z),
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

repeat(X, N, Xs) :-
    N #>= 0,
    if_(N = 0,
        Xs = [],
        (
            Xs = [X|T],
            N1 #= N - 1,
            repeat(X, N1, T)
        )).

prefix(N, Prefix, Whole) :-
    N #>= 0,
    if_(N = 0,
        Prefix = [],
        (
            [H|T]=Whole,
            [H|T1]=Prefix,
            N1 #= N - 1,
            prefix(N1, T1, T)
        )).