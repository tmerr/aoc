:- use_module(library(reif)).
:- use_module(library(clpfd)).

% Splits in half if even length.
try_split_in_half(Num, Left, Right, Ok) :-
    number_string(Num, Str),
    string_length(Str, L),
    M #= L mod 2,
    Half #= L // 2,
    if_(M = 0,
        (
            sub_string(Str, 0, Half, _, StrA),
            sub_string(Str, Half, Half, _, StrB),
            number_string(Left, StrA),
            number_string(Right, StrB),
            Ok = true
        ),
        (
            Left = -1,
            Right = -1,
            Ok = false
        )).

:- table blinkN/3.
blinkN(N, X, Out) :-
    if_(N = 0,
        Out = 1,
        (
            N1 #= N - 1,
            if_(X = 0,
                blinkN(N1, 1, Out),
                (
                    try_split_in_half(X, Left, Right, Ok),
                    if_(Ok = true,
                        (
                            blinkN(N1, Left, OutL),
                            blinkN(N1, Right, OutR),
                            Out #= OutL + OutR
                        ),
                        (
                            X1 #= 2024 * X,
                            blinkN(N1, X1, Out)
                        )
                    )
            ))
        )).

solution(Part1, Part2) :-
    ProblemInput = [2, 54, 992917, 5270417, 2514, 28561, 0, 990],
    maplist(blinkN(25), ProblemInput, Res1),
    sum(Res1, #=, Part1),
    maplist(blinkN(75), ProblemInput, Res2),
    sum(Res2, #=, Part2).