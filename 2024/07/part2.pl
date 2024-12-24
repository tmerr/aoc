:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(clpfd)).

operands(L) --> sequence(integer, white, L).
row(row(I, L)) --> integer(I), ":", white, operands(L), blanks_to_nl.
rows(L) --> sequence(row, L).

num_concat(A, B, Result) :-
    number_string(A, StrA),
    number_string(B, StrB),
    string_concat(StrA, StrB, ConcatStr),
    number_string(Result, ConcatStr).

any_eval_eq(I, I, []).
any_eval_eq(I, Acc, [X|Xs]) :-
    (
        (Acc1 is X + Acc, any_eval_eq(I, Acc1, Xs))
        ;
        (Acc2 is X * Acc, any_eval_eq(I, Acc2, Xs))
        ;
        (num_concat(Acc, X, Acc3), any_eval_eq(I, Acc3, Xs))
    ).

row_contribution(row(I, [X|Xs]), Contribution) :-
    (any_eval_eq(I, X, Xs)
    -> Contribution = I
    ; Contribution = 0).

solution(I) :-
    read_file_to_codes("input.txt", Codes, []),
    phrase(rows(Rows), Codes),
    maplist(row_contribution, Rows, Contributions),
    sum(Contributions, #=, I).