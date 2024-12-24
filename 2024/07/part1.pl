:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(clpfd)).
:- use_module(library(reif)).

operands(L) --> sequence(integer, white, L).
row(row(I, L)) --> integer(I), ":", white, operands(L), blanks_to_nl.
rows(L) --> sequence(row, L).

plus_expr(X, Expr, X+Expr).
times_expr(X, Expr, X*Expr).
exprs([X], [X]).
exprs([X,Y|Xs], Exprs) :-
    exprs([Y|Xs], Exprs1),
    maplist(plus_expr(X), Exprs1, L1),
    maplist(times_expr(X), Exprs1, L2),
    append(L1, L2, Exprs).

find_eq_or_0(_, [], 0).
find_eq_or_0(I, [H|T], Out) :-
    (I #= H) #<==> B,
    if_(B = 1,
        Out = I,
        find_eq_or_0(I, T, Out)).

row_contribution(row(I, Xs), Contribution) :-
    % exprs gives right-associative expressions, so need to reverse.
    reverse(Xs, RevXs),  
    exprs(RevXs, Exprs),
    maplist(#=, Evaluated, Exprs),
    find_eq_or_0(I, Evaluated, Contribution).

solution(I) :-
    read_file_to_codes("input.txt", Codes, []),
    phrase(rows(Rows), Codes),
    maplist(row_contribution, Rows, Contributions),
    sum(Contributions, #=, I).
