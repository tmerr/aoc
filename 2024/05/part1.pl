% Day 5 part 1 in swi Prolog.
% Run with `swipl part1.pl` then `solution(S).` in the interactive prompt.

% Parse the file using DCGs.
integer(I) -->
    digit(D0),
    digits(D),
    { number_codes(I, [D0|D]) }.
digits([]) --> [].
digits([D|T]) -->
    digit(D),
    digits(T).
digit(D) --> [D], { code_type(D, digit) }.
rule(A, B) --> integer(A), "|", integer(B).
rules([(A, B)]) --> rule(A, B).
rules([(A, B)|T]) --> rule(A, B), "\n", rules(T).
update([H]) --> integer(H).
update([H|T]) --> integer(H), ",", update(T).
updates([U]) --> update(U).
updates([U|T]) --> update(U), "\n", updates(T).
grammar(Rules, Updates) --> rules(Rules), "\n\n", updates(Updates), ("\n"; "").
parsed(Rules, Updates) :- phrase_from_file(grammar(Rules, Updates), "input.txt").

% Rule(A, B) semantics: if an update includes both A and B, then A must come before B.
check_rule(U, (A, B)) :-
    \+ (member(A, U), member(B, U));
    (
            nth0(IxA, U, A),
            nth0(IxB, U, B),
            IxA < IxB
    ).

update_matches(Rules, U) :-
    maplist(check_rule(U), Rules).

matching_updates(Updates) :-
    parsed(Rules, Us),
    include(update_matches(Rules), Us, Updates).

middle(XS, X) :-
    length(XS, L),
    Ix is div(L, 2),
    nth0(Ix, XS, X).

sum_middles([], 0).
sum_middles([U|Us], S) :-
    middle(U, M),
    sum_middles(Us, S1),
    S is M + S1.

solution(S) :-
    matching_updates(Us),
    sum_middles(Us, S).