% Day 5 part 2 in swi Prolog.
% Run with `swipl part2.pl` then `solution(S).` in the interactive prompt.

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

% Logic to check whether an update satisfies a rule.
check_rule(U, (A, B)) :-
    \+ (member(A, U), member(B, U));
    (
            nth0(IxA, U, A),
            nth0(IxB, U, B),
            IxA < IxB
    ).

check_all_rules(Rules, U) :-
    maplist(check_rule(U), Rules).

% The bulk of the code here is for fixing updates that break the rules.
%
% My first try was to ask Prolog to find a permutation of the update that
% satisfied the rule. This turned out to be too slow, so I switched to a topological sort.
% It is based on the imperative algorithm in https://en.wikipedia.org/wiki/Topological_sorting.
%
% I also considered recursively swapping entries that did not satisfy the rules, but I wasnt sure
% how to guarantee it would terminate.

has_edge_from(A, (A, _)).

% A topological sort algorithm.
% kahns(S, E, L):
% S: Set of nodes with no incoming edge.
% E: Edges (rules).
% L: The output: a list of sorted elements.
kahns([], [], []).
kahns([H|S], E, [H|L]) :-
    exclude(has_edge_from(H), E, E1),
    findall(
        B, 
        (member((H, B), E), \+ (member((G, B), E), G \= H)),
        Bs),
    append(S, Bs, S1),
    kahns(S1, E1, L). 

relevant_rule(U, (A, B)) :- member(A, U), member(B, U).

relevant_rules(U, R1s, R2s) :-
    include(relevant_rule(U), R1s, R2s).

any_incoming_edge(Rules, V) :- member((_, V), Rules).

corrected_update(Rules, U, V) :-
    relevant_rules(U, Rules, Relevant),
    exclude(any_incoming_edge(Relevant), U, S),
    kahns(S, Relevant, V).

corrected_updates(Corrected) :-
    parsed(Rules, Us),
    exclude(check_all_rules(Rules), Us, Filtered),
    maplist(corrected_update(Rules), Filtered, Corrected).

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
    corrected_updates(Us),
    sum_middles(Us, S).