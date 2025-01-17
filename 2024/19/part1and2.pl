letter(C) --> [C], { code_type(C, alpha) }.
letters([C]) --> letter(C).
letters([C|Cs]) --> letter(C), letters(Cs).
patterns([P]) --> letters(P).
patterns([P|Ps]) --> letters(P), ", ", patterns(Ps).
designs([P]) --> letters(P).
designs([P|Ps]) --> letters(P), "\n", designs(Ps).
parsed(parsed(Patterns, Designs)) -->
    patterns(Patterns), "\n\n", designs(Designs), "\n".

input_data(PatternList, Designs) :-
    phrase_from_file(parsed(parsed(PatternList, Designs)), "input.txt").

:- table design_matches/2.
design_matches([], _).
design_matches([H|T], PatternList) :-
    member(P, PatternList),
    append(P, Rest, [H|T]),
    design_matches(Rest, PatternList).
check_design(PatternList, D) :-
    format("checking ~s\n", [D]),
    % clear memoization table now and then to limit memory usage.
    abolish_all_tables,
    design_matches(D, PatternList).

part1(Out) :- 
    input_data(PatternList, Designs),
    include(check_design(PatternList), Designs, Designs1),
    length(Designs1, Out).

:- table ways_to_match/3.
ways_to_match([], _, 1).
ways_to_match([H|T], PatternList, N) :-
    aggregate_all(
        sum(N1),
        (
            member(P, PatternList),
            append(P, Rest, [H|T]),
            ways_to_match(Rest, PatternList, N1)
        ),
        N).

check_ways_to_match(PatternList, D, OutP) :-
    format("checking ~s\n", [D]),
    % clear memoization table now and then to limit memory usage.
    abolish_all_tables,
    ways_to_match(D, PatternList, OutP).

part2(Count) :-
    input_data(PatternList, Designs),
    aggregate_all(
        sum(N), 
        (
            member(D, Designs),
            check_ways_to_match(PatternList, D, N)
        ),
        Count).