:- use_module(library(clpq)).
:- use_module(library(reif)).
:- use_module(library(dcg/basics)).

mat_mult([[A, B], [C, D]], [E, F], [G, H]) :-
    {G = A*E + B*F,
    H = C*E + D*F}.

det([[A, B], [C, D]], Out) :-
    {Out = A*D - B*C}.

inverse(M, [[E, F], [G, H]], Invertable) :-
    det(M, Det),
    (
        entailed(Det =:= 0)
        -> Invertable = false
        ; Invertable = true
    ),
    if_(Invertable = true,
        (
            [[A, B], [C, D]] = M,
            {E = D / Det,
            F = -B / Det,
            G = -C / Det,
            H = A / Det}
        ),
        true
    ).

block(machine([Ax, Ay], [Bx, By], [Px, Py])) -->
    "Button A: X+", integer(Ax), ", Y+", integer(Ay), eol,
    "Button B: X+", integer(Bx), ", Y+", integer(By), eol,
    "Prize: X=", integer(Px), ", Y=", integer(Py), eol.
blocks([H|T]) --> block(H), blanks, blocks(T).
blocks([]) --> [].

tokens_to_win(machine([Ax, Ay], [Bx, By], [Px, Py]), Out) :-
    if_(inverse([[Ax, Bx], [Ay, By]], Inverted),
        (
            mat_mult(Inverted, [Px, Py], [OutX, OutY]),
            DenomX is denominator(OutX), 
            DenomY is denominator(OutY),
            if_([DenomX, DenomY] = [1, 1],
                % Note: Button A costs 3 tokens.
                {Out = 3 * abs(OutX) + abs(OutY)},
                Out = 0
            )
        ),
        Out = 0).

tokens_spent(Machines, Sum) :- 
    maplist(tokens_to_win, Machines, TokensPerMachine),
    foldl(plus, TokensPerMachine, 0, Sum).

hugify_prize([], []).
hugify_prize([machine(A, B, [Px, Py])|T],
            [machine(A, B, [Px1, Py1])|T1]) :-
    {Px1 = 10000000000000 + Px,
    Py1 = 10000000000000 + Py},
    hugify_prize(T, T1).

solution(Part1, Part2) :-
    read_file_to_codes("input.txt", Codes, []),
    phrase(blocks(Machines), Codes),
    tokens_spent(Machines, Part1),
    hugify_prize(Machines, Machines1),
    tokens_spent(Machines1, Part2).