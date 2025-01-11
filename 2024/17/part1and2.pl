:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(reif)).

parsed(regs(0, RegA, RegB, RegC, []), Program) -->
    parse_reg("A", RegA),
    parse_reg("B", RegB),
    parse_reg("C", RegC),
    blanks,
    parse_prog(Program).

parse_reg(Name, V) --> "Register ", Name, ": ", integer(V), "\n".

parse_prog(L) --> "Program: ", parse_comma_sep(L), "\n".
parse_comma_sep([H]) --> parse_op(H).
parse_comma_sep([H|T]) --> parse_op(H), ",", parse_comma_sep(T).

parse_op(op(adv, combo(V))) --> integer(0), ",", integer(V).
parse_op(op(bxl, literal(V))) --> integer(1), ",", integer(V).
parse_op(op(bst, combo(V))) --> integer(2), ",", integer(V).
parse_op(op(jnz, literal(V))) --> integer(3), ",", integer(V).
parse_op(op(bxc, literal(V))) --> integer(4), ",", integer(V).
parse_op(op(out, combo(V))) --> integer(5), ",", integer(V).
parse_op(op(bdv, combo(V))) --> integer(6), ",", integer(V).
parse_op(op(cdv, combo(V))) --> integer(7), ",", integer(V).

exec_op_(adv, V, regs(I, A, B, C, O), regs(I1, A1, B, C, O)) :-
    A1 #= A >> V,
    I1 #= I + 1.
exec_op_(bxl, V, regs(I, A, B, C, O), regs(I1, A, B1, C, O)) :-
    B1 #= B xor V,
    I1 #= I + 1.
exec_op_(bst, V, regs(I, A, _, C, O), regs(I1, A, B, C, O)) :-
    B #= V mod 8,
    I1 #= I + 1.
exec_op_(jnz, V, regs(I, A, B, C, O), regs(I1, A, B, C, O)) :-
    if_(A = 0, I1 #= I + 1, I1 #= V // 2).
exec_op_(bxc, _, regs(I, A, B, C, O), regs(I1, A, B1, C, O)) :-
    B1 #= B xor C,
    I1 #= I + 1.
exec_op_(out, V, regs(I, A, B, C, O), regs(I1, A, B, C, [H|O])) :-
    H #= V mod 8,
    I1 #= I + 1.
exec_op_(bdv, V, regs(I, A, _, C, O), regs(I1, A, B, C, O)) :-
    B #= A >> V,
    I1 #= I + 1.
exec_op_(cdv, V, regs(I, A, B, _, O), regs(I1, A, B, C, O)) :-
    C #= A >> V,
    I1 #= I + 1.

exec_op(op(Instr, literal(V)), Regs, Regs1) :-
    exec_op_(Instr, V, Regs, Regs1).
exec_op(op(Instr, combo(C)), Regs, Regs1) :-
    combo_val(Regs, C, V),
    exec_op_(Instr, V, Regs, Regs1).
combo_val(_, 0, 0).
combo_val(_, 1, 1).
combo_val(_, 2, 2).
combo_val(_, 3, 3).
combo_val(regs(_, RegA, _, _, _), 4, RegA).
combo_val(regs(_, _, RegB, _, _), 5, RegB).
combo_val(regs(_, _, _, RegC, _), 6, RegC).

exec(Program, Regs, RegsOut) :-
    regs(I, _, _, _, _) = Regs,
    length(Program, Len),
    zcompare(Cmp, I, Len),
    if_(Cmp = <,
        (
            nth0(I, Program, Op),
            exec_op(Op, Regs, Regs1),
            exec(Program, Regs1, RegsOut)
        ),
        % halt
        RegsOut = Regs).

% reverse list and convert to string.
output_str([H]) --> integer(H).
output_str([H|T]) --> output_str(T), ",", integer(H).

exec_has_output_codes(Program, Regs, Out) :-
    exec(Program, Regs, RegsOut),
    regs(_, _, _, _, O) = RegsOut,
    phrase(output_str(O), Out).

part1(OutStr) :-
    phrase_from_file(parsed(Regs, Program), "input.txt"),
    exec_has_output_codes(Program, Regs, OutCodes),
    string_codes(OutStr, OutCodes).

part2(InitialValueA) :-
    phrase_from_file(parsed(regs(I, _, B, C, O), Program), "input.txt"),
    phrase(parse_comma_sep(Program), ProgCodes),
    exec_has_output_codes(Program, regs(I, InitialValueA, B, C, O), ProgCodes),
    once(labeling([bisect, min(InitialValueA)], [InitialValueA])).