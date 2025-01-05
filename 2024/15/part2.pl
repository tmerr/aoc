:- use_module(library(clpfd)).
:- use_module(library(reif)).
:- use_module(library(dcg/basics)).
:- use_module('../lib/asciigrid').
:- use_module('../lib/util').

preprocess_grammar([0'\n|T]) --> `\n`, preprocess_grammar(T).
preprocess_grammar([0'#, 0'#|T]) --> `#`, preprocess_grammar(T).
preprocess_grammar([0'[, 0']|T]) --> `O`, preprocess_grammar(T).
preprocess_grammar([0'., 0'.|T]) --> `.`, preprocess_grammar(T).
preprocess_grammar([0'@, 0'.|T]) --> `@`, preprocess_grammar(T).
preprocess_grammar([0'^|T]) --> `^`, preprocess_grammar(T).
preprocess_grammar([0'v|T]) --> `v`, preprocess_grammar(T).
preprocess_grammar([0'<|T]) --> `<`, preprocess_grammar(T).
preprocess_grammar([0'>|T]) --> `>`, preprocess_grammar(T).
preprocess_grammar([]) --> [].

move([0, -1]) --> `^`.
move([0, 1]) --> `v`.
move([-1, 0]) --> `<`.
move([1, 0]) --> `>`.
moves([]) --> [].
moves([H|T]) --> move(H), blanks, moves(T).
problem_grammar(problem(Grid, Moves)) --> grid_grammar(Grid), `\n`, moves(Moves).

vec_add([X1, Y1], [X2, Y2], [X3, Y3]) :-
    X3 #= X1 + X2,
    Y3 #= Y1 + Y2.

state_string(state(Grid, BotPos), Str) :-
    replace_grid_tile(Grid, BotPos, 0'@, Grid1),
    phrase(problem_grammar(problem(Grid1, [])), Codes),
    string_codes(Str, Codes).

grid_as_state(Grid, state(Grid1, BotPos)) :-
    grid_tile(Grid, BotPos, 0'@),
    replace_grid_tile(Grid, BotPos, 0'., Grid1).

box_sides(0'[, Left, Left, Right) :-
    vec_add(Left, [1, 0], Right).
box_sides(0'], Right, Left, Right) :-
    vec_add(Left, [1, 0], Right).
affected_boxes(Grid, Pos, Delta, Affected, OK) :-
    grid_tile(Grid, Pos, Char),
    if_(Char = 0'.,
    (
        OK = true,
        Affected = []
    ),
    if_(memberd_t(Char, `[]`),
    (
        [DeltaX, _] = Delta,
        if_(DeltaX = 0,
        (
            % vertical
            box_sides(Char, Pos, L, R),
            vec_add(L, Delta, AheadL),
            vec_add(R, Delta, AheadR),
            affected_boxes(Grid, AheadL, Delta, AffectedL, OK_L),
            affected_boxes(Grid, AheadR, Delta, AffectedR, OK_R),
            if_([OK_L, OK_R] = [true, true],
                (
                    OK = true,
                    append(AffectedL, AffectedR, AffectedLR),
                    Affected = [affected(L, 0'[), affected(R, 0']) | AffectedLR]
                ),
                (
                    OK = false,
                    Affected = []
                ))
        ),
        (
            % horizontal
            vec_add(Pos, Delta, Ahead),
            affected_boxes(Grid, Ahead, Delta, Affected1, OK),
            if_(OK = true,
                Affected = [affected(Pos, Char)|Affected1],
                Affected = [])
        ))
    ),
    (
        OK = false,
        Affected = []
    ))).

clear_tiles(affected(Pos, _), Grid, Grid1) :-
    replace_grid_tile(Grid, Pos, 0'., Grid1).

place_tiles(Delta, affected(Pos, Char), Grid, Grid1) :-
    vec_add(Pos, Delta, Pos1),
    replace_grid_tile(Grid, Pos1, Char, Grid1).

move_bot(state(Grid, BotPos), Delta, state(Grid1, BotPos1)) :-
    vec_add(BotPos, Delta, Ahead),
    affected_boxes(Grid, Ahead, Delta, Affected, OK),
    if_(OK = true,
        (
            BotPos1 = Ahead,
            sort(0, @<, Affected, AffectedDeduped),
            foldl(clear_tiles, AffectedDeduped, Grid, Grid0),
            foldl(place_tiles(Delta), AffectedDeduped, Grid0, Grid1)
        ),
        (BotPos1 = BotPos, Grid = Grid1)).

moves(S, SFinal) --> [Delta], { move_bot(S, Delta, S1) }, moves(S1, SFinal).
moves(S, S) --> [].

tile_contribution(item([X, Y], Char), Contribution) :-
    if_(Char = 0'[,
        Contribution #= 100 * Y + X,
        Contribution #= 0).

sum_box_gps_coordinates(state(Grid, _), Sum) :-
    grid_enumerate(Grid, Items),
    maplist(tile_contribution, Items, Contributions),
    sum(Contributions, #=, Sum).

solution :-
    phrase_from_file(preprocess_grammar(Preprocessed), "input.txt"),
    phrase(problem_grammar(Problem), Preprocessed),
    problem(Grid, Moves) = Problem,
    grid_as_state(Grid, State),
    phrase(moves(State, SFinal), Moves),
    state_string(SFinal, Str),
    write("Final state:\n"),
    write(Str),
    write("Sum of GPS coords: "),
    sum_box_gps_coordinates(SFinal, Sum),
    write(Sum).