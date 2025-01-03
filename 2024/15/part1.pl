:- use_module(library(clpfd)).
:- use_module(library(reif)).
:- use_module(library(dcg/basics)).
:- use_module(asciigrid).
:- use_module(util).

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

state_string(state(Grid, _), Str) :-
    phrase(problem_grammar(problem(Grid, [])), Codes),
    string_codes(Str, Codes).

grid_as_state(Grid, state(Grid1, BotPos)) :-
    grid_tile(Grid, BotPos, 0'@),
    replace_grid_tile(Grid, BotPos, 0'., Grid1).

append_boulder(Grid, Pos, Delta, Grid1, OK) :-
    grid_tile(Grid, Pos, Char),
    if_(Char = 0'.,
    (
        OK = true,
        replace_grid_tile(Grid, Pos, 0'O, Grid1)
    ),
    if_(Char = 0'O,
    (
        vec_add(Pos, Delta, Pos1),
        append_boulder(Grid, Pos1, Delta, Grid1, OK)
    ),
    (
        OK = false,
        Grid = Grid1
    ))).

push_boulder(Grid, Pos, Delta, Grid1, OK) :-
    append_boulder(Grid, Pos, Delta, Grid0, OK),
    if_(OK = true,
        replace_grid_tile(Grid0, Pos, 0'., Grid1),
        Grid1 = Grid).

move_bot(state(Grid, BotPos), Delta, state(Grid1, BotPos1)) :-
    vec_add(BotPos, Delta, Ahead),
    push_boulder(Grid, Ahead, Delta, Grid1, OK),
    if_(OK = true,
        vec_add(BotPos, Delta, BotPos1),
        BotPos1 = BotPos).

moves(S, S) --> [].
moves(S, SFinal) --> [Delta], { move_bot(S, Delta, S1) }, moves(S1, SFinal).

tile_contribution(Grid, [X, Y], Contribution) :-
    grid_tile(Grid, [X, Y], Char),
    if_(Char = 0'O,
        Contribution #= 100 * Y + X,
        Contribution #= 0).

sum_box_gps_coordinates(state(Grid, _), Sum) :-
    grid(_, [W, H])= Grid,
    num_range(0, W, XRange),
    num_range(0, H, YRange),
    cartesian_product(XRange, YRange, Coords),
    maplist(tile_contribution(Grid), Coords, Contributions),
    sum(Contributions, #=, Sum).

solution :-
    phrase_from_file(problem_grammar(Problem), "input.txt"),
    problem(Grid, Moves) = Problem,
    grid_as_state(Grid, State),
    phrase(moves(State, SFinal), Moves),
    state_string(SFinal, Str),
    write("Final state:\n"),
    write(Str),
    write("Sum of GPS coords: "),
    sum_box_gps_coordinates(SFinal, Sum),
    write(Sum).