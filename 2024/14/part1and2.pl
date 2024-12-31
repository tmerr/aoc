:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).
:- use_module(library(reif)).
:- use_module(util).

robot([Px,Py], [Vx, Vy]) -->
    "p=", integer(Px), ",", integer(Py), " v=", integer(Vx), ",", integer(Vy).
robots([robot(U, V)|T]) --> robot(U, V), blanks, robots(T).
robots([]) --> [].

:- det(move_bot/4).
move_bot([W, H], Seconds, robot([Px, Py], [Vx, Vy]), robot([Px1, Py1], [Vx, Vy])) :-
    Px1 #= (Px + Seconds*Vx) mod W,
    Py1 #= (Py + Seconds*Vy) mod H.

:- det(move_bots/4).
move_bots([W, H], Robots, Seconds, Robots1) :-
    maplist(move_bot([W, H], Seconds), Robots, Robots1).

% Get a vector with one component per quadrant.
% delta([Left, Right, Up, Down], Delta)
delta([0, 0, _, _], [0, 0, 0, 0]).
delta([_, _, 0, 0], [0, 0, 0, 0]).
delta([1, 0, 1, 0], [1, 0, 0, 0]).
delta([1, 0, 0, 1], [0, 1, 0, 0]).
delta([0, 1, 1, 0], [0, 0, 1, 0]).
delta([0, 1 ,0, 1], [0, 0, 0, 1]).
delta_for_quadrant([W, H], robot([Px, Py], _), Delta) :-
    MidX #= (W-1)//2,
    MidY #= (H-1)//2,
    Px #=< (MidX - 1) #<==> Left,
    Px #>= (MidX + 1) #<==> Right,
    Py #=< (MidY - 1) #<==> Up,
    Py #>= (MidY + 1) #<==> Down,
    delta([Left, Right, Up, Down], Delta).

add(A, B, C) :- C #= A + B.
mul(A, B, C) :- C #= A * B.

vec_add(U, V, W) :-
    maplist(add, U, V, W).

initial_state_from_file(Dims, Robots) :-
    read_file_to_codes("input.txt", Codes, []),
    phrase(robots(Robots), Codes),
    Dims = [101, 103].

part1_solution(Part1) :-
    initial_state_from_file(Dims, Robots),
    move_bots(Dims, Robots, 100, Robots1),
    maplist(delta_for_quadrant(Dims), Robots1, Deltas),
    foldl(vec_add, Deltas, [0,0,0,0], QuadrantSums),
    foldl(mul, QuadrantSums, 1, Part1).

add_robot(robot([X, Y], _), Tree, Tree1) :-
    rb_insert(Tree, [X, Y], 0'#, Tree1).
to_ascii(Tree, [W, H], X, Y, Out) :-
    if_(X = W,
        (
            Y1 #= Y + 1,
            Out = [0'\n|Out1],
            to_ascii(Tree, [W, H], 0, Y1, Out1)
        ),
        if_(Y = H,
            Out = [],
            (
                X1 #= X + 1,
                (
                    rb_lookup([X, Y], Char, Tree)
                    ;
                    Char = 0' 
                ),
                Out = [Char|Out1],
                to_ascii(Tree, [W, H], X1, Y, Out1)
            ))).

num_vertically_adjacent([W, H], [robot([X1, Y1], _)|T], Acc, N) :-
    if_(T = [],
        N = Acc,
        (
            [robot([X2, Y2], _)|_] = T,
            DiffX #= abs(X2 - X1),
            DiffY #= abs(Y2 - Y1),
            if_([DiffX, DiffY] = [0, 1],
                Acc1 #= Acc + 1,
                Acc1 #= Acc),
            num_vertically_adjacent([W, H], T, Acc1, N)
        )).
num_vertically_adjacent([W, H], Robots, N) :-
    sort(Robots, Robots1),
    num_vertically_adjacent([W, H], Robots1, 0, N).

display_bots([W, H], Robots) :-
    rb_new(Tree1),
    foldl(add_robot, Robots, Tree1, Tree2),
    to_ascii(Tree2, [W, H], 0, 0, Chars),
    string_codes(S, Chars),
    write(S),
    write("\n").

part2_move_robots_loop(Dims, Ix, MaxVertical, Robots) :-
    move_bots(Dims, Robots, 1, Robots1),
    num_vertically_adjacent(Dims, Robots1, NumVertical),
    Ix1 #= Ix + 1,
    NumVertical #>= MaxVertical #<==> Gt,
    if_(Gt = 1,
        (
            MaxVertical1 = NumVertical,
            write("new max vertical at seconds: "),
            write(Ix1),
            write(", value: "),
            write(NumVertical),
            write("\n"),
            display_bots(Dims, Robots1)
        ),
        (
            MaxVertical1 = MaxVertical
        )),
    part2_move_robots_loop(Dims, Ix1, MaxVertical1, Robots1).

part2_solution :-
    initial_state_from_file(Dims, Robots),
    part2_move_robots_loop(Dims, 0, 0, Robots).