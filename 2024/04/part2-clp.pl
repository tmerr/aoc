% Day 4 part 2 in swi prolog. Run with `swipl part2-clp.pl`.
% Then `solution(Count).` from the interactive prompt.
% Character code reference:
%   LF: 10, M: 77, A: 65, S: 83.

:- use_module(library(clpfd)).
:- use_module(arraytree).

file_contents(Codes) :-
    read_file_to_codes("input.txt", Codes, []).

remove_newlines([], [], _, _).
remove_newlines([10|T], O, Acc, Acc) :-
    remove_newlines(T, O, 0, Acc).
remove_newlines([H|T], [H|O], Acc, LineLength) :-
    H #\= 10,
    Acc1 #= Acc + 1,
    remove_newlines(T, O, Acc1, LineLength).

codes_as_grid(Codes, grid(Tree, Width, Height)) :-
    remove_newlines(Codes, FlatList, 0, Width),
    length(FlatList, Len),
    Height #= Len // Width,
    list_as_tree(FlatList, Tree).

coord_as_index(Width, Height, X, Y, Index) :-
    X #>= 0, X #< Width,
    Y #>= 0, Y #< Height,
    Index #= Y * Width + X.

char_at(grid(Tree, Width, Height), X, Y, Code) :-
    coord_as_index(Width, Height, X, Y, Index),
    tree_nth0(Tree, Index, Code).

xmas(Grid, [X, Y], IsXmas) :-
    char_at(Grid, X, Y, C0),
    X1 #= X - 1,
    X2 #= X + 1,
    Y1 #= Y - 1,
    Y2 #= Y + 1,
    char_at(Grid, X1, Y1, C1),
    char_at(Grid, X2, Y1, C2),
    char_at(Grid, X2, Y2, C3),
    char_at(Grid, X1, Y2, C4),
    ((C0 #= 65) #/\ (
        % Look for any of four rotations of
        % M M
        % S S
        (((C1 #= 77) #/\ (C2 #= 77)) #/\ (C3 #= 83) #/\ (C4 #= 83))
        #\/ (((C2 #= 77) #/\ (C3 #= 77)) #/\ (C4 #= 83) #/\ (C1 #= 83))
        #\/ (((C3 #= 77) #/\ (C4 #= 77)) #/\ (C1 #= 83) #/\ (C2 #= 83))
        #\/ (((C4 #= 77) #/\ (C1 #= 77)) #/\ (C2 #= 83) #/\ (C3 #= 83))
    )) #<==> IsXmas.

distribute(_, [], []).
distribute(X, [Y|Ys], [[X, Y]|Zs]) :-
    distribute(X, Ys, Zs).

cartesian_product([], _, []).
cartesian_product([X|Xs], Ys, Zs) :-
    distribute(X, Ys, Z),
    append(Z, Zs1, Zs),
    cartesian_product(Xs, Ys, Zs1).

count_xmas(G, Count) :-
    grid(_, Width, Height) = G,
    % Generate list of non-edge coords. The xmas predicate on the edges is
    % undefined since the adjacents are out of bounds.
    MaxX #= Width - 2,
    MaxY #= Height - 2,
    numlist(1, MaxX, Xs),
    numlist(1, MaxY, Ys),
    cartesian_product(Xs, Ys, Coords),
    maplist(xmas(G), Coords, Deltas),
    sum(Deltas, #=, Count).

char_domain(C) :- C in 10 \/ 65 \/ 77 \/ 83.

problem(G, Width, Height, Count) :-
    G = grid(_, Width, Height),
    Len #= (1 + Width) * Height,
    length(Codes, Len),
    maplist(char_domain, Codes),
    codes_as_grid(Codes, G),
    count_xmas(G, Count),
    label(Codes),
    string_codes(Text, Codes),
    write("Problem\n"),
    write(Text),
    write("\n").

solution(Count) :-
    file_contents(Codes),
    codes_as_grid(Codes, grid(Tree, Width, Height)),
    count_xmas(grid(Tree, Width, Height), Count).