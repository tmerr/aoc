:- use_module(library(clpfd)).

prepend_repeated(_, 0, Base, Base).
prepend_repeated(V, N, Base, [V|T]) :-
    N #> 0,
    N1 #= N - 1,
    prepend_repeated(V, N1, Base, T).

parse_odd([], _, []).
parse_odd([H|T], FileID, Res) :-
    number_codes(FreeBlocks, [H]),
    parse_even(T, FileID, Res1),
    prepend_repeated(free, FreeBlocks, Res1, Res).

parse_even([], _, []).
parse_even([H|T], FileID, Res) :-
    number_codes(FileBlocks, [H]),
    FileID1 #= FileID + 1,
    parse_odd(T, FileID1, Res1),
    prepend_repeated(file(FileID), FileBlocks, Res1, Res).

defrag([], _, []).
defrag([file(FileID)|Xs], Ys, [file(FileID)|Zs]) :-
    defrag(Xs, Ys, Zs).
defrag([free|Xs], [free|Ys], Zs) :-
    defrag([free|Xs], Ys, Zs).
defrag([free|Xs], [file(FileID)|Ys], [file(FileID)|Zs]) :-
    defrag(Xs, Ys, Zs).

count_file_blocks([], 0).
count_file_blocks([free|Xs], N) :-
    count_file_blocks(Xs, N).
count_file_blocks([file(_)|Xs], N) :-
    N #= N1 + 1,
    count_file_blocks(Xs, N1).

takeN(_, 0, []).
takeN([X|Xs], N, [X|Ys]) :-
    N #> 0,
    N1 #= N - 1,
    takeN(Xs, N1, Ys).
    
solution_number([], _, 0).
solution_number([file(X)|Xs], Ix, N) :-
    N #= N1 + Ix * X,
    Ix1 #= Ix+1,
    solution_number(Xs, Ix1, N1).

solution(Solution) :-
    % Note: assumes there is no newline in the input.
    read_file_to_codes("input.txt", Codes, []),
    parse_even(Codes, 0, Storage),
    count_file_blocks(Storage, NumBlocks),
    reverse(Storage, Reversed),
    defrag(Storage, Reversed, Defragged),
    % Defrag leaves extra elements at the end.
    % This is sort of a hack to remove them.
    takeN(Defragged, NumBlocks, Defragged1),
    solution_number(Defragged1, 0, Solution).