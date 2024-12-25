:- use_module(library(clpfd)).
:- use_module(library(reif)).

parse_odd([], _, _, [], []).
parse_odd([H|T], FileID, Ix, [free(Ix, Length)|T1], Files) :-
    number_codes(Length, [H]),
    Ix1 #= Ix + Length,
    parse_even(T, FileID, Ix1, T1, Files).

parse_even([], _, _, [], []).
parse_even([H|T], FileID, Ix, FreeList, [file(FileID, Ix, FileLength)|T1]) :-
    number_codes(FileLength, [H]),
    Ix1 #= Ix + FileLength,
    FileID1 #= FileID + 1,
    parse_odd(T, FileID1, Ix1, FreeList, T1).

% Use up the given Size of space in the free list.
% Outputs the index of the free space used and the new free list.
% If free space cannot be found, outputs a list identical to the
% original, and -1.
consume_space([], _, _, [], -1).
consume_space([free(Ix, Length)|T], Size, MaxIx, FreeList1, OutIx) :-
    Remaining #= Length - Size,
    (
        % I would prefer (Remaining #>= 0 #/\ Ix #=< MaxIx) #<==> Fits,
        % but it is too slow.
        (Remaining >= 0, Ix =< MaxIx)
        -> (Fits = 1)
        ; (Fits = 0)
    ),
    if_(Fits = 1,
        (
            if_(Remaining = 0,
                (
                    FreeList1 = T,
                    OutIx = Ix
                ),
                (
                    Ix1 #= Ix + Size,
                    FreeList1 = [free(Ix1, Remaining)|T],
                    OutIx = Ix
                ))
        ),
        (
            [H1|T1] = FreeList1,
            H1 = free(Ix, Length),
            consume_space(T, Size, MaxIx, T1, OutIx)
        )).

defrag([], _, []).
defrag([file(FileID, Ix, FileLength)|T], FreeList, [OutH|OutT]) :-
    consume_space(FreeList, FileLength, Ix, FreeList1, FreeIx),
    if_(FreeIx = -1,
        OutH = file(FileID, Ix, FileLength),
        OutH = file(FileID, FreeIx, FileLength)),
    defrag(T, FreeList1, OutT).

checksum([], 0).
checksum([file(FileID, Ix, Length)|Xs], N) :-
    % Use n*(n-1)/2 for summation of 0 + .. + (n-1).
    N #= N1 + FileID * (Ix * Length + (Length * (Length - 1) // 2)),
    checksum(Xs, N1).

solution(Solution) :-
    % Note: assumes there is no newline in the input.
    read_file_to_codes("input.txt", Codes, []),
    parse_even(Codes, 0, 0, FreeList, Files),
    reverse(Files, FilesReversed),
    defrag(FilesReversed, FreeList, FilesOut),
    checksum(FilesOut, Solution).