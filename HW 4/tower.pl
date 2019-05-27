% Tower Implementation

tower(N, T, C) :-
    % Assign counts structure to C
    C = counts(Top, Bottom, Left, Right),

    % Ensure that number of sublists for matrix is N, and counts are length N. 
    length(T, N),
    length(Top, N),
    length(Bottom, N),
    length(Left, N),
    length(Right, N),

    % These constraints must be satisfied, so add the cut. 
    !,

    % Ensure all sublists are size N, and their elements are within [1,N] and all different for the T matrix 
    maplist(length_reverse(N), T),
    maplist(domain_reverse(1, N), T),
    maplist(fd_all_different, T),

    transpose(T, Tt),
    % Make every column different/unique
    maplist(fd_all_different, Tt),

    % Check the counts from each angle, reversing the list to see from the opposite angle
    check_count(T, Left),
    check_count_reverse(T, Right),
    check_count(Tt, Top),
    check_count_reverse(Tt, Bottom),
    
    % Label each list
    maplist(fd_labeling, T).

length_reverse(N, List) :-
    length(List, N).

domain_reverse(Begin, End, List) :-
    fd_domain(List, Begin, End).

count_visible_towers([], 0, _).
count_visible_towers([H | T], Count, Max) :-
    H #> Max,
    NewCount #= Count - 1,
    count_visible_towers(T, NewCount, H);
    H #< Max,
    count_visible_towers(T, Count, Max).

check_count([], []).
check_count([ListHead | ListTail], [CountHead | CountTail]) :- 
    count_visible_towers(ListHead, CountHead, 0),
    check_count(ListTail, CountTail).

check_count_reverse([], []).
check_count_reverse([ListHead | ListTail], [CountHead | CountTail]) :- 
    reverse(ListHead, ReverseListHead),
    count_visible_towers(ReverseListHead, CountHead, 0),
    check_count_reverse(ListTail, CountTail).

% Transpose Code, obtained from StackOverflow, specifically https://stackoverflow.com/questions/4280986/how-to-transpose-a-matrix-in-prolog

transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).

transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
        lists_firsts_rests(Ms, Ts, Ms1),
        transpose(Rs, Ms1, Tss).

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        lists_firsts_rests(Rest, Fs, Oss).


% Plain Tower Implementation

plain_tower(N, T, C) :- 
    % Assign counts structure to C
    C = counts(Top, Bottom, Left, Right),

    % Ensure that all matrices have size N
    length(T, N),
    length(Top, N),
    length(Bottom, N),
    length(Left, N),
    length(Right, N),

    % Create matrix constraints w/ respect to Left and Right Counts
    matrix_constraint(T, N, Left, Right),
    transpose(T, Tt),
    % Create matrix constraints w/ respect to Top and Bottom Counts
    matrix_constraint(Tt, N, Top, Bottom).

% Creates a list with elements in reverse order from [N, 1]
create_ordered_list(0, []).
create_ordered_list(N, [H | T]) :-
    H = N,
    NewN is N - 1,
    create_ordered_list(NewN, T).

matrix_constraint([], N, _, _).
matrix_constraint([THead | TTail], N, [LeftCountHead | LeftCountTail], [RightCountHead | RightCountTail]) :-
    % Get a permutation for a unique list of size N
    permute_list(N, THead),

    % Squeeze Counts between 1 and N effectivelt
    member(LeftCountHead, THead),
    member(RightCountHead, THead),

    % Check counts
    check_count_plain(THead, LeftCountHead, 0),
    reverse(THead, ReverseTHead),
    check_count_plain(ReverseTHead, RightCountHead, 0),

    % Recurse to fill remaining
    matrix_constraint(TTail, N, LeftCountTail, RightCountTail).


% Had to separate base cases this for runtime/prevent hanging
check_count_plain([ListMember | []], 1, Max) :-
    ListMember > Max.

check_count_plain([ListMember | []], 0, Max) :-
    ListMember < Max.

check_count_plain([H | T], Count, Max) :-
    H > Max,
    NewCount is Count - 1,
    check_count_plain(T, NewCount, H);
    H < Max,
    check_count_plain(T, Count, Max).

permute_list(N, List) :-
    length(OrderedList, N),
    create_ordered_list(N, OrderedList),
    !,
    permutation(OrderedList, List).



% Speedup 
run_tower_n_times(0).
run_tower_n_times(N) :-
    tower(6, T, counts([5,4,4,3,2,1],[1,2,2,3,3,3],[6,5,4,2,2,1],[1,2,2,3,2,3])),
    NewN is N - 1,
    run_tower_n_times(NewN).
    

tower_runtime(Time) :-
    statistics(runtime, [StartTime | _]),
    run_tower_n_times(100),
    statistics(runtime, [EndTime | _]),
    Difference is EndTime - StartTime,
    Time is Difference / 100.

plain_tower_runtime(PTowerTime) :-
    statistics(runtime, [PlainStartTime | _]),
    plain_tower(6, T, counts([5,4,4,3,2,1],[1,2,2,3,3,3],[6,5,4,2,2,1],[1,2,2,3,2,3])),
    statistics(runtime, [PlainEndTime | _]),
    PTowerTime is PlainEndTime - PlainStartTime.

speedup(Ratio) :-
    tower_runtime(TowerTime),
    plain_tower_runtime(PlainTowerRuntime),
    Ratio is PlainTowerRuntime / TowerTime.




% Ambiguous Puzzle

ambiguous(N, C, T1, T2) :-
    tower(N, T1, C),
    tower(N, T2, C),
    T1 \= T2.




