% debugging-test.pl
% Purpose: Test debugging features, breakpoints, and step-through execution
% Features tested: Debugging, breakpoints, variable inspection, call stack

% Simple recursive factorial function for debugging
factorial(0, 1) :- !.
factorial(N, F) :-
    N > 0,
    N1 is N - 1,
    factorial(N1, F1),
    F is N * F1.

% List operations for debugging
append([], L, L).
append([H|T], L, [H|R]) :-
    append(T, L, R).

% Member predicate with choice points
member(X, [X|_]).
member(X, [_|T]) :-
    member(X, T).

% Reverse list implementation
reverse(List, Reversed) :-
    reverse(List, [], Reversed).

reverse([], Acc, Acc).
reverse([H|T], Acc, Reversed) :-
    reverse(T, [H|Acc], Reversed).

% Sorting algorithm (bubble sort)
bubble_sort(List, Sorted) :-
    swap(List, List1), !,
    bubble_sort(List1, Sorted).
bubble_sort(Sorted, Sorted).

swap([X, Y|Rest], [Y, X|Rest]) :-
    X > Y.
swap([X|Rest], [X|Rest1]) :-
    swap(Rest, Rest1).

% Test predicate with multiple solutions
likes(mary, food).
likes(mary, wine).
likes(john, wine).
likes(john, mary).

happy(X) :- likes(X, wine).

% Debugging test queries:
% ?- factorial(5, F).
% ?- append([1,2], [3,4], L).
% ?- member(2, [1,2,3]).
% ?- reverse([1,2,3,4], R).
% ?- bubble_sort([3,1,4,1,5], S).

% Expected debugging behavior:
% - Breakpoints should work on any line with code
% - Step into should enter recursive calls
% - Variable values should be visible in debugger
% - Call stack should show recursive calls