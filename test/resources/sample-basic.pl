% Basic Prolog test file for validation
% This file contains simple predicates for testing basic functionality

% Facts
parent(tom, bob).
parent(tom, liz).
parent(bob, ann).
parent(bob, pat).
parent(pat, jim).

% Rules
grandparent(X, Z) :- 
    parent(X, Y), 
    parent(Y, Z).

ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :- 
    parent(X, Z), 
    ancestor(Z, Y).

% List operations
member(X, [X|_]).
member(X, [_|T]) :- member(X, T).

append([], L, L).
append([H|T], L, [H|R]) :- append(T, L, R).

% Simple arithmetic
factorial(0, 1).
factorial(N, F) :- 
    N > 0,
    N1 is N - 1,
    factorial(N1, F1),
    F is N * F1.

% Test queries for validation
test_query_1 :- parent(tom, bob).
test_query_2 :- grandparent(tom, ann).
test_query_3 :- factorial(5, 120).