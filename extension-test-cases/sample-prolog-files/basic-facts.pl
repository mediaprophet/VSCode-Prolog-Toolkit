% basic-facts.pl
% Purpose: Test basic Prolog syntax highlighting, completion, and query execution
% Features tested: Facts, rules, queries, syntax highlighting, hover provider

% Simple facts about family relationships
parent(tom, bob).
parent(tom, liz).
parent(bob, ann).
parent(bob, pat).
parent(pat, jim).

% Gender facts
male(tom).
male(bob).
male(jim).
female(liz).
female(ann).
female(pat).

% Rules for family relationships
father(X, Y) :- parent(X, Y), male(X).
mother(X, Y) :- parent(X, Y), female(X).
grandparent(X, Z) :- parent(X, Y), parent(Y, Z).
grandfather(X, Z) :- grandparent(X, Z), male(X).
grandmother(X, Z) :- grandparent(X, Z), female(X).

% Sibling relationships
sibling(X, Y) :- parent(Z, X), parent(Z, Y), X \= Y.
brother(X, Y) :- sibling(X, Y), male(X).
sister(X, Y) :- sibling(X, Y), female(X).

% Test queries (for quick query testing):
% ?- parent(tom, X).
% ?- father(bob, ann).
% ?- grandparent(tom, jim).
% ?- sibling(ann, pat).

% Expected results:
% parent(tom, X) should return X = bob, X = liz
% father(bob, ann) should return true
% grandparent(tom, jim) should return true
% sibling(ann, pat) should return true