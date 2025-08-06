% Complex Prolog test file for advanced validation
% This file contains more sophisticated predicates for testing advanced functionality

:- dynamic(fact/1).
:- dynamic(rule/2).

% Complex data structures
person(john, [age(30), occupation(engineer), city(london)]).
person(mary, [age(25), occupation(doctor), city(paris)]).
person(bob, [age(35), occupation(teacher), city(berlin)]).

% Meta-predicates
call_with_depth_limit(Goal, Limit) :-
    call_with_depth_limit(Goal, Limit, _).

% Higher-order predicates
map(_, [], []).
map(Pred, [H|T], [H2|T2]) :-
    call(Pred, H, H2),
    map(Pred, T, T2).

filter(_, [], []).
filter(Pred, [H|T], [H|T2]) :-
    call(Pred, H), !,
    filter(Pred, T, T2).
filter(Pred, [_|T], T2) :-
    filter(Pred, T, T2).

% DCG (Definite Clause Grammar) example
sentence --> noun_phrase, verb_phrase.
noun_phrase --> determiner, noun.
verb_phrase --> verb, noun_phrase.

determiner --> [the].
determiner --> [a].
noun --> [cat].
noun --> [dog].
verb --> [chases].
verb --> [sees].

% Constraint handling
:- use_module(library(clpfd)).

sudoku_constraint(Vars) :-
    Vars ins 1..9,
    all_different(Vars).

% Complex recursive structure
tree_size(empty, 0).
tree_size(node(_, Left, Right), Size) :-
    tree_size(Left, LeftSize),
    tree_size(Right, RightSize),
    Size is LeftSize + RightSize + 1.

tree_height(empty, 0).
tree_height(node(_, Left, Right), Height) :-
    tree_height(Left, LeftHeight),
    tree_height(Right, RightHeight),
    Height is max(LeftHeight, RightHeight) + 1.

% Performance test predicate
fibonacci(0, 0) :- !.
fibonacci(1, 1) :- !.
fibonacci(N, F) :-
    N > 1,
    N1 is N - 1,
    N2 is N - 2,
    fibonacci(N1, F1),
    fibonacci(N2, F2),
    F is F1 + F2.

% Memoized fibonacci for performance comparison
:- dynamic(fib_memo/2).

fibonacci_memo(N, F) :-
    fib_memo(N, F), !.
fibonacci_memo(0, 0) :- 
    assertz(fib_memo(0, 0)).
fibonacci_memo(1, 1) :- 
    assertz(fib_memo(1, 1)).
fibonacci_memo(N, F) :-
    N > 1,
    N1 is N - 1,
    N2 is N - 2,
    fibonacci_memo(N1, F1),
    fibonacci_memo(N2, F2),
    F is F1 + F2,
    assertz(fib_memo(N, F)).

% Error handling test
safe_divide(X, Y, Result) :-
    (   Y =:= 0 ->
        throw(error(evaluation_error(zero_divisor), safe_divide/3))
    ;   Result is X / Y
    ).

% Complex query for testing
complex_query(Person, Details) :-
    person(Person, Details),
    member(age(Age), Details),
    Age > 25,
    member(occupation(Job), Details),
    Job \= unemployed.

% Test predicates for validation
test_complex_1 :- 
    map(succ, [1,2,3], [2,3,4]).

test_complex_2 :- 
    tree_size(node(a, node(b, empty, empty), node(c, empty, empty)), 3).

test_complex_3 :- 
    fibonacci_memo(10, 55).

test_complex_4 :-
    phrase(sentence, [the, cat, chases, a, dog]).

% Stress test predicate
stress_test(N) :-
    findall(X, between(1, N, X), List),
    length(List, N).