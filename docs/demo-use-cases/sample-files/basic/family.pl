% family.pl
% Purpose: Classic family relationships example for VSCode Prolog Toolkit demos
% Demo Usage: Syntax highlighting, query execution, chat assistant, activity bar
% Complexity: Beginner-friendly with clear examples

% =============================================================================
% FAMILY FACTS DATABASE
% =============================================================================

% Parent relationships - the foundation of our family tree
parent(tom, bob).
parent(tom, liz).
parent(bob, ann).
parent(bob, pat).
parent(pat, jim).
parent(pat, sarah).
parent(liz, mike).
parent(liz, emma).

% Gender facts - essential for derived relationships
male(tom).
male(bob).
male(jim).
male(mike).
female(liz).
female(ann).
female(pat).
female(sarah).
female(emma).

% Age facts - for demonstration of arithmetic operations
age(tom, 65).
age(bob, 40).
age(liz, 38).
age(ann, 18).
age(pat, 16).
age(jim, 8).
age(sarah, 6).
age(mike, 12).
age(emma, 10).

% =============================================================================
% DERIVED RELATIONSHIPS - Rules that define family relationships
% =============================================================================

% Father relationship - parent who is male
father(X, Y) :- 
    parent(X, Y), 
    male(X).

% Mother relationship - parent who is female
mother(X, Y) :- 
    parent(X, Y), 
    female(X).

% Grandparent relationship - parent of a parent
grandparent(X, Z) :- 
    parent(X, Y), 
    parent(Y, Z).

% Grandfather - grandparent who is male
grandfather(X, Z) :- 
    grandparent(X, Z), 
    male(X).

% Grandmother - grandparent who is female
grandmother(X, Z) :- 
    grandparent(X, Z), 
    female(X).

% Sibling relationship - same parents, different people
sibling(X, Y) :- 
    parent(Z, X), 
    parent(Z, Y), 
    X \= Y.

% Brother - sibling who is male
brother(X, Y) :- 
    sibling(X, Y), 
    male(X).

% Sister - sibling who is female
sister(X, Y) :- 
    sibling(X, Y), 
    female(X).

% Uncle - brother of a parent
uncle(X, Y) :- 
    parent(Z, Y), 
    brother(X, Z).

% Aunt - sister of a parent
aunt(X, Y) :- 
    parent(Z, Y), 
    sister(X, Z).

% Cousin - child of an aunt or uncle
cousin(X, Y) :- 
    parent(P1, X), 
    parent(P2, Y), 
    sibling(P1, P2).

% =============================================================================
% UTILITY PREDICATES - Helpful predicates for queries and demonstrations
% =============================================================================

% Get all children of a person
children(Parent, Children) :-
    findall(Child, parent(Parent, Child), Children).

% Get all ancestors of a person (recursive)
ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).

% Get all descendants of a person (recursive)
descendant(X, Y) :- parent(Y, X).
descendant(X, Y) :- parent(Z, X), descendant(Z, Y).

% Check if someone is an adult (age >= 18)
adult(Person) :- 
    age(Person, Age), 
    Age >= 18.

% Check if someone is a child (age < 18)
child(Person) :- 
    age(Person, Age), 
    Age < 18.

% Get family members by generation
generation(Person, 0, Person).
generation(Person, N, Ancestor) :-
    N > 0,
    parent(Parent, Person),
    N1 is N - 1,
    generation(Parent, N1, Ancestor).

% =============================================================================
% DEMO QUERIES - Example queries for testing and demonstration
% =============================================================================

% Basic fact queries:
% ?- parent(tom, X).          % Who are Tom's children?
% ?- parent(X, ann).          % Who are Ann's parents?
% ?- male(bob).               % Is Bob male?

% Rule-based queries:
% ?- father(bob, Y).          % Who are Bob's children (father relationship)?
% ?- mother(liz, Z).          % Who are Liz's children (mother relationship)?
% ?- grandparent(tom, W).     % Who are Tom's grandchildren?

% Complex relationship queries:
% ?- sibling(ann, pat).       % Are Ann and Pat siblings?
% ?- uncle(bob, jim).         % Is Bob Jim's uncle?
% ?- cousin(ann, mike).       % Are Ann and Mike cousins?

% Utility predicate queries:
% ?- children(tom, Kids).     % Get all of Tom's children as a list
% ?- ancestor(tom, jim).      % Is Tom an ancestor of Jim?
% ?- adult(ann).              % Is Ann an adult?

% Advanced queries for chat assistant demos:
% ?- findall(X, father(X, _), Fathers).           % Find all fathers
% ?- findall(X-Y, grandparent(X, Y), GrandRels). % All grandparent relationships
% ?- findall(P, (age(P, A), A > 30), Adults).    % People over 30

% =============================================================================
% EXPECTED RESULTS - What should happen when queries are executed
% =============================================================================

% parent(tom, X) should return:
%   X = bob
%   X = liz

% father(bob, Y) should return:
%   Y = ann
%   Y = pat

% grandparent(tom, W) should return:
%   W = ann
%   W = pat
%   W = jim
%   W = sarah
%   W = mike
%   W = emma

% sibling(ann, pat) should return:
%   true

% children(tom, Kids) should return:
%   Kids = [bob, liz]

% =============================================================================
% DEMO INSTRUCTIONS
% =============================================================================

% For Syntax Highlighting Demo:
% - Open this file to show comprehensive Prolog syntax
% - Point out comment highlighting, predicate names, variables
% - Show operator highlighting (:-), punctuation, and structure

% For Activity Bar Demo:
% - Use quick query interface with: parent(tom, X)
% - Show file detection in workspace explorer
% - Demonstrate query execution from dashboard

% For Chat Assistant Demo:
% - Consult file with: @prolog /consult family.pl
% - Query with: @prolog father(bob, Y)
% - Get help with: @prolog /help member/2

% For Debugging Demo:
% - Set breakpoints on recursive predicates like ancestor/2
% - Step through execution to show variable bindings
% - Inspect call stack during recursive calls

% =============================================================================
% FILE METADATA
% =============================================================================

% File: family.pl
% Version: 1.0.0
% Created: January 2025
% Purpose: Demo file for VSCode Prolog Toolkit
% Lines of Code: ~150
% Demo Scenarios: Syntax, Activity Bar, Chat Assistant, Basic Development
% Complexity: Beginner
% Dependencies: None (pure Prolog)