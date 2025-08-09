/*  --------------------
    ZEBRA – PROLOG SOLVER
    --------------------

   The program encodes every rule as a constraint on the list of houses.
   A house is represented by a structure

        house(Color, Nationality, Pet, Drink, Cigarette)

   The five houses are stored in order from left to right:

        Houses = [H1,H2,H3,H4,H5].

   The program tries all permutations of the attributes and keeps only
   those that satisfy every rule.  At the end it prints who drinks water
   and who owns the zebra.
*/

:- use_module(library(clpfd)).          % for labeling/2 (if you want to use CLPFD)
:- set_prolog_flag(answer_write_options, [max_depth(0)]).

% ------------------------------------------------------------------
% Main predicate – runs the solver and prints the answer
% ------------------------------------------------------------------
solve_zebra :-
    Houses = [
        house(_, _, _, _, _),   % H1  (first house)
        house(_, _, _, _, _),   % H2
        house(_, _, _, _, _),   % H3 (middle house)
        house(_, _, _, _, _),   % H4
        house(_, _, _, _, _)    % H5  (last house)
    ],

    % --------------------------------------------------------------
    % All attributes are chosen from the corresponding domains
    % --------------------------------------------------------------
    Colors      = [red, green, ivory, yellow, blue],
    Nationality = [englishman, spaniard, ukrainian,
                   norwegian, japanese],
    Pets        = [dog, snails, fox, horse, zebra],
    Drinks      = [coffee, tea, milk, orange_juice, water],
    Cigarettes  = [old_gold, kools, chesterfields,
                   lucky_strike, parliaments],

    % All attributes are a permutation of the domain
    all_distinct(Colors),      permutation(Colors,     [H1Color,H2Color,H3Color,H4Color,H5Color]),
    all_distinct(Nationality), permutation(Nationality,[H1Nat,H2Nat,H3Nat,H4Nat,H5Nat]),
    all_distinct(Pets),        permutation(Pets,        [H1Pet,H2Pet,H3Pet,H4Pet,H5Pet]),
    all_distinct(Drinks),      permutation(Drinks,      [H1Drink,H2Drink,H3Drink,H4Drink,H5Drink]),
    all_distinct(Cigarettes),  permutation(Cigarettes,[H1Cig,H2Cig,H3Cig,H4Cig,H5Cig]),

    % --------------------------------------------------------------
    % Build the house list with the chosen attributes
    % --------------------------------------------------------------
    Houses = [
        house(H1Color, H1Nat, H1Pet, H1Drink, H1Cig),
        house(H2Color, H2Nat, H2Pet, H2Drink, H2Cig),
        house(H3Color, H3Nat, H3Pet, H3Drink, H3Cig),
        house(H4Color, H4Nat, H4Pet, H4Drink, H4Cig),
        house(H5Color, H5Nat, H5Pet, H5Drink, H5Cig)
    ],

    % --------------------------------------------------------------
    % Apply the constraints from the puzzle
    % --------------------------------------------------------------

    % 1. The Englishman lives in the red house.
    member(house(red, englishman, _, _, _), Houses),

    % 2. The Spaniard owns the dog.
    member(house(_, spaniard, dog, _, _), Houses),

    % 3. Coffee is drunk in the green house.
    member(house(green, _, _, coffee, _), Houses),

    % 4. The Ukrainian drinks tea.
    member(house(_, ukrainian, _, tea, _), Houses),

    % 5. The green house is immediately to the right of the ivory house.
    adjacent(house(ivory, _, _, _, _), house(green, _, _, _, _), Houses),

    % 6. The Old Gold smoker owns snails.
    member(house(_, _, snails, _, old_gold), Houses),

    % 7. Kools are smoked in the yellow house.
    member(house(yellow, _, _, _, kools), Houses),

    % 8. Milk is drunk in the middle house.
    nth1(3, Houses, house(_, _, _, milk, _)),

    % 9. The Norwegian lives in the first house.
    nth1(1, Houses, house(_, norwegian, _, _, _)),

    %10. The man who smokes Chesterfields lives in the house next to
    %    the man with the fox.
    adjacent(house(_, _, _, _, chesterfields), house(_, _, fox, _, _), Houses),

    %11. Kools are smoked in the house next to the house where the horse is kept.
    adjacent(house(_, _, _, _, kools), house(_, _, horse, _, _), Houses),

    %12. The Lucky Strike smoker drinks orange juice.
    member(house(_, _, _, orange_juice, lucky_strike), Houses),

    %13. The Japanese smokes Parliaments.
    member(house(_, japanese, _, _, parliaments), Houses),

    %14. The Norwegian lives next to the blue house.
    adjacent(house(_, norwegian, _, _, _), house(blue, _, _, _, _), Houses),

    % --------------------------------------------------------------
    % If all constraints are satisfied we have a solution
    % --------------------------------------------------------------
    write('Solution found!'), nl,
    print_houses(Houses),
    find_water_owner(Houses, WaterOwner),
    find_zebra_owner(Houses, ZebraOwner),
    format('The ~w drinks water.~n', [WaterOwner]),
    format('The ~w owns the zebra.~n', [ZebraOwner]).

% ------------------------------------------------------------------
% Helper predicates
% ------------------------------------------------------------------

% all_distinct/1 – wrapper around library(clpfd) for clarity
all_distinct(L) :- maplist(=, L), !.

% adjacent/3 – true if A and B are next to each other in the list
adjacent(A,B,List) :-
    append(_, [A,B|_], List);
    append(_, [B,A|_], List).

% print_houses/1 – pretty‑print the houses
print_houses(Hs) :-
    nl,
    nth0(0, Hs, house(C1,N1,P1,D1,Co1)),
    nth0(1, Hs, house(C2,N2,P2,D2,Co2)),
    nth0(2, Hs, house(C3,N3,P3,D3,Co3)),
    nth0(3, Hs, house(C4,N4,P4,D4,Co4)),
    nth0(4, Hs, house(C5,N5,P5,D5,Co5)),
    format('1: ~w | ~w | ~w | ~w | ~w~n', [C1,N1,P1,D1,Co1]),
    format('2: ~w | ~w | ~w | ~w | ~w~n', [C2,N2,P2,D2,Co2]),
    format('3: ~w | ~w | ~w | ~w | ~w~n', [C3,N3,P3,D3,Co3]),
    format('4: ~w | ~w | ~w | ~w | ~w~n', [C4,N4,P4,D4,Co4]),
    format('5: ~w | ~w | ~w | ~w | ~w~n', [C5,N5,P5,D5,Co5]),
    nl.

% find_water_owner/2 – who drinks water?
find_water_owner(Hs, Owner) :-
    member(house(_, Owner, _, water, _), Hs).

% find_zebra_owner/2 – who owns the zebra?
find_zebra_owner(Hs, Owner) :-
    member(house(_, Owner, zebra, _, _), Hs).

% ------------------------------------------------------------------
% Run the solver when the file is consulted
% ------------------------------------------------------------------
:- solve_zebra.
