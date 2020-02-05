% 1. Wijantra Cojamnong ID 5627
% 2. Mai Norapong ID 5619
% 3. Nutthanich Narphromar ID 5490
% 4. Tharathorn Bunrattanasathian ID 6011

% Facts
person(albus_potter).
person(angelina_johnson).
person(arthur_weasley).
person(bill_weasley).
person(charlie_weasley).
person(dominique_weasley).
person(dudley_dursley).
person(euphemia_potter).
person(fleamont_potter).
person(fleur_delacour).
person(fred_johnson_weasley).
person(fred_weasley).
person(george_weasley).
person(ginny_weasley).
person(harry_potter).
person(hermione_granger).
person(hugo_granger-weasley).
person(james_potter).
person(james_s_potter).
person(lilly_potter).
person(lilly_l_potter).
person(louis_weasley).
person(marge_dursley).
person(molly_weasley).
person(percy_weasley).
person(petunia_dursley).
person(ron_weasley).
person(rose_granger-weasley).
person(roxanne_weasley).
person(vernon_dursley).
person(victoire_weasley).
 

% Properties
male(albus_potter).
female(angelina_johnson).
male(arthur_weasley).
male(bill_weasley).
male(charlie_weasley).
male(dominique_weasley).
male(dudley_dursley).
female(euphemia_potter).
male(fleamont_potter).
female(fleur_delacour).
male(fred_johnson_weasley).
male(fred_weasley).
male(george_weasley).
female(ginny_weasley).
male(harry_potter).
female(hermione_granger).
male(hugo_granger-weasley).
 
male(james_potter).
male(james_s_potter).
female(lilly_potter).
female(lilly_l_potter).
male(louis_weasley).
female(marge_dursley).
female(molly_weasley).
male(percy_weasley).
female(petunia_dursley).
male(ron_weasley).
female(rose_granger-weasley).
female(roxanne_weasley).
male(vernon_dursley).
female(victoire_weasley).


% Relationships
parent(ginny_weasley, albus_potter).
parent(harry_potter, albus_potter).
spouse(angelina_johnson, george_weasley).
spouse(arthur_weasley, molly_weasley).
parent(arthur_weasley, bill_weasley).
parent(molly_weasley, bill_weasley).
spouse(bill_weasley, fleur_delacour).
parent(arthur_weasley, charlie_weasley).
parent(molly_weasley, charlie_weasley).
parent(bill_weasley, dominique_weasley).
parent(fleur_delacour, dominique_weasley).
parent(petunia_dursley, dudley_dursley).
parent(vernon_dursley, dudley_dursley).
spouse(euphemia_potter, fleamont_potter).
spouse(fleamont_potter, euphemia_potter).
spouse(fleur_delacour, bill_weasley).
parent(angelina_johnson, fred_johnson_weasley).
parent(george_weasley, fred_johnson_weasley).
parent(arthur_weasley, fred_weasley).
parent(molly_weasley, fred_weasley).
parent(arthur_weasley, george_weasley).
parent(molly_weasley, george_weasley).
spouse(george_weasley, angelina_Johnson).
parent(arthur_weasley, ginny_weasley).
parent(molly_weasley, ginny_weasley).
spouse(ginny_weasley, harry_potter).
parent(lilly_potter, harry_potter).
parent(james_potter, harry_potter).
spouse(harry_potter, ginny_weasley).
spouse(hermione_granger, ron_weasley).
parent(ron_weasley, hugo_granger-weasley).
parent(hermione_granger, hugo_granger-weasley).
parent(euphemia_potter, james_potter).
parent(fleamont_potter, james_potter).
spouse(james_potter, lilly_potter).
parent(harry_potter, james_s_potter).
parent(ginny_weasley, james_s_potter).
spouse(lilly_potter, james_potter).
parent(harry_potter, lilly_l_potter).
parent(ginny_weasley, lilly_l_potter).
parent(bill_weasley, louis_weasley).
parent(fleur_delacour, louis_weasley).
spouse(molly_weasley, arthur_weasley).
parent(arthur_weasley, percy_weasley).
parent(molly_weasley, percy_weasley).
spouse(petunia_dursley, vernon_dursley).
parent(arthur_weasley, ron_weasley).
parent(molly_weasley, ron_weasley).
spouse(ron_weasley, hermione_granger).
parent(ron_weasley, rose_granger-weasley).
parent(hermione_granger, rose_granger-weasley).
parent(angelina_johnson, roxanne_weasley).
parent(george_weasley, roxanne_weasley).
spouse(vernon_dursley, petunia_dursley).
parent(bill_weasley, victoire_weasley).
parent(fleur_delacour, victoire_weasley).




% Rules
mother(X,Y) :- female(X), parent(X,Y).
father(X,Y) :- male(X), parent(X,Y).
child(X,Y) :- parent(Y,X).
son(X,Y) :- child(X,Y), male(X).
daughter(X,Y) :- child(X,Y), female(X).
sibling(X,Y) :- parent(Z,X), parent(Z,Y), X \= Y.
sister(X,Y) :- sibling(X,Y), female(X).
brother(X,Y) :- sibling(X,Y), male(X).
husband(X,Y) :- spouse(X,Y), male(X).
wife(X,Y) :- spouse(X,Y), female(X).
cousin(X,Y) :- child(X,Z), sibling(Z,W), parent(W,Y).
niece(X,Y) :- daughter(X,Z), sibling(Z,Y).
niece(X,Y) :- daughter(X,Z), sibling(Z,W), spouse(W,Y).
nephew(X,Y) :- son(X,Z), sibling(Z,Y).
nephew(X,Y) :- son(X,Z), sibling(Z,W), spouse(W,Y).
grandmother(X,Y) :- mother(X,Z), parent(Z,Y).
grandfather(X,Y) :- father(X,Z), parent(Z,Y).
aunt(X,Y) :- sister(X,Z), parent(Z,Y).
aunt(X,Y) :- wife(X,Z), uncle(Z,Y).
uncle(X,Y) :- brother(X,Z), parent(Z,Y).
uncle(X,Y) :- husband(X,Z), aunt(Z,Y).
father-in-law(X,Y) :- father(X,Z), spouse(Z,Y).
mother-in-law(X,Y) :- mother(X,Z), spouse(Z,Y).

% Tests

:- begin_tests(test).

test(a) :-
    assertion(mother(lilly_potter,harry_potter)),
    assertion(father(harry_potter,albus_potter)),
    assertion(child(harry_potter, james_potter)),
    assertion(son(dominique_weasley, bill_weasley)),
    assertion(daughter(ginny_weasley, arthur_weasley)),
    assertion(sibling(ron_weasley, ginny_weasley)),
    assertion(sister(ginny_weasley, ron_weasley)),
    assertion(brother(ron_weasley, ginny_weasley)),
    assertion(husband(fleamont_potter, euphemia_potter)),
    assertion(wife(hermione_granger, ron_weasley)),
    assertion(cousin(rose_granger-weasley, albus_potter)),
    assertion(niece(rose_granger-weasley, ginny_weasley)),
    assertion(niece(rose_granger-weasley, harry_potter)),
    assertion(nephew(albus_potter, ron_weasley)),
    assertion(grandmother(euphemia_potter, harry_potter)),
    assertion(grandfather(fleamont_potter, harry_potter)),
    assertion(sibling(ron_weasley, ginny_weasley)),
    assertion(aunt(hermione_granger, albus_potter)),
    assertion(aunt(ginny_weasley, rose_granger-weasley)),
    assertion(uncle(ron_weasley, albus_potter)),
    assertion(uncle(ron_weasley, dominique_weasley)),
    assertion(father-in-law(arthur_weasley, hermione_granger)),
    assertion(mother-in-law(molly_weasley, hermione_granger)).

:- end_tests(test).

% Run tests with the query “run_tests.”
