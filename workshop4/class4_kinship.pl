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
parent(albus_potter, ginny_weasley).
parent(albus_potter, harry_potter).
spouse(angelina_johnson, george_weasley).
spouse(arthur_weasley, molly_weasley).
parent(bill_weasley, arthur_weasley).
parent(bill_weasley, molly_weasley).
spouse(bill_weasley, fleur_delacour).
parent(charlie_weasley, arthur_weasley).
parent(charlie_weasley, molly_weasley).
parent(dominique_weasley, bill_weasley).
parent(dominique_weasley, fleur_delacour).
parent(dudley_dursley, petunia_dursley).
parent(dudley_dursley, vernon_dursley).
spouse(euphemia_potter, fleamont_potter).
spouse(fleamont_potter, euphemia_potter).
spouse(fleur_delacour, bill_weasley).
parent(fred_johnson_weasley, angelina_johnson).
parent(fred_johnson_weasley, george_weasley).
parent(fred_weasley, arthur_weasley).
parent(fred_weasley, molly_weasley).
parent(george_weasley, arthur_weasley).
parent(george_weasley, molly_weasley).
spouse(george_weasley, angelina_Johnson).
parent(ginny_weasley, arthur_weasley).
parent(ginny_weasley, molly_weasley).
spouse(ginny_weasley, harry_potter).
parent(harry_potter, lilly_potter).
parent(harry_potter, james_potter).
spouse(harry_potter, ginny_weasley).
spouse(hermione_granger, ron_weasley).
parent(hugo_granger-weasley, ron_weasley).
parent(hugo_granger-weasley, hermione_granger).
parent(james_potter, euphemia_potter).
parent(james_potter, fleamont_potter).
spouse(james_potter, lilly_potter).
parent(james_s_potter, harry_potter).
parent(james_s_potter, ginny_weasley).
spouse(lilly_potter, james_potter).
parent(lilly_l_potter, harry_potter).
parent(lilly_l_potter, ginny_weasley).
parent(louis_weasley, bill_weasley).
parent(louis_weasley, fleur_delacour).
spouse(molly_weasley, arthur_weasley).
parent(percy_weasley, arthur_weasley).
parent(percy_weasley, molly_weasley).
spouse(petunia_dursley, vernon_dursley).
parent(ron_weasley, arthur_weasley).
parent(ron_weasley, molly_weasley).
spouse(ron_weasley, hermione_granger).
parent(rose_granger-weasley, ron_weasley).
parent(rose_granger-weasley, hermione_granger).
parent(roxanne_weasley, angelina_johnson).
parent(roxanne_weasley, george_weasley).
spouse(vernon_dursley, petunia_dursley).
parent(victoire_weasley, bill_weasley).
parent(victoire_weasley, fleur_delacour).


% Rules
mother(X,Y) :- female(X), parent(X,Y). %✅
father(X,Y) :- male(X), parent(X,Y).
child(X,Y) :- parent(Y,X).
son(X,Y) :- child(X,Y), male(X).
daughter(X,Y) :- child(X,Y), female(X).
sibling(X,Y) :- parent(Z,X), parent(Z,Y), X \= Y.
sister(X,Y) :- sibling(X,Y), female(X).
brother(X,Y) :- sibling(X,Y), male(X).
husband(X,Y) :- spouse(X,Y), male(X).
wife(X,Y) :- spouse(X,Y), female(X).

cousin(X,Y) :- child(X,sibling(Z,parent(W,Y))).
niece(X,Y) :- daughter(X,Z), sibling(Z,Y).
niece(X,Y) :- daughter(X,Z), sibling(Z,W), spouse(W,Y).
grandmother(X,Y) :- mother(X,Z), parent(Z,Y).
grandfather(X,Y) :- father(X,Z), parent(Z,Y).
