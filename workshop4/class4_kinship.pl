<<<<<<< HEAD
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
=======
parent(pam,bob).
parent(tom,bob).
parent(tom,liz).
parent(bob,ann).
parent(bob,pat).
parent(pat,jim).
father(X,Y) :- parent(X,Y), male(X).
mother(X,Y) :- parent(X,Y),female(X).
grandparent(X,Z) :- parent(X,Y), parent(Y,Z).
child(X,Y) :- parent(Y,X).
sister(X,Y) :- parent(Z,X), parent(Z,Y), female(X), X\=Y.
ancester(X,Y) :- parent(X,Y).
ancester(X,Y) :- parent(X,W), ancester(W,Y).
same(X,Y) :- X=Y.
diff(X,Y) :- not(same(X,Y)).
>>>>>>> 718ea89f022ad9dd48b74065324139ecef8b5f1b
