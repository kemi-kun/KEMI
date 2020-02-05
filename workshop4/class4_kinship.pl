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
