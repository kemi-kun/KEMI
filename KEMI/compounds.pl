use_module('elements').

made_of(methane, hydrogen).

made_of(X, Y) :-
    chemical_element(Y).

% Compounds
compound(X, [H|T]) :-
    made_of(X, H),
    compound(X, T).

compound(methane, [hydrogen, carbon]).
