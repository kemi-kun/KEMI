/* 
    Facts

*/

% Elements
chemical_element(hydrogen).
chemical_element(carbon).

% Element names
element_name(S, E) :- 
    chemical_element(E),
    S.

element_name('H', chemical_element(hydrogen)).
element_name('C', chemical_element(carbon)).

made_of(methane, chemical_element(hydrogen)).

compound(a, []).

compound(X, [H|T]) :-
    made_of(X, H),
    compound(X, T).

compound(methane, [chemical_element(hydrogen), chemical_element(carbon)]).
% compound(methane, chemical_element(hydrogen), chemical_element(carbon)).