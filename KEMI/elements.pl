/* 
    Facts

*/

% Elements
chemical_element(hydrogen).
chemical_element(carbon).
symbol('H').

% Element names
element_name(S, E) :- 
    chemical_element(E),
    symbol(S).

element_name('H', hydrogen).
element_name('C', carbon).


% % compound(methane, chemical_element(hydrogen), chemical_element(carbon)).