:- module(icompositional,[boron_hydride_stoichiometric/2,general_stoichiometric/2,addition_compound_cn/2,ion_cn/2,binary_compound_cn/2,homonuclear_cn/2]).
:- use_module(chemutils,[get_element/3]).
:- use_module(elements,[element_name/2]).


homonuclear_cn(Formula, Name) :-
    get_element(Formula, 0, Element),
    get_num_atoms(Formula, Element, Amount),
    element_name(Element, ElementName),
    group(Element, 18) ->
        mul_prefix_except_mono(Amount, MulPrefix);
    multiplicative_prefix(Amount, MulPrefix),
    string_concat(MulPrfix, ElementName, Name).
 
 
binary_compound_cn(Formula, Name) :-
    fail.
ion_cn(Formula, Name) :-
    fail.
addition_compound_cn(Formula, Name) :-
    fail.

general_stoichiometric(Formula, Name) :-
    fail.
boron_hydride_stoichiometric(Formula, Name) :-
    fail.
