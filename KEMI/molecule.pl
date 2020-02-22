
:- module(molecule, []).
:- use_module(elements, [element_name/2]).
:- use_module(utilities, [extract_term/2]).

homonuclear_molecule(Element, Quantity) :-
    element_name(_, Element),
    Quantity > 1.

homonuclear_molecule_quantity(Molecule, Quantity) :-
    call(Molecule),
    extract_term(Molecule, [_,Quantity|_]).

homonuclear_diatomic(Molecule) :-
    homonuclear_molecule_quantity(Molecule, Quantity),
    Quantity = 2.

% formula_to_homonuclear_molecule(Raw, Elements) :-
%     re_split("([1-9][0-9]*)"/n, Raw, [RawSymbol, Number|_], []),
%     element_name(Symbol, Element),
%     Molecule =.. [homonuclear_molecule, Element, Number],
%     call(Molecule),
%     atom_string(RawSymbol, Symbol).

