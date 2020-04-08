:- module(stoichiometry,[percent_composition/3,percent_composition/2,molar_mass/2]).

:- use_module(uchem,[count_atoms/2]).
:- use_module(unums,[multiply/3,divide/3,plus_/3]).
:- use_module(facts,[element_fact/5]).

molar_mass_(Atom, MW) :-
    Atom = Element-Amount,
    element_fact(Element, _, _, _, AtomicW),
    multiply(Amount, AtomicW, MW).


%!  molar_mass(+Formula:string, +MW:real) is semidet.
%!  molar_mass(+Formula:string, -MW:real) is semidet.
%!  molar_mass(-Formula:string, +MW:real) is failure.
%!  molar_mass(-Formula:string, -MW:real) is failure.
%
%   True when the chemical compound with formula Formula has molar mass MW.
%
molar_mass(Formula, MW) :-
    count_atoms(Formula, Elements),
    maplist(molar_mass_, Elements, MWList),
    foldl(plus_, MWList, 0, MW).


%!  percent_composition(+Formula:string, -PercentComps:list(Element:atom-PC:int)) is det.
%
%   True when all elements in the chemical compound with formula Formula
%   have percent composition contained in PercentComps.
%
percent_composition(Formula, PercentComps) :-
    count_atoms(Formula, Atoms),
    maplist(molar_mass_, Atoms, MWList),
    foldl(plus_, MWList, 0, TotalMW),
    maplist(rdivide(TotalMW), MWList, PCList_),
    maplist(multiply(100), PCList_, PCList),
    pairs_keys_values(Atoms, Elements, _),
    pairs_keys_values(PercentComps, Elements, PCList).

rdivide(A, B, C) :- divide(B, A, C).


%!  percent_composition(+Formula:string, -Element:atom, -PercentComp:int) is multi.
%!  percent_composition(+Formula:string, +Element:atom, -PercentComp:int) is semidet.
%!  percent_composition(-Formula:string, ?Element:atom, ?PercentComp:int) is failure.
%
%   True when the element Element in the chemical compound with formula Formula
%   has percent composition equal to PercentComp.
%
percent_composition(Formula, Element, PercentComp) :-
    nonvar(Element), var(PercentComp),
    percent_composition_(Formula, Element, PercentComp),
    !.
percent_composition(Formula, Element, PercentComp) :-
    % var(Element), var(PercentComp),
    percent_composition_(Formula, Element, PercentComp).
percent_composition_(Formula, Element, PercentComp) :-
    percent_composition(Formula, PercentComps),
    member(Element-PercentComp, PercentComps).
