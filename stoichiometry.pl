:- use_module(uchem,[count_atoms/2]).
:- use_module(unums,[multiply/3,divide/3,plus_/3]).
:- use_module(facts,[element_fact/5]).

molar_mass_(Atom, MW) :-
    Atom = Element-Amount,
    element_fact(Element, _, _, _, AtomicW),
    multiply(Amount, AtomicW, MW).


%!  molar_mass(+Formula:string, -MW:real) is det.
%!  molar_mass(-Formula:string, +MW:real) is failure.
%
%   Calculate molecular mass of `Formula`.
%
molar_mass(Formula, MW) :-
    count_atoms(Formula, Elements),
    maplist(molar_mass_, Elements, MWList),
    foldl(plus_, MWList, 0, MW).


%!  percent_composition(+Formula:string, -PercentComps:list(Element-PC)) is det.
%
%   Calculate percent composition (weight) of elements in `Formula`.
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


%!  percent_composition(+Formula:string, -Element:atom, -PercentComps:list(Element-PC)) is multi.
%!  percent_composition(+Formula:string, +Element:atom, -PercentComps:list(Element-PC)) is semidet.
%!  percent_composition(-Formula:string, ?Element:atom, ?PercentComps:list(Element-PC)) is failure.
%
%   Calculate percent composition (weight) of `Element` in `Formula`.
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
