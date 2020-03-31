:- use_module(uchem,[count_atoms/2]).
:- use_module(utils,[multiply/3,plus_/3]).
:- use_module(facts,[element_fact/5]).

molecular_weight_(Atom, MW) :-
    Atom = Element-Amount,
    element_fact(Element, _, _, _, AtomicW),
    multiply(Amount, AtomicW, MW).

%!  molecular_weight(+Formula:string, -MW:real) is det.
%!  molecular_weight(-Formula:string, +MW:real) is failure.
%
%   Calculate molecular mass of `Formula`
%
molecular_weight(Formula, MW) :-
    count_atoms(Formula, Elements),
    maplist(molecular_weight_, Elements, MWList),
    foldl(plus_, MWList, 0, MW).
