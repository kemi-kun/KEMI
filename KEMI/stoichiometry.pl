:- use_module(uchem,[count_atoms/2]).
:- use_module(utils,[multiply/3]).
:- use_module(facts,[element_fact/5]).

plus_(Num1, Num2, Result) :-
    Result is Num1 + Num2.

molecular_weight_(Atom, MolarMass) :-
    Atom = Element-Amount,
    element_fact(Element, _, _, _, AtomicW),
    multiply(Amount, AtomicW, MolarMass).
%!  molecular_weight(+Formula:string, -MolarMass:real) is det.
%!  molecular_weight(-Formula:string, +MolarMass:real) is failure.
%
%   Calculate molecular mass of `Formula`
%
molecular_weight(Formula, MolarMass) :-
    count_atoms(Formula, Elements),
    maplist(molecular_weight_, Elements, MolarMassList),
    foldl(plus_, MolarMassList, 0.0, MolarMass).
