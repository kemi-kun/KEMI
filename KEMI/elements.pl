/* 
    Facts

*/
:- module(elements,[element_quantity/2, extract_elements_from_formula/2, octet_rule_oxidation_number/2, halogen/1]).
:- use_module(utils, [extract_term/2]).
:- use_module(facts, [en/2, num_protons/2, element_name/2]).

%
%
% TODO: validate each element
%


element_quantity(Symbol, Quantity) :-
    Quantity > 0,
    element_name(Symbol, _).
    
formula_to_quantified(Raw, Elements) :-
    re_split("([1-9][0-9]*)"/n, Raw, [RawSymbol, Quantity|_], []),
    element_name(Symbol, _),
    Elements =.. [element_quantity, Symbol, Quantity],
    call(Elements),
    atom_string(RawSymbol, Symbol).

%!  extract_elements_from_formula(+Formula:string, -Elements:list) is det.
%
%   Retur
%
extract_elements_from_formula(Formula, Elements) :-
    extract_elements_(Formula, 0, "", Elements).

determine_multiplicity(Formula, Result) :- 
    formula_to_quantified(Formula, Result).
determine_multiplicity(Formula, Result) :- 
    Result =.. [element_quantity, Formula, 1],
    call(Result).

extract_elements_(Formula, Start, _, _) :-
    string_length(Formula, Length),
    Start = Length.
extract_elements_(Formula, Start, String, End) :-
    string_length(Formula, Length),
    Start < Length,
    Final is Length - 1 - Start,
    sub_string(Formula, Final, _, Start, Out),
    Trim is Start + 1,
    string_concat(Out, String, ConcatString),
    (
        is_upper(Out) -> extract_elements_(Formula, Trim, "", End2); 
        extract_elements_(Formula, Trim, ConcatString, End2), !
    ),
    (
        is_upper(Out) -> determine_multiplicity(ConcatString, Result), 
                         Sth = [Result];
        Sth = []
    ),
    append(End2, Sth, End).

%! period(+Element:string, -Period:integer) is det.
%! period(-Element:string, +Period:integer) is multi.
%
%  True if element Element is in period Period.
%
period(Element, Period) :-
    num_protons(Element, Z),
    (
        Z =<   2 -> Period is 1;
        Z =<  10 -> Period is 2;
        Z =<  18 -> Period is 3;
        Z =<  36 -> Period is 4;
        Z =<  54 -> Period is 5;
        Z =<  86 -> Period is 6;
        Z =< 118 -> Period is 7;
        Period is 8
    ).

delta(X, Y, R) :- 
    (
        X > Y -> R is X - Y;
        Y > X -> R is Y - X
    ).

%! delta_en(+Element1:string, +Element2:string, -Result:float) is det.
%! delta_en(-Element1:string, -Element2:string, +Result:float) is multi.
%
%  Compute delta EN between Element1 and Element2
%
delta_en(Element1, Element2, Result) :- 
    en(Element1, En1), 
    en(Element2, En2),
    delta(En1, En2, Result),
    !.

%! ionic(+Element1:string, +Element2:string) is det.
%
%  True if both element form an ionic bond.
%
%  TODO: take formula instead of element
ionic(Element1, Element2) :-
    delta_en(Element1, Element2, Result),
    Result > 1.7.

%! polar_covalent(+Element1:string, +Element2:string) is det.
%
%  True if both element form a polar covalent bond.
%
%  TODO: take formula instead of element
polar_covalent(Element1, Element2) :-
    delta_en(Element1, Element2, Result),
    Result > 0.4,
    Result < 1.5.

%! nonpolar_covalent(+Element1:string, +Element2:string) is det.
%
%  True if both element form a nonpolar covalent bond.
%
%  TODO: take formula instead of element
nonpolar_covalent(Element1, Element2) :-
    delta_en(Element1, Element2, Result),
    Result > 0,
    Result < 0.4.

%  Facts for group 18
noble("He", 18).
noble("Ne", 18).
noble("Ar", 18).
noble("Kr", 18).
noble("Xe", 18).
noble("Rn", 18).
noble("Og", 18).

%! group(+Element:string, -Group:integer) is det.
%! group(-Element:string, +Group:integer) is multi.
%
%  True if element is group 18.
%
group(Element, Group) :-
    (
        Element = "He" -> Group is 18, !;
        Element = "Ne" -> Group is 18, !;
        Element = "Ar" -> Group is 18, !;
        Element = "Kr" -> Group is 18, !;
        Element = "Xe" -> Group is 18, !;
        Element = "Rn" -> Group is 18, !;
        Element = "Og" -> Group is 18, !;
        fail
    ).

%! group(+Element:string, -Group:integer) is det.
%! group(-Element:string, +Group:integer) is multi.
%
%  True if element has group.
%
group(Element, Group) :-
    num_protons(Element, Z),
    noble(NobleElement, 18),
    num_protons(NobleElement, Z2),
    T0 is 1,
    T1 is Z2 + 1,
    T2 is Z2 + 2,
    T3 is Z2 + 3,
    T17 is Z2 - 1,
    T16 is Z2 - 2,
    T15 is Z2 - 3,
    T14 is Z2 - 4,
    T13 is Z2 - 5,
    T12 is Z2 - 6,
    T11 is Z2 - 7,
    T10 is Z2 - 8,
    T9 is Z2 - 9,
    T8 is Z2 - 10,
    T7 is Z2 - 11,
    T6 is Z2 - 12,
    T5 is Z2 - 13,
    T4 is Z2 - 14,
    (  
        Z = T0 -> Group is 1;
        Z = T1 -> Group is 1;
        Z = T2 -> Group is 2;
        Z = T3 -> Group is 3;
        Z = T17 -> Group is 17;
        Z = T16 -> Group is 16;
        Z = T15 -> Group is 15;
        Z = T14 -> Group is 14;
        Z = T13-> Group is 13;
        Z = T12 -> Group is 12;
        Z = T11 -> Group is 11;
        Z = T10 -> Group is 10;
        Z = T9 -> Group is 9;
        Z = T8 -> Group is 8;
        Z = T7 -> Group is 7;
        Z = T6 -> Group is 6;
        Z = T5 -> Group is 5;
        Z = T4 -> Group is 4      
    ),
    !.

alkali(Element) :-
    alkali(Element, 1).

alkali(Element, Group) :-
    Group = 1,
    group(Element, Group).

alkali_earth(Element) :-
    alkali_earth(Element, 2).

alkali_earth(Element, Group) :-
    Group = 2,
    group(Element, Group).

pnictogen(Element) :-
    pnictogen(Element, 15).

pnictogen(Element, Group) :-
    Group = 15,
    group(Element, Group).

chalcogen(Element) :-
    chalcogen(Element, 16).

chalcogen(Element, Group) :-
    group(Element, Group),
    Group = 16.

halogen(Element) :-
    halogen(Element, 17).

halogen(Element, Group) :-
    group(Element, Group),
    Group = 17.

transition(Element) :-
    transition(Element, _).

transition(Element, Group) :-
    group(Element, Group),
    Group > 2, 
    Group < 13,
    not(lanthanide(Element)),
    not(actinide(Element)).

lanthanide(Element) :-
    num_protons(Element, Proton),
    Proton > 56, Proton < 72.

actinide(Element) :-
    num_protons(Element, Proton),
    Proton > 88, Proton < 104.

valence(Element, Valence) :-
    not(transition(Element)),
    group(Element, Group),
    (Group > 12, Valence is Group - 10, !; Valence = Group).

octet_rule_oxidation_number(Element, O) :-
    valence(Element, V),
    (V > 0, V < 4 -> O is V; 
    V > 4 -> O is -(8-V); O is 4; 
    O is -4).