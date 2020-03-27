:- module('ion', [binary_stoichiometric_name/2]).
:- use_module('str', [replace/4, capitalize/2, contains/2, remove/3]).
:- use_module('elements',[element_quantity/2, extract_elements_from_formula/2, standard_bonding_number_of/2, halogen/1]).
:- use_module('molecule', [homonuclear_polyatomic_name/2]).
:- use_module('utils', [extract_term/2]).
:- use_module('facts', [en/2, element_name/2, synonymous/2, multiplicative_prefix/2]).

other_name("N", "az").
other_name("O", "oxid").

anion("H", hydride, -1).

replace_ending_(ElementName, [], Result):-
    Result = ElementName, !.

replace_ending_(ElementName, [Removed|Next], Result) :-
    (str:contains(ElementName, Removed),
    str:remove(ElementName, Removed, PrevResult),
    replace_ending_(PrevResult, Next, Result), !); replace_ending_(ElementName, Next, Result).

replace_ending_once_(ElementName, [Removed|Next], Result) :-
    (str:contains(ElementName, Removed),
    str:remove(ElementName, Removed, Result), !); replace_ending_(ElementName, Next, Result).

replace_ending_ma_(ElementName, Result) :-
    replace_ending_once_(ElementName, ["ogen", "ygen", "en", "ese", "ic", "ine", "ium", "on", "orus", "um", "ur", "y"], Result).

replace_ending_ha_(ElementName, Result) :-
    replace_ending_once_(ElementName, ["ogen", "ygen", "en", "ese", "ic", "ine", "ium", "orus", "um", "ur", "y"], Result).

element_prefix_(Symbol, Prefix) :-
    (synonymous(Symbol, Name); element_name(Symbol, Name)),
    replace_ending_ma_(Name, Prefix).

element_prefix_ha_(Symbol, Prefix) :-
    (synonymous(Symbol, Name); element_name(Symbol, Name)),
    replace_ending_ha_(Name, Prefix).


% TODO: Parent hydride ions
% -- Polyatomic parent hydride ions

% +Element, -Name, -Charge
common_monatomic_ion(Element, Name, Charge) :-
    octet_rule_oxidation_number(Element, Charge),
    (Charge > 0 -> monatomic_cation_name(Element, Name);
    Charge < 0 -> monatomic_anion_name(Element, Name);
    fail).

%
% IR-5.3.2.2
%  
monatomic_cation(Symbol, Name, Charge) :-
    Charge > 0,
    common_monatomic_ion(Symbol, Name, Charge).
%
% IR-5.3.2.2
%
monatomic_cation_name(Symbol, Result) :-
    element_name(Symbol, ElementName),
    atom_string(ElementName, Result).

%
% IR-5.3.2.3
%
homopolyatomic_cation_name(Term, Result) :-
    extract_term(Term, [Symbol, Quantity|_]),
    Quantity > 1,
    element_name(Symbol, E),
    multiplicative_prefix(Quantity, Prefix),
    string_concat(Prefix, E, Result).


heteropolyatomic_ion([First, Second|_], Name, Charge) :- 
    extract_term(First, [FSymbol, FQ|_]),
    extract_term(Second, [SSymbol, SQ|_]),
    en_gt(FSymbol, SSymbol, _),
    octet_rule_oxidation_number(FSymbol, C1),
    octet_rule_oxidation_number(SSymbol, C2),
    Charge is (C1*FQ) + (C2*SQ),
    (Charge > 0 -> heteropolyatomic_cation_name([First, Second], Name);
    heteropolyatomic_anion_name([First, Second], Name)).

%
% IR-5.3.2.4
%
heteropolyatomic_cation_name([First, Second|_], Name) :-
    % Charge < 0,
    % Quantified is -Charge,
    extract_term(First, [FSymbol, FQ|_]),
    extract_term(Second, [SSymbol, SQ|_]),
    en_gt(FSymbol, SSymbol, Terminal),
    (FSymbol == Terminal, Q = FQ; Q = SQ),
    delete([FSymbol, SSymbol], Terminal, [Central|_]),
    element_prefix_(Terminal, Prefix),
    element_name(Central, Suffix),
    (Q > 1, multiplicative_prefix(Q, Mult),
    string_concat(Mult, Prefix, S1), !; S1 = Prefix),
    string_concat(S1, "ido", S2),
    string_concat(S2, Suffix, Name).

%
% IR-5.3.3.2
%
monatomic_anion_name(Symbol, Result) :-
    element_prefix_(Symbol, Name),
    string_concat(Name, "ide", Result).

%
% IR-5.3.3.3
%
homopolyatomic_anion_name(Term, Result) :-
    extract_term(Term, [Symbol, Quantity|_]),
    Quantity > 1,
    element_prefix_(Symbol, E),
    multiplicative_prefix(Quantity, Prefix),
    string_concat(Prefix, E, Name),
    string_concat(Name, "ide", Result).

parent_from_hydride(First, Second, Parent) :-
    extract_term(First, [FirstSymbol, _|_]),
    extract_term(Second, [SecondSymbol, _|_]),
    (element_name(FirstSymbol, hydrogen) -> Parent = SecondSymbol; 
     element_name(SecondSymbol, hydrogen) -> Parent = FirstSymbol; fail).

hydroxide(First, Second) :-
    extract_term(First, [FirstSymbol, _|_]),
    extract_term(Second, [SecondSymbol, _|_]),
    (element_name(FirstSymbol, hydrogen) -> element_name(SecondSymbol, oxygen); 
        element_name(SecondSymbol, hydrogen) -> element_name(FirstSymbol, oxygen); fail).
%
% IR-5.3.3.4
%
heteropolyatomic_anion_name(L, Name) :-
    length(L, 3),
    [Third, First, Second|_] = L,
    % extract_term(Third, [TSymbol, TQ|_]),
    extract_term(First, [FSymbol, FQ|_]),
    extract_term(Second, [SSymbol, SQ|_]),
    homonuclear_polyatomic_name(Third, S0),
    en_gt(FSymbol, SSymbol, Terminal),
    (FSymbol == Terminal, Q = FQ; Q = SQ),
    delete([FSymbol, SSymbol], Terminal, [Central|_]),
    element_prefix_(Terminal, Prefix),
    element_prefix_ha_(Central, Suffix),
    (Q > 1, multiplicative_prefix(Q, Mult),
     string_concat(Mult, Prefix, S1); S1 = Prefix),
    string_concat(S0, "(", T0),
    string_concat(T0, S1, T1),
    string_concat(T1, "ido", S2),
    string_concat(S2, Suffix, S3),
    string_concat(S3, "ate)", Name), !.

%
% IR-5.3.3.4
%
heteropolyatomic_anion_name([First, Second|_], Name) :-
    (
        parent_hydride_anion_name([First, Second], Name), !;
    extract_term(First, [FSymbol, FQ|_]),
    extract_term(Second, [SSymbol, SQ|_]),
    
    en_gt(FSymbol, SSymbol, Terminal),
    (FSymbol == Terminal, Q = FQ; Q = SQ),
    delete([FSymbol, SSymbol], Terminal, [Central|_]),
    element_prefix_(Terminal, Prefix),
    element_prefix_ha_(Central, Suffix),
    (Q > 1, multiplicative_prefix(Q, Mult),
     string_concat(Mult, Prefix, S1); S1 = Prefix),
    string_concat(S1, "ido", S2),

    string_concat(S2, Suffix, S3),
    string_concat(S3, "ate", Name)).


parent_hydride_cation_name([F,S|_], Name) :-
    parent_from_hydride(F, S, Symbol),
    (other_name(Symbol, Prefix); element_prefix_(Symbol, Prefix)),
    string_concat(Prefix, "anium", Name).

parent_hydride_anion_name([F,S|_], Name) :-
    hydroxide(F, S), Name = "hydroxide"
    ;( parent_from_hydride(F, S, Symbol), 
    (other_name(Symbol, Prefix); element_prefix_(Symbol, Prefix)),
    string_concat(Prefix, "anide", Name)).

%!  binary_stoichiometric_name(+Formula: string, -Name: string) 
%
%   Given the molecular formula of an ionic compound,
%   then resolve its name.
%
%   TODO: more sophistication
binary_stoichiometric_name(Formula, Name) :-
    extract_elements_from_formula(Formula, X),
    ([F, S|Anion] = X, 
        length(Anion, L), 
        L > 0, 
        parent_from_hydride(F, S, _), 
        Cation = [F, S], parent_hydride_cation_name(Cation, Prefix), !; 
    [Cation|Anion] = X, classify_cation(Cation, Prefix)),
    % classify_cation(Cation, Prefix),
    classify_anion(Anion, Suffix),
    string_concat(Prefix, " ", Then),
    string_concat(Then, Suffix, Name).


%!  en_gt(+Element1: string, +Element2: string, -Result: string) is det.
%   
%   Given element symbols, compare their electronegativity (EN),
%   then return the symbol of element with the highest EN.
%   
en_gt(Element1, Element2, Result) :-
    en(Element1, En1),
    en(Element2, En2),
    (
        En1 > En2 -> Result = Element1;
        En2 > En1 -> Result = Element2
    ).

classify_cation(Any, Cation) :-
    % length(Any, 1),
    % [H|_] = Any,
    
    extract_term(Any, [Symbol, Quantity|_]),
    Quantity = 1,
    monatomic_cation_name(Symbol, Cation), !.

classify_cation(Any, Cation) :-
    % length(Any, 1),
    compound(Any),
    % [H|_] = Any,
    homopolyatomic_cation_name(Any, Cation), !.

classify_cation(Any, Cation) :-
    heteropolyatomic_cation_name(Any, Cation), !.

classify_anion(Any, Anion) :-
    length(Any, 1),
    [H|_] = Any,
    extract_term(H, [Symbol, Quantity|_]),
    Quantity = 1,
    monatomic_anion_name(Symbol, Anion),
    !.

classify_anion(Any, Anion) :-
    length(Any, 1),
    [H|_] = Any,
    homopolyatomic_anion_name(H, Anion),
    !.

classify_anion(Any, Anion) :-
    heteropolyatomic_anion_name(Any, Anion),
    !.

% heteropolyatomic_anion([First, Second|_], Name) :-
%     en_gt(First, Second, R),
%     extract_term(Term, [Symbol, Quantity|_]).

%! get_electronegative_from_binary_compound(+Formula: string, -Result: compound<string, integer>) is det.
%
%  Given a formula of binary compound, 
%  then return electronegative element.
%
get_electronegative_from_binary_compound(Formula, Result) :- 
    extract_elements_from_formula(Formula, [Element1, Element2|_]),
    extract_term(Element1, [Symbol1|_]),
    extract_term(Element2, [Symbol2|_]),
    en_gt(Symbol1, Symbol2, E),
    (
        Symbol1 = E -> Result = Element1;
        Result = Element2
    ).

%! get_electropositive_from_binary_compound(+Formula: string, -Result: compound<string, integer>) is det.
%
%  Given a formula of binary compound, 
%  then return electropositive element.
%
get_electropositive_from_binary_compound(Formula, Result) :- 
    extract_elements_from_formula(Formula, [Element1, Element2|_]),
    extract_term(Element1, [Symbol1|_]),
    extract_term(Element2, [Symbol2|_]),
    en_gt(Symbol1, Symbol2, E),
    (
        Symbol1 = E -> Result = Element2;
        Result = Element1
    ).
