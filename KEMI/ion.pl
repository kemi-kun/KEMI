:- module('ion', [binary_stoichiometric_name/2]).
:- use_module('str', [replace/4, capitalize/2]).
:- use_module('elements',[en/2, element_name/2, extract_elements_from_formula/2, halogen/1]).
:- use_module('utilities', [extract_term/2]).
:- use_module('facts', [multiplicative_prefix/2]).

sub_suffix(carbon, "on").
sub_suffix(nitrogen, "ogen").
sub_suffix(phosphorus, "orus").
sub_suffix(hydrogen, "ogen").
sub_suffix(oxygen, "ygen").
sub_suffix(sulfur, "ur").

type1_anion_name(ElementName, Result) :-
    (halogen(ElementName) -> replace(ElementName, "ine", "", Name);
    sub_suffix(ElementName, Sub) -> replace(ElementName, Sub, "", Name); 
    Name = ElementName),
    string_concat(Name, "ide", Result).

basic_cation(Term, Name) :-
    extract_term(Term, [Symbol, Quantity|_]),
    Quantity > 1,
    element_name(Symbol, ElementName),
    multiplicative_prefix(Quantity, Prefix),
    string_concat(Prefix, ElementName, Name).

basic_cation(Term, Name) :-
    extract_term(Term, [Symbol|_]),
    element_name(Symbol, Name).

basic_anion(Term, Name) :-
    extract_term(Term, [Symbol, Quantity|_]),
    Quantity > 1,
    element_name(Symbol, ElementName),
    type1_anion_name(ElementName, AnionName),
    multiplicative_prefix(Quantity, Prefix),
    string_concat(Prefix, AnionName, Name).

basic_anion(Term, Name) :-
    extract_term(Term, [Symbol, Quantity|_]),
    Quantity = 1,
    element_name(Symbol, ElementName),
    type1_anion_name(ElementName, Name).

%!  symbol_to_name_ionic(+Formula: string, -Name: string) 
%
%   Given the molecular formula of an ionic compound,
%   then resolve its name.
%
%   TODO: more sophistication
binary_stoichiometric_name(Formula, Name) :-
    extract_elements_from_formula(Formula, [Cation, Anion|_]),
    basic_cation(Cation, Prefix),
    Caps = Prefix,
    basic_anion(Anion, Suffix),
    string_concat(Caps, " ", Then),
    string_concat(Then, Suffix, Name).


%!  engt(+Element1: string, +Element2: string, -Result: string) is det
%   
%   Given element symbols, compare their electronegativity (EN),
%   then return the symbol of element with the highest EN.
%   
engt(Element1, Element2, Result) :-
    en(Element1, En1),
    en(Element2, En2),
    En1 > En2,
    Result = Element1,
    !.
engt(Element1, Element2, Result) :-
    en(Element1, En1),
    en(Element2, En2),
    En1 < En2,
    Result = Element2,
    !.
