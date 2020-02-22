:- use_module('str', [replace/4, capitalize/2]).
:- use_module('elements',[en/2, element_name/2, metal/1, nonmetal/1, extract_elements_from_formula/2]).

basic_metal_cation(Symbol, Name) :-
    metal(Symbol),
    element_name(Symbol, Name).

basic_nonmetal_anion(Symbol, Name) :-
    nonmetal(Symbol),
    element_name(Symbol, ElementName),
    replace(ElementName, "ine", "", NextPrefix),
    string_concat(NextPrefix, "ide", Name).


%!  symbol_to_name_ionic(+Formula: string, -Name: string) 
%
%   Given the molecular formula of an ionic compound,
%   then resolve its name.
%
%   TODO: more sophistication
symbol_to_name_ionic(Formula, Name) :-
    extract_elements_from_formula(Formula, [H, T|_]),
    atom_string(H, Next),
    basic_metal_cation(Next, Prefix),
    capitalize(Prefix, Caps),
    atom_string(T, Anion),
    basic_nonmetal_anion(Anion, Suffix),
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
