:- module('ion', [binary_stoichiometric_name/2]).
:- use_module('str', [replace/4, capitalize/2, contains/2, remove/3]).
:- use_module('elements',[element_quantity/2, extract_elements_from_formula/2, halogen/1]).
:- use_module('utils', [extract_term/2]).
:- use_module('facts', [en/2, element_name/2, multiplicative_prefix/2]).

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

element_prefix_(Symbol, Prefix) :-
    element_name(Symbol, Name),
    replace_ending_ma_(Name, Prefix).

%
% IR-5.3.2.2
%
monatomic_cation_name(ElementName, Result) :-
    element_name(_, ElementName),
    Result = ElementName.

%
% IR-5.3.2.3
%
homopolyatomic_cation_name(Term, Result) :-
    extract_term(Term, [Symbol, Quantity|_]),
    Quantity > 1,
    element_name(Symbol, E),
    multiplicative_prefix(Quantity, Prefix),
    string_concat(Prefix, E, Result).

%
% IR-5.3.2.4
%
heteropolyatomic_cation_name([First, Second|_], Name) :-
    % Charge < 0,
    % Quantified is -Charge,
    extract_term(First, [FSymbol, FQ|_]),
    extract_term(Second, [SSymbol, SQ|_]),
    engt(FSymbol, SSymbol, Terminal),
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
monatomic_anion_name(ElementName, Result) :-
    element_name(Sym, ElementName),
    element_prefix_(Sym, Name),
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

%
% IR-5.3.3.4
%
heteropolyatomic_anion_name([First, Second|_], Name) :-
    extract_term(First, [FSymbol, FQ|_]),
    extract_term(Second, [SSymbol, SQ|_]),
    engt(FSymbol, SSymbol, Terminal),
    (FSymbol == Terminal, Q = FQ; Q = SQ),
    delete([FSymbol, SSymbol], Terminal, [Central|_]),
    element_prefix_(Terminal, Prefix),
    element_prefix_(Central, Suffix),
    (Q > 1, multiplicative_prefix(Q, Mult),
     string_concat(Mult, Prefix, S1); S1 = Prefix),
    string_concat(S1, "ido", S2),
    string_concat(S2, Suffix, S3),
    string_concat(S3, "ate", Name).

%!  binary_stoichiometric_name(+Formula: string, -Name: string) 
%
%   Given the molecular formula of an ionic compound,
%   then resolve its name.
%
%   TODO: more sophistication
binary_stoichiometric_name(Formula, Name) :-
    extract_elements_from_formula(Formula, [Cation|Anion]),
    classify_cation(Cation, Prefix),
    Caps = Prefix,
    classify_anion(Anion, Suffix),
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
    (
        En1 > En2 -> Result = Element1;
        En2 > En1 -> Result = Element2
    ).

classify_cation(Any, Cation) :-
    % length(Any, 1),
    % [H|_] = Any,
    extract_term(Any, [Symbol, Quantity|_]),
    Quantity = 1,
    element_name(Symbol, ElementName),
    monatomic_cation_name(ElementName, Cation),
    !.

classify_cation(Any, Cation) :-
    length(Any, 1),
    [H|_] = Any,
    homopolyatomic_cation_name(H, Cation),
    !.

classify_cation(Any, Cation) :-
    heteropolyatomic_cation_name(Any, Cation),
    !.

classify_anion(Any, Anion) :-
    length(Any, 1),
    [H|_] = Any,
    extract_term(H, [Symbol, Quantity|_]),
    Quantity = 1,
    element_name(Symbol, ElementName),
    monatomic_anion_name(ElementName, Anion),
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
%     engt(First, Second, R),
%     extract_term(Term, [Symbol, Quantity|_]).
