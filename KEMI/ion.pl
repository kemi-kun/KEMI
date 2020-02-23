:- module('ion', [binary_stoichiometric_name/2]).
:- use_module('str', [replace/4, capitalize/2, contains/2, remove/3]).
:- use_module('elements',[en/2, element_name/2, element_quantity/2, extract_elements_from_formula/2, halogen/1]).
:- use_module('utilities', [extract_term/2]).
:- use_module('facts', [multiplicative_prefix/2]).

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

monatomic_anion_name(ElementName, Result) :-
    element_name(Sym, ElementName),
    element_prefix_(Sym, Name),
    string_concat(Name, "ide", Result).

homopolyatomic_anion_name(Term, Result) :-
    extract_term(Term, [Symbol, Quantity|_]),
    Quantity > 1,
    element_name(Symbol, _),
    multiplicative_prefix(Quantity, Prefix),
    element_prefix_(Symbol, ElementPrefix),
    string_concat(Prefix, ElementPrefix, Name),
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

%!  binary_stoichiometric_name(+Formula: string, -Name: string) 
%
%   Given the molecular formula of an ionic compound,
%   then resolve its name.
%
%   TODO: more sophistication
binary_stoichiometric_name(Formula, Name) :-
    extract_elements_from_formula(Formula, [Cation|Anion]),
    basic_cation(Cation, Prefix),
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
    En1 > En2,
    Result = Element1,
    !.
engt(Element1, Element2, Result) :-
    en(Element1, En1),
    en(Element2, En2),
    En1 < En2,
    Result = Element2,
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

%
% IR-5.3.3.4
%
heteropolyatomic_anion_name([First, Second|_], Name) :-
    % Charge < 0,
    % Quantified is -Charge,
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
    % string_concat(StepOne, "(", StepTwo),
    % string_concat(StepTwo, Quantified, StepThree),
    % string_concat(StepThree, "-)", Name).
    % extract_term(Term, [Symbol, Quantity|_]).

% heteropolyatomic_anion([First, Second|_], Name) :-
%     engt(First, Second, R),
%     extract_term(Term, [Symbol, Quantity|_]).
