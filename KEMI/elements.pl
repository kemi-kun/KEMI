/* 
    Facts

*/
:- module(elements,[element_quantity/2, extract_elements_from_formula/2, halogen/1]).
:- use_module(utilities, [extract_term/2]).
:- use_module(facts, [element_name/2]).

%
%
% TODO: validate each element
%

halogen(fluorine).
halogen(chlorine).
halogen(bromine).
halogen(iodine).
halogen(astatine).

element_quantity(Symbol, Quantity) :-
    Quantity > 0,
    element_name(Symbol, _).
    
formula_to_quantified(Raw, Elements) :-
    re_split("([1-9][0-9]*)"/n, Raw, [RawSymbol, Quantity|_], []),
    element_name(Symbol, _),
    Elements =.. [element_quantity, Symbol, Quantity],
    call(Elements),
    atom_string(RawSymbol, Symbol).

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
        is_upper(Out) -> 
        extract_elements_(Formula, Trim, "", End2); 
        extract_elements_(Formula, Trim, ConcatString, End2),
    !) ,
    (is_upper(Out) -> determine_multiplicity(ConcatString, Result), Sth = [Result]; Sth = []),
    append(End2, Sth, End).