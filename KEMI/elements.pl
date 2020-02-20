/* 
    Facts

*/
:- module(elements,[element_name/2, metal/1,nonmetal/1, extract_elements_from_formula/2]).


% Elements
chemical_element(hydrogen).
chemical_element(carbon).
symbol('H').

% Element names
element_name(S, E) :- 
    chemical_element(E),
    symbol(S).

element_name('H', hydrogen).
element_name('C', carbon).
element_name("Na", sodium).
element_name("Cl", chlorine).

metal("Na").
nonmetal("Cl").

%
%
% TODO: validate each element
%

extract_elements_from_formula(Formula, Elements) :-
    extract_elements_(Formula, 0, "", Elements).

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
    (is_upper(Out) -> Sth = [ConcatString]; Sth = []),
    append(End2, Sth, End).

    




