/* 
    Facts

*/

:- use_module('str', [replace/4, capitalize/2]).


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
%   Given a molecular formula,
%   then resolve its name.
%
%   TODO: more complications
symbol_to_name_ionic(Formula, Name) :-
    extract_elements(Formula, [H, T|_]),
    atom_string(H, Next),
    basic_metal_cation(Next, Prefix),
    capitalize(Prefix, Caps),
    atom_string(T, Anion),
    basic_nonmetal_anion(Anion, Suffix),
    string_concat(Caps, " ", Then),
    string_concat(Then, Suffix, Name).

%
%
% TODO: validate each element
%

extract_elements(Formula, Elements) :-
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

    



% % compound(methane, chemical_element(hydrogen), chemical_element(carbon)).

% extract_elements(String, Elements) :-
%     write_ln("base"),
%     string_length(String, Length),
%     Length = 1,
%     Elements = [String],
%     !.

% extract_elements(String, Elements) :-
%     length(Elements, ListLength),
%     write_ln("2"),
%     write_ln(String),
%     string_length(String, Length),
%     SubLength is Length-1,
%     Length > 1,
%     sub_string(String, 0, SubLength, 1, OutString),
%     write_ln(OutString),
    
%     extract_elements(OutString, ElementsB),
%     append(ElementsB, [String], Elements),
%    % NaCl
    
%     % append(Output, [], Output2),
%     true.
