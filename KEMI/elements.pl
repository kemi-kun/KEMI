/* 
    Facts

*/

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
