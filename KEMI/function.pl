:- use_module('elements',[element_quantity/2, extract_elements_from_formula/2]).
:- use_module('utils', [extract_term/2]).

%!  list_remove(+In: list, +Element: atom, -Out: list) is det.
%!  list_remove(-In: list, +Element: atom, +Out: list) is det.
%  
%   Remove the first appearance of `Element` in list `In`
%   and return the result list as `Out`.
%   Return false if `Element` is not in `In`
%  
list_remove(In, Element, Out) :-
    selectchk(Element, In, Out).

%!  reversed(+In: list, -Out: list) is det.
%  
%   Reverse list `In`and return the result list as `Out`.
%   Return false if `In` is not list
%
reversed(In, Out) :-
    reverse(In, Out),
    !.

%!  enumerate(+List: list, -Pairs: list(Key-Value)) is det.
%!  enumerate(-List: list, +Pairs: list(Key-Value)) is det.
%  
%   True when =Pairs= is a _Pairs_ with index as key and element as value.
%
enumerate(List, Pairs) :-
    pairs_keys_values(Pairs, Keys, List),
    enumerate_(List, Keys).
enumerate_([H1], [H2]) :-
    H2 is 0,
    !.
enumerate_([H1|T1], [H2|T2]) :-
    enumerate_(T1, T2),
    nth0(0, T2, N_0),
    H2 is N_0 + 1.

%!  range(+Start: integer, +Start: integer, +Start: integer, -Range: list) is det.
%
%   True when `Range` is a list with range [Start, Stop).
%   Currently only one way.
%
range(Start, Stop, Step, Range) :-
    range_(Stop, Step, [Start|T]),
    append([Start], T, Range).
range_(Stop, Step, [H|T]) :-
    not(var(Step)),
    T = [],
    High is Stop-1,
    Low is Stop-Step,
    between(Low, High, H),
    !.
range_(Stop, Step, [H|T]) :-
    nth0(0, T, T0),
    T0 is H + Step,
    T0 < Stop,
    range_(Stop, Step, T).
 
%!  split(+In: string, -Out: list) is det.
%  
%   Split string `In` to list
%   and return the result list as `Out`.
%
split(In, Out) :-
    string_codes(In, Code),
    maplist(number_to_character,
       Code, Out).

number_to_character(Number, Character) :-
    string_codes(Character,[Number]).

%! split_decimal(+Number: integer, -Numbers: list) is det.
% 
%  Return the number splitted into digits.
%  
%  split_decimal(1234, [1, 2, 3, 4]).
split_decimal(Number, Numbers) :-
    number_chars(Number, Numbers_),
    maplist(atom_number, Numbers_, Numbers).

%! append_element(+List: list, +Element: atom, -Result: list) is det.
%! append_element(+List: list, -Element: atom, +Result: list) is det.
%
%  Append `Element` to a list `List`
%
append_element(List, Element, Result) :-
    append(List, [Element], Result).

%! remove_parentheses_(+String: string, -Result: string) is det.
%
%  Remove "(", ")", "[", "]", "{" and "}" from `String`.
%
% remove_parentheses_("NaCl", "NaCl").
% remove_parentheses_("[Al(POCl3)6]3+", "AlPOCl363+").
% remove_parentheses_("H2[PtCl6]", "H2PtCl6").
% remove_parentheses_("ab[(c)][d2](3)", "abcd23").
remove_parentheses_(String, Result) :-
    re_split("[(\\)\\[\\]\\{\\}]", String, R1, []),
    delete(R1, "(", R2),
    delete(R2, ")", R3),
    delete(R3, "[", R4),
    delete(R4, "]", R5),
    delete(R5, "{", R6),
    delete(R6, "}", R7),
    delete(R7, "", Result_),
    atomics_to_string(Result_, Result).

%! get_element(+Formula: string, +Index: integer, -Element: string) is det.
%
%  Get element at `Index` position in formula `Formula`.
%  (index starts at 0)
%  Return false if there's no element at `Index`
%
get_element(Formula, Index, Element) :-
    remove_parentheses_(Formula, Formula_),
    extract_elements_from_formula(Formula_, Elements),
    nth0(Index, Elements, ElementQuantity),
    extract_term(ElementQuantity, [Element|_]).

%! get_all_elements(+Formula: string, -ElementSet: list) is det.
%
%  Return a set (a list without duplicate) of elements in
%  formula `Formula`
%
get_all_elements(Formula, ElementSet) :-
    remove_parentheses_(Formula, Formula_),
    extract_elements_from_formula(Formula_, ElementQuantities),
    extract_element_quantity(ElementQuantities, ElementList),
    list_to_set(ElementList, ElementSet),
    !.

% [element_quantity("Na", 1), element_quantity("Cl", 1)] => ["Na", "Cl"]
extract_element_quantity([], []).
extract_element_quantity(ElementQuantities, Elements) :-
    ElementQuantities = [EleQuanH|EleQuanT],
    extract_term(EleQuanH, [EleH|_]),
    extract_element_quantity(EleQuanT, EleT),
    append([EleH], EleT, Elements),
    !.

% get_all_elements("ClOF", R).
% get_all_elements("PH5", R).

%! get_num_elements(+Formula: string, -Amount: integer) is det.
%
%  Return total number (num of type) of elements in
%  formula `Formula`
%
get_num_elements(Formula, Amount) :-
    remove_parentheses_(Formula, Formula_),
    extract_elements_from_formula(Formula_, Elements),
    length(Elements, Amount).