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
 
%!  split(+In: String, -Out: list) is det.
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

%! split_decimal(+Number, -Numbers) is det.
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

%! get_element(+Formula: string, +Index: integer, -Element: string)
%
%  Get element at `Index` position in formula `Formula`.
%  (index starts at 0)
%  Return false if there's no element at `Index`
%
get_element(Formula, Index, Element) :-
    extract_elements_from_formula(Formula, Elements),
    nth0(Index, Elements, ElementQuantity),
    extract_term(ElementQuantity, [Element|_]).