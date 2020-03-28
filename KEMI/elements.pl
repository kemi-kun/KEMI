/* 

Implements:
- IR-3 Elements
- Table VI
- Group
- Period
*/
:- module(elements,[element_name/2]).

:- use_module(strutils,[capitalize/2]).
:- use_module(utils,[split_decimal/2]).
:- use_module(facts,[
    element_fact/5,
    alternative_element_name_fact/2,
    numerical_root_fact/2
    ]).


element_name(Element, ElementName) :-
    element_fact(Element, ElementName, _, _, _) -> true;
    new_element_name(Element, ElementName);
    alternative_element_name_fact(Element, ElementName).

atomic_number(Element, Z) :-
    element_fact(Element, _, _, Z, _) -> true;
    new_element_atomic_number(Element, Z).


%!  new_element_atomic_number(+Element:string, +Z:integer) is det.
%!  new_element_atomic_number(+Element:string, -Z:integer) is det.
%!  new_element_atomic_number(-Element:string, -Z:integer) is failure.
%!  new_element_atomic_number(-Element:string, +Z:integer) is det.
%
%   True when `Z` is the atomic number of "new" element `Element`.
%
new_element_atomic_number(Element, Z) :-
    nonvar(Z),
    split_decimal(Z, L0),
    maplist(numerical_root_symbol, L0, L1),
    string_chars(T0, L1),
    capitalize(T0, Element),
    !.
new_element_atomic_number(Element, Z) :-
    nonvar(Element),
    capitalize(T0, Element),
    string_chars(T0, L1),
    maplist(numerical_root_symbol, L0, L1),
    split_decimal(Z, L0),
    !.

%!  numerical_root_symbol(+Number:integer, -Symbol:atom) is det.
%!  numerical_root_symbol(-Number:integer, +Symbol:atom) is det.
%!  numerical_root_symbol(+Number:integer, +Symbol:atom) is det.
%
%   True when `Symbol` is the first character of the numerical root of `Number`.
%
numerical_root_symbol(Number, Symbol) :-
    var(Number), var(Symbol),
    numerical_root_fact(Number, Root),
    string_chars(Root, [Symbol|_]).
numerical_root_symbol(Number, Symbol) :-
    numerical_root_fact(Number, Root),
    string_chars(Root, [Symbol|_]),
    !.


%!  period(+Element:string, -Period:integer) is det.
%!  period(-Element:string, +Period:integer) is multi.
%!  period(-Element:string, -Period:integer) is multi.
%
%   True if element Element is in period Period.
%
period(Element, Period) :-
    element_fact(Element, _, _, Z, _),
    (
        Z =<   2 -> Period is 1;
        Z =<  10 -> Period is 2;
        Z =<  18 -> Period is 3;
        Z =<  36 -> Period is 4;
        Z =<  54 -> Period is 5;
        Z =<  86 -> Period is 6;
        Z =< 118 -> Period is 7;
        Period is 8
    ).

%! period2(+Element:string, -Period:integer) is multi.
%! period2(-Element:string, +Period:integer) is multi.
%! period2(-Element:string, -Period:integer) is multi.
%
%  True if element Element is in period Period.
%
period2(Element, Period) :-
    element_fact(Element, _, _, Z, _),
    List = [0, 2, 10, 18, 36, 54, 86, 118],
        nth0(Period, List, Proton),
        nth1(Period, List, Proton0),
        Z > Proton0,
        Z =< Proton.

% Group(Element, Number) ← {
% 	Group(Noble, 18) ∧ 
% 	AtomicNumber(Element, Proton) ∧
% 	AtomicNumber(Noble, NProton) ∧ [ (
% 			Proton = NProton + Number ∧
% 			1 ≤ Number ≤ 3
% 		) ∨ (
% 			Proton = NProton - 18 + Number ∧
% 			4 ≤ Number ≤ 17
% 		) ∨ (
% 			Element = "H" ∧ Number = 1
% 		)
% 	]
% } ∨ {
% 	AtomicNumber(Element, Proton) ∧
% 	FullOrbital(Proton) ∧
% 	Number = 18
% }
 
% FullOrbital(Proton) ← {
% 	Proton in {2, 10, 18, 36, 54, 86, 118}
% }

% group(Element, Group) :-
%     Element = hydrogen, 
%     Group = 1,
%     !.
% group(Element, Group) :-
%     nonvar(Element),
%     element_fact(Element, _, _, Z, _),
%     full_orbital(Z),
%     Group is 18,
%     !.
% group(Element, Group) :-
%     (var(Group);nonvar(Group), Group \= 18),
%     atomic_number(Element, Z),
%     group(Noble, 18),
%     write(Noble),
%     atomic_number(Noble, NZ),
%     write(NZ),
%     ((
%         plus(NZ, X, Z),
%         between(1, X, 3),
%         Group is X
%     );(
%         plus(NZ, -18, T0),
%         plus(T0, Y, Z),
%         between(4, Y, 17),
%         Group is Y
%     )).
% group(Element, Group) :-
%     nonvar(Group),
%     Group = 18,
%     element_fact(Element, _, _, Z, _),
%     full_orbital(Z).


full_orbital(NumProtons) :-
    member(NumProtons, [2, 10, 18, 36, 54, 86, 118]),
    !.

%! group(+Element:string, +Group:integer) is det.
%! group(+Element:string, -Group:integer) is det.
%! group(-Element:string, +Group:integer) is multi.
%! group(-Element:string, -Group:integer) is WRONG. # TODO: Fix
%
%  True if element is group 18.
%
group(Element, Group) :-
    nonvar(Element) ->
        Element = hydrogen,
        Group is 1,
        !;
    Element = hydrogen,
    Group is 1.
group(Element, Group) :-
    var(Element),
    (nonvar(Group) -> Group \= 18; true),   % skip this if Group is given = 18
    element_fact(Element, _, _, Z, _),
    not(full_orbital(Z)),                   % skip if is group 18
    group(NobleElement, 18),
    element_fact(NobleElement, _, _, Z2, _),
    T1 is Z2 + 1,
    T2 is Z2 + 2,
    T3 is Z2 + 3,
    T17 is Z2 - 1,
    T16 is Z2 - 2,
    T15 is Z2 - 3,
    T14 is Z2 - 4,
    T13 is Z2 - 5,
    T12 is Z2 - 6,
    T11 is Z2 - 7,
    T10 is Z2 - 8,
    T9 is Z2 - 9,
    T8 is Z2 - 10,
    T7 is Z2 - 11,
    T6 is Z2 - 12,
    T5 is Z2 - 13,
    T4 is Z2 - 14,
    (  
        Z = T1 -> Group is 1;
        Z = T2 -> Group is 2;
        Z = T3 -> Group is 3;
        Z = T17 -> Group is 17;
        Z = T16 -> Group is 16;
        Z = T15 -> Group is 15;
        Z = T14 -> Group is 14;
        Z = T13-> Group is 13;
        Z = T12 -> Group is 12;
        Z = T11 -> Group is 11;
        Z = T10 -> Group is 10;
        Z = T9 -> Group is 9;
        Z = T8 -> Group is 8;
        Z = T7 -> Group is 7;
        Z = T6 -> Group is 6;
        Z = T5 -> Group is 5;
        Z = T4 -> Group is 4      
    ).
group(Element, Group) :-
    nonvar(Element),
    (nonvar(Group) -> Group \= 18; true),   % skip this if Group is given = 18
    element_fact(Element, _, _, Z, _),
    not(full_orbital(Z)),                   % skip if is group 18
    group(NobleElement, 18),
    element_fact(NobleElement, _, _, Z2, _),
    T1 is Z2 + 1,
    T2 is Z2 + 2,
    T3 is Z2 + 3,
    T17 is Z2 - 1,
    T16 is Z2 - 2,
    T15 is Z2 - 3,
    T14 is Z2 - 4,
    T13 is Z2 - 5,
    T12 is Z2 - 6,
    T11 is Z2 - 7,
    T10 is Z2 - 8,
    T9 is Z2 - 9,
    T8 is Z2 - 10,
    T7 is Z2 - 11,
    T6 is Z2 - 12,
    T5 is Z2 - 13,
    T4 is Z2 - 14,
    (  
        Z = T1 -> Group is 1;
        Z = T2 -> Group is 2;
        Z = T3 -> Group is 3;
        Z = T17 -> Group is 17;
        Z = T16 -> Group is 16;
        Z = T15 -> Group is 15;
        Z = T14 -> Group is 14;
        Z = T13-> Group is 13;
        Z = T12 -> Group is 12;
        Z = T11 -> Group is 11;
        Z = T10 -> Group is 10;
        Z = T9 -> Group is 9;
        Z = T8 -> Group is 8;
        Z = T7 -> Group is 7;
        Z = T6 -> Group is 6;
        Z = T5 -> Group is 5;
        Z = T4 -> Group is 4      
    ),
    !.
group(Element, Group) :-
    element_fact(Element, _, _, Z, _),
    full_orbital(Z),
    Group is 18.
