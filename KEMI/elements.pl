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
%   True when `Z` is the atomic number of new element `Element`
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
%   True when `Symbol` is the first character of the numerical root of `Number`
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
