/* 

Implements:
- IR-3 Elements
- Table VI
- Group
- Period
*/
:- module(elements,[period/2,new_element_name_atomic_number/2,new_element_symbol_atomic_number/2,new_element/4,group/2,element_name/2,element_symbol/2]).

:- use_module(ustr,[capitalize/2,join/3]).
:- use_module(unums,[split_digits/2,num_digits/2]).
:- use_module(facts).
:- use_module(library(arithmetic)).


%!  element_name(+Element:atom, +ElementName:string) is semidet.
%!  element_name(+Element:atom, -ElementName:string) is multi.
%!  element_name(-Element:atom, -ElementName:string) is multi.
%!  element_name(-Element:atom, +ElementName:string) is multi.
%
%   True when `ElementName` is an acceptable IUPAC name for chemical element
%   `Element`.
%
%   Generates element facts with the alternative name right after the 
%   element, if any, when both Element and ElementName is unbound.
%
element_name(Element, ElementName) :-
    var(Element), var(ElementName) -> (
        element_fact(Element, _, _, _, _),
        (
            alternative_element_name(Element, ElementName) ->
                element_fact(Element, ElementName, _, _, _),
                alternative_element_name(Element, ElementName);
            element_fact(Element, ElementName, _, _, _)
        )
    );
    nonvar(Element), alternative_element_name(Element, _) -> (
        element_fact(Element, ElementName, _, _, _);
        alternative_element_name(Element, ElementName), !
    );
    nonvar(ElementName),
    alternative_element_name(Element, ElementName) -> true;

    alternative_element_name(Element, ElementName);
    element_fact(Element, ElementName, _, _, _), !;
    new_element(Element, ElementName, _, _).


%!  element_symbol(+Element:atom, +Symbol:string) is semidet.
%!  element_symbol(+Element:atom, -Symbol:string) is semidet.
%!  element_symbol(-Element:atom, -Symbol:string) is multi.
%!  element_symbol(-Element:atom, +Symbol:string) is semidet.
%
%   True when element `Element` has symbol `Symbol`.
%   Generates element facts when both `Element` and `Symbol` is unbound.
%
element_symbol(Element, Symbol) :-
    var(Element), var(Symbol) ->
        element_fact(Element, _, Symbol, _, _);
    element_fact(Element, _, Symbol, _, _), !;
    new_element(Element, _, Symbol, _).


%!  atomic_number(+Element:atom, +AtomicNumber:string) is semidet.
%!  atomic_number(+Element:atom, -AtomicNumber:string) is multi.
%!  atomic_number(-Element:atom, -AtomicNumber:string) is multi.
%!  atomic_number(-Element:atom, +AtomicNumber:string) is semidet.
%
%   True when element `Element` has `AtomicNumber` protons.
%   Generates element facts when both `Element` and `AtomicNumber` is unbound.
%
atomic_number(Element, AtomicNumber) :-
    var(Element), var(AtomicNumber) ->
        element_fact(Element, _, _, AtomicNumber, _);
    element_fact(Element, _, _, AtomicNumber, _), !;
    new_element(Element, _, _, AtomicNumber).


%!  new_element(?Element:atom, ?Name:string, ?Symbol:string, ?NumProtons:int) is det/multi.
%
%   True when "new element" `Element` has systematic element name `Name`,
%   symbol `Symbol` and `NumProtons` protons.
%
new_element(Element, ElementName, Symbol, NumProtons) :-
    (nonvar(Element); nonvar(ElementName)) ->
        atom_string(Element, ElementName),
        new_element_name_atomic_number(ElementName, NumProtons),
        new_element_symbol_atomic_number(Symbol, NumProtons);
    new_element_symbol_atomic_number(Symbol, NumProtons),
    new_element_name_atomic_number(ElementName, NumProtons),
    atom_string(Element, ElementName).


%!  new_element_name_atomic_number(+ElementName:string, +AtomicNumber:int) is semidet.
%!  new_element_name_atomic_number(+ElementName:string, -AtomicNumber:int) is semidet.
%!  new_element_name_atomic_number(-ElementName:string, -AtomicNumber:int) is multi.
%!  new_element_name_atomic_number(-ElementName:string, +AtomicNumber:int) is det.
%
%   True when “new element” with systematic element name `ElementName` has
%   `AtomicNumber` protons.
%
new_element_name_atomic_number(ElementName, AtomicNumber) :-
    nonvar(ElementName), var(AtomicNumber) ->
        new_element_name_atomic_number_(ElementName, AtomicNumber);
    new_element_atomic_number_name_(AtomicNumber, ElementName).

%!  new_element_atomic_number_name_(+Z:int, -ElementName:string) is semidet.
new_element_atomic_number_name_(Z, ElementName) :-
    between(1, infinite, Z),
    split_digits(Z, Digits),
    maplist(numerical_root_fact, Digits, Roots),
    join("", Roots, Root),
    (
        sub_string(Root, _, 1, 0, "i") ->
            string_concat(Root, um, ElementName);
        string_concat(Root, ium, ElementName)
    ).

%!  new_element_name_atomic_number_(+ElementName:string, -Z:int) is semidet.
new_element_name_atomic_number_(ElementName, Z) :-
    string_length(ElementName, L),
    Min is round((L - 3) / 4),
    Max is round(L / 3),
    % MaxL         7, 11, 15, ...
    % Digits of Z: 1,  2,  3, ...
    % MinL         4,  6,  9, ...
    between(Min, Max, N),
    num_digits(N, Z),
    new_element_atomic_number_name_(Z, ElementName), !.


%!  new_element_symbol_atomic_number(+Symbol:atom, +AtomicNumber:int) is semidet.
%!  new_element_symbol_atomic_number(+Symbol:atom, -AtomicNumber:int) is semidet.
%!  new_element_symbol_atomic_number(-Symbol:atom, -AtomicNumber:int) is multi.    % infinite
%!  new_element_symbol_atomic_number(-Symbol:atom, +AtomicNumber:int) is det.
%
%   True when “new element” with symbol `ElementName` has `AtomicNumber` protons.
%
new_element_symbol_atomic_number(Symbol, AtomicNumber) :-
    nonvar(Symbol), var(AtomicNumber) ->
        string_length(Symbol, L),
        num_digits(L, AtomicNumber),
        new_element_symbol_atomic_number_(Symbol, AtomicNumber), !;
    new_element_symbol_atomic_number_(Symbol, AtomicNumber).

new_element_symbol_atomic_number_(Symbol, Z) :-
    between(1, infinite, Z),
    split_digits(Z, Digits),
    maplist(numerical_root_fact, Digits, Roots),
    maplist(get_first_char_, Roots, Chars),
    join("", Chars, Symbol_),
    capitalize(Symbol_, Symbol).

%!  get_first_char_(+String:string, -Char:string) is det.
get_first_char_(String, Char) :-
    sub_string(String, 0, 1, _, Char).


%!  numerical_root_symbol(+Number:int, +Symbol:atom) is semidet.
%!  numerical_root_symbol(+Number:int, -Symbol:atom) is det.
%!  numerical_root_symbol(-Number:int, -Symbol:atom) is multi.
%!  numerical_root_symbol(-Number:int, +Symbol:atom) is det.
%
%   True when `Symbol` is the first character of the numerical root of `Number`.
%
numerical_root_symbol(Number, Symbol) :-
    (var(Number), var(Symbol)) ->
        numerical_root_fact(Number, Root),
        string_chars(Root, [Symbol|_]);
    numerical_root_fact(Number, Root),
    string_chars(Root, [Symbol|_]),
    !.


%!  period(+Element:atom, -Period:int) is det.
%!  period(-Element:atom, +Period:int) is multi.
%!  period(-Element:atom, -Period:int) is multi.
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

%! period2(+Element:atom, +Period:int) is semidet.
%! period2(+Element:atom, -Period:int) is semidet.
%! period2(-Element:atom, -Period:int) is multi.
%! period2(-Element:atom, +Period:int) is semidet.
%
%  True if element Element is in period Period.
%
period2(Element, Period) :-
    nonvar(Element), var(Period) ->
        period2_(Element, Period), !;
    period2_(Element, Period).

period2_(Element, Period) :-
    element_fact(Element, _, _, Z, _),
    List = [0, 2, 10, 18, 36, 54, 86, 118],
        nth0(Period, List, Proton),
        nth1(Period, List, Proton0),
        Z > Proton0,
        Z =< Proton.

%!  group(+Element:atom, +Group:int) is semidet.
%!  group(+Element:atom, -Group:int) is semidet.
%!  group(-Element:atom, -Group:int) is multi.
%!  group(-Element:atom, +Group:int) is multi.
%
%   True if chemical element `Element` is in group `Group`.
%
group(Element, Group) :-
    element_fact(Element, _, _, _, _),
    group_(Element, Group_),
    Group = Group_.

%!  group(+Element:atom, -Group:int) is semidet.
group_(Element, Group) :-
    element_fact(Element, _, _, Proton, _),
    member(NProton, [2, 10, 18, 36, 54, 86, 118]),
    T is min(NProton+3, 118),   % for efficiency
    between(1, T, Proton),      % for efficiency
    (
        group_exception(Element, Group) ->
            true;

        Proton is NProton,
        Group is 18 ->
            true;

        between(1, 3, Group),
        Proton is NProton + Group ->
            true;

        between(4, 17, Group),
        Proton is NProton -18 + Group ->
            true
    ), !.

group_exception(hydrogen, 1).
group_exception(boron, 13).
group_exception(aluminium, 13).
