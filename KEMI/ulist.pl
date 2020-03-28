:- module(ulist,[enumerate/2]).


%!  reversed(+In: list, -Out: list) is det.
%  
%   Reverse list `In`and return the result list as `Out`.
%   Return false if `In` is not list
%
% reversed(In, Out) :-
%     reverse(In, Out),
%     !.

%! append_element(+List: list, +Element: atom, -Result: list) is det.
%! append_element(+List: list, -Element: atom, +Result: list) is det.
%
%  Append `Element` to a list `List`
%
% append_element(List, Element, Result) :-
%     append(List, [Element], Result).


%!  enumerate(+List: list, -Pairs: list(Key-Value)) is det.
%!  enumerate(-List: list, +Pairs: list(Key-Value)) is det.
%  
%   True when =Pairs= is a _Pairs_ with index as key and element as value.
%
enumerate(List, Pairs) :-
    (nonvar(List); nonvar(Pairs)),
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
%   BUG: can't do negative steps
%
range(Start, Stop, Step, Range) :-
    range_(Stop, Step, [Start|T]),
    append([Start], T, Range).
range_(Stop, Step, [H|T]) :-
    nonvar(Step),
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
