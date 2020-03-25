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
