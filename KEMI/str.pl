:- module(str,[capitalize/2,contains/2,remove/3,replace/4]).

contains(String, Substring) :-
    sub_string(String, _,_,_, Substring), !.

remove(String, Removed, Result) :-
    replace(String, Removed, "", Result), !.

%!  replace(String: string, S1: string, S2: string, -Result: string) is nondet
%
%   Find the first occurence of substring `S1` and replace it with `S2`.
%
%   Fails if `S1` is not in `String`.
%
replace(String, S1, S2, Result) :-
    sub_string(String, BS, _, AS, S1),
    sub_string(String, 0, BS, _, Before),
    sub_string(String, _, AS, 0, After),
    string_concat(Before, S2, T0),
    string_concat(T0, After, Result).

%!	capitalize(String: string, -Result: string) is det
%
%   Capitalize the first letter in `String`.
%
capitalize(String, Result) :-
    sub_string(String, 0, 1, _, First),
    sub_string(String, 1, _, 0, Sub),
    string_upper(First, FirstUpper),
    string_concat(FirstUpper, Sub, Result).
