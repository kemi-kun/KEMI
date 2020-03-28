:- module(strutils,[join/3,split/2,capitalize/2,contains/2,remove/3,replace/4]).


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

%!	capitalize(+String: string, +Result: string) is failure.
%!	capitalize(+String: string, -Result: string) is det.
%!	capitalize(-String: string, +Result: string) is det.
%!	capitalize(-String: string, -Result: string) is failure.
%
%   True when first letter in `String` is upper and the rest is lower,
%   and all leters in `Result` is lower.
%
capitalize(String, Result) :-
    nonvar(String),
    sub_string(String, 0, 1, _, First),
    sub_string(String, 1, _, 0, Sub),
    string_upper(First, FirstUpper),    % one-way
    string_lower(Sub, Sub),
    string_concat(FirstUpper, Sub, Result),
    !.
capitalize(String, Result) :-
    nonvar(Result),
    sub_string(Result, 0, 1, _, First),
    sub_string(Result, 1, _, 0, Sub),
    string_lower(First, FirstLower),    % one-way
    string_lower(Sub, Sub),
    string_concat(FirstLower, Sub, String),
    !.


%!  split(+In: string, -Out: list) is det.
%!  split(-In: string, +Out: list) is ERROR.
%
%   Split string `In` to list
%   and return the result list as `Out`.
%
split(In, Out) :-
    string_codes(In, Code),
    maplist(number_to_character_, Code, Out).

number_to_character_(Number, Character) :-
    string_codes(Character,[Number]).

%!  join(+Sep:string, +Subs:list(string), +String:string) is semidet.
%!  join(+Sep:string, +Subs:list(string), -String:string) is det.
%!  join(+Sep:string, -Subs:list(string), -String:string) is failure.
%!  join(+Sep:string, -Subs:list(string), +String:string) is det.
%
join(Sep, Subs, String) :-
    nonvar(Sep), nonvar(Subs),
    join_(Sep, Subs, String),
    !.
join(Sep, Subs, String) :-
    nonvar(Sep), nonvar(String),
    split_string(String, Sep, "", Subs),
    !.
join_(Sep, [H|T], String) :-
    nonvar(Sep), nonvar(H),
    T = [] ->
        String = H;
    join_(Sep, T, S0),
    string_concat(H, Sep, T0),
    string_concat(T0, S0, String),
    !.
