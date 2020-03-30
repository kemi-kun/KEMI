:- module(ustr,[
    re_matchsub_mul/5,
    re_match_count/3,
    re_finditer/4,
    re_findall/4,
    join/3,
    remove_chars/3,
    split/2,
    capitalize/2,
    replace/4
    ]).
    

% contains(String, Substring) :-
%     sub_string(String, _,_,_, Substring), !.

%!  replace(+String: string, +S1:string, +S2:string, +Result:string) is multi.  # TODO: Remove choice points
%!  replace(+String: string, +S1:string, +S2:string, -Result:string) is multi.  # TODO: Remove choice points
%!  replace(+String: string, +S1:string, -S2:string, +Result:string) is ERROR.  # TODO: Fix
%!  replace(+String: string, -S1:string, +S2:string, +Result:string) is multi.  # TODO: REmove chocie points
%!  replace(-String: string, +S1:string, +S2:string, +Result:string) is ERROR.  # TODO: Fix
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
    string_concat(T0, After, Result),
    !.

%!	capitalize(+String:string, +Result:string) is failure.
%!	capitalize(+String:string, -Result:string) is det.
%!	capitalize(-String:string, +Result:string) is det.
%!	capitalize(-String:string, -Result:string) is failure.
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


%!  split(+In:string, -Out:list) is det.
%!  split(-In:string, +Out:list) is ERROR.
%
%   Split string `In` to list
%   and return the result list as `Out`.
%
split(In, Out) :-
    string_codes(In, Code),
    maplist(number_to_character_, Code, Out).

number_to_character_(Number, Character) :-
    string_codes(Character,[Number]).


%! remove_chars(+String:string, +CharsToRemove:string, +Result:string) is semidet.
%! remove_chars(+String:string, +CharsToRemove:string, -Result:string) is det.
%
%  Remove "(", ")", "[", "]", "{" and "}" from `String`.
%
% remove_chars("NaCl", "NaCl").
% remove_chars("[Al(POCl3)6]3+", "AlPOCl363+").
% remove_chars("H2[PtCl6]", "H2PtCl6").
% remove_chars("ab[(c)][d2](3)", "abcd23").
remove_chars(String, CharsToRemove, Result) :-
    split(String, Strings),
    exclude(is_(CharsToRemove), Strings, Result_),
    atomics_to_string(Result_, Result).
is_(String, Elem) :- split(String, X), member(Elem, X).


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


%!	re_matchsub_mul(+Regex, +String:string, -Subs:list[dict], +Options:list, -Leftover:string) is det.
%
%   Like re_matchsub but returns all matches.
%
%   Note: Use re_finditer/3 if leftover is not needed.
%
re_matchsub_mul(Regex, String, Subs, Options, Leftover) :-
    re_matchsub(Regex, String, Sub1, Options),

    % get UnMatched string
    get_dict(0, Sub1, FullMatch),
    sub_string(String, Before, _, After, FullMatch),
    sub_string(String, 0, Before, _, BeforePart),
    sub_string(String, _, After, 0, AfterPart),
    string_concat(BeforePart, AfterPart, Unmatched),
    
    re_matchsub_mul(Regex, Unmatched, Subs0, Options, Leftover),
    append(Subs0, [Sub1], Subs),
    !.
re_matchsub_mul(Regex, String, Subs, Options, Leftover) :-
    not(re_matchsub(Regex, String, _, Options)),
    Subs = [],
    Leftover = String.


%!  re_findall(+Regex, +String:string, -Matches:list(string), +Options:list) is det.
%
%   Attemps to be python's `re.findall()`.
%
%   Reference: https://docs.python.org/3/library/re.html#re.findall
%
re_findall(Regex, String, Matches, Options) :-
    re_foldl(append_matchstring_, Regex, String, [], Matches, Options).

append_matchstring_(Match, V0, V1) :-
    get_dict(0, Match, X),
    append(V0, [X], V1).


%!  re_finditer(+Regex, +String:string, -Matches:list(re_match), +Options:list) is det.
%
%   Attemps to be python's `re.finditer()`.
%
%   Reference: https://docs.python.org/3/library/re.html#re.finditer
%
re_finditer(Regex, String, Matches, Options) :-
    re_foldl(append_match_, Regex, String, [], Matches, Options).

append_match_(Match, V0, V1) :-
    append(V0, [Match], V1).


%!  re_match_count(+Regex, +String:string, -Count:int) is det.
%
%   Counts all matches in a string.
%
%   Taken from https://www.swi-prolog.org/pldoc/doc_for?object=re_foldl/6.
%
re_match_count(Regex, String, Count) :-
    re_foldl(increment_, Regex, String, 0, Count, []).

increment_(_Match, V0, V1) :-
    V1 is V0+1.
