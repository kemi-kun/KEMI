:- module(utils,[add_dict/4,join_dict/3,extract_term/2,re_matchsub_mul/5]).



extract_term(Term, Args) :-
    Term =.. [_|Args].


%!	re_matchsub_mul(+Regex, +String:string, -Subs:list[dict], +Options:list, -Leftover:string) is det.
%
%   Like re_matchsub but returns all matches.
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


%!  add_dict(+Key, +InDict:dict, +Value:integer, -OutDict:dict) is det.
%
%   Same as python's `dct[key] += value`.
%   Creates new key amd value pair if no prior value.
%
add_dict(Key, InDict, AddValue, OutDict) :-
    get_dict(Key, InDict, Value) ->
        NewValue is Value + AddValue,
        put_dict(Key, InDict, NewValue, OutDict);
    put_dict(Key, InDict, AddValue, OutDict).


%!  join_dict(+Dict1:dict, +Dict2:dict, -OutDict:dict) is det.
%
%   Joins two dicts together by adding their values for 
%   their respective keys.
%
join_dict(Dict1, Dict2, OutDict) :-
    dict_keys(Dict2, Keys2),
    dicts_to_same_keys([Dict1, Dict2], dict_fill(0), [D0, _]),
    join_dict_(Keys2, D0, Dict2, OutDict).
join_dict_([H|T], InDict, Dict, OutDict) :-
    get_dict(H, Dict, T0),
    (
        add_dict(H, InDict, T0, D0),
        join_dict_(T, D0, Dict, OutDict),
        !;
        add_dict(H, InDict, T0, OutDict)
    ).
