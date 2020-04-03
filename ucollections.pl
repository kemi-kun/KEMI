:- module(ucollections,[
    % add_dict/4,join_dict/3,
    value_is_empty_string/1,
    dict_remove_on_cond/3,
    join_pairs_by_keys/4,
    get_dict_optional/3,
    get_dict_or_default/4,
    range/4,
    enumerate/2
    ]).


%!  enumerate(+List: list, -Pairs: list(Key-Value)) is det.
%!  enumerate(-List: list, +Pairs: list(Key-Value)) is det.
%  
%   True when =Pairs= is a _Pairs_ with index as key and element as value.
%
enumerate(List, Pairs) :-
    (nonvar(List); nonvar(Pairs)),
    pairs_keys_values(Pairs, Keys, List),
    enumerate_(List, Keys).
enumerate_([_H1], [H2]) :-
    H2 is 0,
    !.
enumerate_([_H1|T1], [H2|T2]) :-
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


%!  get_dict_optional(+Keys:list, +Dict, -Value) is det.
%!  get_dict_optional(+Keys:list, +Dict, +Value) is semidet.
%
%   Try each key and return the first value that is true.
%
get_dict_optional(Keys, Dict, Value) :-
    get_dict_optional_(Keys, Dict, Value_),
    Value_ = Value.

get_dict_optional_(Keys, Dict, Value) :-
    Keys \= [],
    get_dict_optional__(Keys, Dict, Value).

get_dict_optional__([H|T], Dict, Value) :-
    get_dict(H, Dict, Value) -> true;
        get_dict_optional_(T, Dict, Value).


%!  get_dict_or_default(+Default, +Keys, +Dict, -Value) is det.
%!  get_dict_or_default(+Default, +Keys, +Dict, +Value) is semidet.
%
%   True when `Key`-`Value` pair is in `Dict`
%   OR when `Key` is not in `Dict` and `Value` = `Default`. 
% 
get_dict_or_default(Default, Keys, Dict, Value) :-
    get_dict(Keys, Dict, Value) -> true;
        Value = Default.


dict_remove_on_cond(Condition, Dict, TrimmedDict) :-
    dict_pairs(Dict, Tag, Pairs),
    exclude(Condition, Pairs, TrimmedPairs),
    dict_pairs(TrimmedDict, Tag, TrimmedPairs).

value_is_empty_string(KeyValue) :-
    KeyValue = _Key-Value,
    Value = "".


% %!  add_dict(+Key, +InDict:dict, +Value:integer, -OutDict:dict) is det.
% %
% %   Same as python's `dct[key] += value`.
% %   Creates new key amd value pair if no prior value.
% %
% add_dict(Key, InDict, AddValue, OutDict) :-
%     get_dict(Key, InDict, Value) ->
%         NewValue is Value + AddValue,
%         put_dict(Key, InDict, NewValue, OutDict);
%     put_dict(Key, InDict, AddValue, OutDict).


% %!  join_dict(+Dict1:dict, +Dict2:dict, -OutDict:dict) is det.
% %
% %   Joins two dicts together by adding their values for 
% %   their respective keys.
% %
% join_dict(Dict1, Dict2, OutDict) :-
%     dict_keys(Dict2, Keys2),
%     dicts_to_same_keys([Dict1, Dict2], dict_fill(0), [D0, _]),
%     join_dict_(Keys2, D0, Dict2, OutDict).
% join_dict_([H|T], InDict, Dict, OutDict) :-
%     get_dict(H, Dict, T0),
%     (
%         add_dict(H, InDict, T0, D0),
%         join_dict_(T, D0, Dict, OutDict),
%         !;
%         add_dict(H, InDict, T0, OutDict)
%     ).


%!	join_pairs_by_keys(:Function, +P1, +P2, -Joined:list(Key-Value)) is det.
%
%   Join two pairs together using `Function` to collapse values that have the 
%   same key. `Function` should be in the form Function(+A:T, +B:T, -C:T).
%
join_pairs_by_keys(Function, P1, P2, Joined) :-
    append(P1, P2, Pairs_),
    sort(1, @=<, Pairs_, Pairs),
    group_pairs_by_key(Pairs, Joined_),
    pairs_keys_values(Joined_, Keys, Values_),
    maplist(collapse_(Function), Values_, Values),
    pairs_keys_values(Joined, Keys, Values).

collapse_(Function, [V0|L], Value) :-
    foldl(Function, L, V0, Value).
