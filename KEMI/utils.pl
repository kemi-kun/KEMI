:- module(utils,[
    value_is_empty_string/1,
    dict_remove_on_cond/3,
    join_pairs_by_keys/4,
    get_dict_optional/3,
    get_dict_or_default/4,
    add_dict/4,join_dict/3,
    plus_/3,
    multiply/3,
    divide/3,
    split_digits/2,
    split_decimal/3
    ]).


%!  split_digits(+Number:int, +Digits:list) is semidet.
%!  split_digits(+Number:int, -Digits:list) is det.
%!  split_digits(-Number:int, -Digits:list) is failure.
%!  split_digits(-Number:int, +Digits:list) is det.
%
%   Return the number splitted into digits.
%
%   Example:
%       ?- split_digits(1234, [1, 2, 3, 4]).
%       true.
%
split_digits(Number, Digits) :-
    nonvar(Number),
    number_chars(Number, Digits_),
    maplist(atom_number, Digits_, Digits),
    !.
split_digits(Number, Digits) :-
    nonvar(Digits),
    maplist(atom_number, Digits_, Digits),
    number_chars(Number, Digits_),
    !.

%!  split_decimal(+Number:int, +First:int, +Rest:int) is semidet.
%!  split_decimal(+Number:int, +First:int, -Rest:int) is semidet.
%!  split_decimal(+Number:int, -First:int, -Rest:int) is det.
%!  split_decimal(+Number:int, -First:int, +Rest:int) is semidet.
%!  split_decimal(-Number:int, +First:int, +Rest:int) is semidet.
%!  split_decimal(-Number:int, +First:int, -Rest:int) is ERROR.
%!  split_decimal(-Number:int, -First:int, -Rest:int) is ERROR.
%!  split_decimal(-Number:int, -First:int, +Rest:int) is ERROR.
%
%   Split the number into two parts.
%
%   Example:
%       ?- split_decimal(1234, 1000, 234).
%       true.
%       ?- split_decimal(31, 30, 1).
%       true.
%       ?- split_decimal(600, 600, 0).
%       true.
%
split_decimal(Number, First, Rest) :-
    nonvar(Number), % deals with + X X
    split_decimal_(Number, First, Rest),
    !.
split_decimal(Number, First, Rest) :-
    var(Number),    % deals with - X X
    plus(First, Rest, Number),
    split_decimal_(Number, First, Rest).
split_decimal_(Number, First, Rest) :-
    split_digits(Number, [First_|Rest_]),
    length(Rest_, L),
    First is First_ * 10^L,
    split_digits(Rest, Rest_).


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

%!  multiply(+A, +B, -C) is det.
%!  multiply(+A, -B, +C) is det.
%!  multiply(-A, +B, +C) is det.
%!  multiply(+A, +B, +C) is semidet.
%
%   C = A * B
%
multiply(A, B, C) :-
    nonvar(A), nonvar(B),
    C is A * B,
    !.
multiply(A, B, C) :-
    nonvar(A), nonvar(C),
    B is C / A,
    !.
multiply(A, B, C) :-
    nonvar(B), nonvar(C),
    A is C / B.

% multiply(A, B, C) :-
%     B > 1 -> plus(B, -1, Y), multiply(A, Y, X), plus(X, A, C);
%     B = 1 -> C = A;
%     B < 0 -> Y is -B, multiply(A, Y, C).

divide(A, B, C) :-
    nonvar(A), nonvar(B),
    C is A / B,
    !.
divide(A, B, C) :-
    nonvar(A), nonvar(C),
    B is A / C,
    !.
divide(A, B, C) :-
    nonvar(B), nonvar(C),
    A is B * C.

plus_(Num1, Num2, Result) :-
    Result is Num1 + Num2.

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
