:- module(utils,[split_digits/2,split_decimal/2]).


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
