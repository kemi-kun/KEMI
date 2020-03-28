:- module(utils,[split_decimal/2]).


%!  split_decimal(+Number:int, +Numbers:list) is det.
%!  split_decimal(+Number:int, -Numbers:list) is det.
%!  split_decimal(-Number:int, -Numbers:list) is failure.
%!  split_decimal(-Number:int, +Numbers:list) is det.
%
%   Return the number splitted into digits.
%
%   Example:
%       ?- split_decimal(1234, [1, 2, 3, 4]).
%       true.
%
split_decimal(Number, Numbers) :-
    nonvar(Number),
    number_chars(Number, Numbers_),
    maplist(atom_number, Numbers_, Numbers),
    !.
split_decimal(Number, Numbers) :-
    nonvar(Numbers),
    maplist(atom_number, Numbers_, Numbers),
    number_chars(Number, Numbers_),
    !.
