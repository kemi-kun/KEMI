:- module(utils,[split_digits/2]).


%!  split_digits(+Number:int, +Numbers:list) is det.
%!  split_digits(+Number:int, -Numbers:list) is det.
%!  split_digits(-Number:int, -Numbers:list) is failure.
%!  split_digits(-Number:int, +Numbers:list) is det.
%
%   Return the number splitted into digits.
%
%   Example:
%       ?- split_digits(1234, [1, 2, 3, 4]).
%       true.
%
split_digits(Number, Numbers) :-
    nonvar(Number),
    number_chars(Number, Numbers_),
    maplist(atom_number, Numbers_, Numbers),
    !.
split_digits(Number, Numbers) :-
    nonvar(Numbers),
    maplist(atom_number, Numbers_, Numbers),
    number_chars(Number, Numbers_),
    !.
