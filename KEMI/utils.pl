:- module(utils,[split_decimal/2]).


%! split_decimal(+Number: integer, -Numbers: list) is det.
% 
%  Return the number splitted into digits.
%  
%  split_decimal(1234, [1, 2, 3, 4]).
split_decimal(Number, Numbers) :-
    number_chars(Number, Numbers_),
    maplist(atom_number, Numbers_, Numbers).
