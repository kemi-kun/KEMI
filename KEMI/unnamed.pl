/** <module> Unnamed Elements

Implements rule IR-3.1.1 in IUPAC Red Book (pg. 47).
*/
:- use_module(str, [replace/4, capitalize/2]).

%!	name(+NumProtons:integer, -Name: string) is det.
%
%   True if Name is the IUPAC temporary name for unnamed element with
%   NumProtons protons.
%
%   @arg NumProtons The number of protons of the element.
%   @arg Name The IUPAC temporary name of the element.
%   @bug Doesn't replace more than one occurance (e.g. 9090)
%   @tbd Causes error if NumProton is queried instead.
%
name(NumProtons, Name) :-
    digit_symbol__name_(NumProtons, N1),
    string_concat(N1, "ium", T0),
    name_check_i_(T0, T1),
    name_check_n_(T1, Name),
    !.

%!	symbol(+NumProtons:integer, -Symbol: string) is det.
%
%   True if Symbol is the IUPAC temporary symbol for unnamed element
%   with NumProtons protons.
%
%   @arg NumProtons The number of protons of the element.
%   @arg Name The IUPAC temporary name of the element.
%   @tbd Causes error if NumProton is queried instead.
%
symbol(NumProtons, Symbol) :-
    digit_symbol_(NumProtons, T0),
    str:capitalize(T0, Symbol),
    !.

digit_symbol__name_(NumProtons, Name) :-
    NumProtons < 10,
    NumProtons = 0,
    Name = "nil".
digit_symbol__name_(NumProtons, Name) :-
    NumProtons < 10,
    NumProtons = 1,
    Name = "un".
digit_symbol__name_(NumProtons, Name) :-
    NumProtons < 10,
    NumProtons = 2,
    Name = "bi".
digit_symbol__name_(NumProtons, Name) :-
    NumProtons < 10,
    NumProtons = 3,
    Name = "tri".
digit_symbol__name_(NumProtons, Name) :-
    NumProtons < 10,
    NumProtons = 4,
    Name = "quad".
digit_symbol__name_(NumProtons, Name) :-
    NumProtons < 10,
    NumProtons = 5,
    Name = "pent".
digit_symbol__name_(NumProtons, Name) :-
    NumProtons < 10,
    NumProtons = 6,
    Name = "hex".
digit_symbol__name_(NumProtons, Name) :-
    NumProtons < 10,
    NumProtons = 7,
    Name = "sept".
digit_symbol__name_(NumProtons, Name) :-
    NumProtons < 10,
    NumProtons = 8,
    Name = "oct".
digit_symbol__name_(NumProtons, Name) :-
    NumProtons < 10,
    NumProtons = 9,
    Name = "enn".
digit_symbol__name_(NumProtons, Name) :-
    NumProtons >= 10,
    T0 is NumProtons mod 10,
    T1 is NumProtons // 10,
    digit_symbol__name_(T0, N0),
    digit_symbol__name_(T1, N1),
    string_concat(N1, N0, Name).

name_check_i_(Name, Result) :-
    str:replace(Name, "ii", "i", Result),
    !.
name_check_i_(Name, Result) :-
    Result = Name.

name_check_n_(Name, Result) :-
    str:replace(Name, "nnn", "nn", Result),
    !.
name_check_n_(Name, Result) :-
    Result = Name.


digit_symbol_(NumProtons, Symbol) :-
    NumProtons < 10,
    NumProtons = 0,
    Symbol = "n".
digit_symbol_(NumProtons, Symbol) :-
    NumProtons < 10,
    NumProtons = 1,
    Symbol = "u".
digit_symbol_(NumProtons, Symbol) :-
    NumProtons < 10,
    NumProtons = 2,
    Symbol = "b".
digit_symbol_(NumProtons, Symbol) :-
    NumProtons < 10,
    NumProtons = 3,
    Symbol = "t".
digit_symbol_(NumProtons, Symbol) :-
    NumProtons < 10,
    NumProtons = 4,
    Symbol = "q".
digit_symbol_(NumProtons, Symbol) :-
    NumProtons < 10,
    NumProtons = 5,
    Symbol = "p".
digit_symbol_(NumProtons, Symbol) :-
    NumProtons < 10,
    NumProtons = 6,
    Symbol = "h".
digit_symbol_(NumProtons, Symbol) :-
    NumProtons < 10,
    NumProtons = 7,
    Symbol = "s".
digit_symbol_(NumProtons, Symbol) :-
    NumProtons < 10,
    NumProtons = 8,
    Symbol = "o".
digit_symbol_(NumProtons, Symbol) :-
    NumProtons < 10,
    NumProtons = 9,
    Symbol = "e".
digit_symbol_(NumProtons, Symbol) :-
    NumProtons >= 10,
    T0 is NumProtons mod 10,
    T1 is NumProtons // 10,
    digit_symbol_(T0, N0),
    digit_symbol_(T1, N1),
    string_concat(N1, N0, Symbol).
