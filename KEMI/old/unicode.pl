:- module(unicode,[sub/2,sup/2]).


%!  sup(Number: integer, -String: string) 
%!  sup(C: atom, -SC: atom)
%
%   Given number (and supppoprted characters),
%   resolve the unicode superscript version.
%
sup(C, SC) :-
    atom(C),
    atom_length(C, 1),
    (
        C = '0' -> SC = '\u2070';
        C = '1' -> SC = '\u00B9';
        C = '2' -> SC = '\u00B2';
        C = '3' -> SC = '\u00B3';
        C = '4' -> SC = '\u2074';
        C = '5' -> SC = '\u2075';
        C = '6' -> SC = '\u2076';
        C = '7' -> SC = '\u2077';
        C = '8' -> SC = '\u2078';
        C = '9' -> SC = '\u2079';
        C = '+' -> SC = '\u207A';
        C = '-' -> SC = '\u207B';
        C = '(' -> SC = '\u207D';
        C = ')' -> SC = '\u207E';
        C = 'n' -> SC = '\u207F';
        fail
    ).
sup(Number, String) :-
    number(Number),
    number_chars(Number, CharList),
    fsup_(CharList, CL2),
    string_chars(String, CL2).
fsup_([H|T], R) :-
    T = [],
    sup(H, R1),
    R = [R1].
fsup_([H|T], R) :-
    T \= [],
    sup(H, R1),
    fsup_(T, R2),
    append([R1], R2, R).

%!  sub(Number: integer, -String: string) 
%!  sub(C: atom, -SC: atom)
%
%   Given number (and supppoprted characters),
%   resolve the unicode subscript version.
%
sub(C, SC) :-
    atom(C),
    atom_length(C, 1),
    (
        C = '0' -> SC = '\u2080';
        C = '1' -> SC = '\u2081';
        C = '2' -> SC = '\u2082';
        C = '3' -> SC = '\u2083';
        C = '4' -> SC = '\u2084';
        C = '5' -> SC = '\u2085';
        C = '6' -> SC = '\u2086';
        C = '7' -> SC = '\u2087';
        C = '8' -> SC = '\u2088';
        C = '9' -> SC = '\u2089';
        C = '+' -> SC = '\u208A';
        C = '-' -> SC = '\u208B';
        C = '(' -> SC = '\u208D';
        C = ')' -> SC = '\u208E';
        C = 'n' -> SC = '\u2099';
        fail
    ).
sub(Number, String) :-
    number(Number),
    number_chars(Number, CharList),
    fsub_(CharList, CL2),
    string_chars(String, CL2).
fsub_([H|T], R) :-
    T = [],
    sub(H, R1),
    R = [R1].
fsub_([H|T], R) :-
    T \= [],
    sub(H, R1),
    fsub_(T, R2),
    append([R1], R2, R).
