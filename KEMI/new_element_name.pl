% IR-3.1.1 (pg. 47)
digit(Proton, Name) :-
    Proton < 10,
    Proton = 0,
    Name = "nil".
digit(Proton, Name) :-
    Proton < 10,
    Proton = 1,
    Name = "un".
digit(Proton, Name) :-
    Proton < 10,
    Proton = 2,
    Name = "bi".
digit(Proton, Name) :-
    Proton < 10,
    Proton = 3,
    Name = "tri".
digit(Proton, Name) :-
    Proton < 10,
    Proton = 4,
    Name = "quad".
digit(Proton, Name) :-
    Proton < 10,
    Proton = 5,
    Name = "pent".
digit(Proton, Name) :-
    Proton < 10,
    Proton = 6,
    Name = "hex".
digit(Proton, Name) :-
    Proton < 10,
    Proton = 7,
    Name = "sept".
digit(Proton, Name) :-
    Proton < 10,
    Proton = 8,
    Name = "oct".
digit(Proton, Name) :-
    Proton < 10,
    Proton = 9,
    Name = "enn".
digit(Proton, Name) :-
    Proton >= 10,
    T0 is Proton mod 10,
    T1 is Proton // 10,
    digit(T0, N0),
    digit(T1, N1),
    string_concat(N1, N0, Name).

% replace substring S1 with S2 in String
replace(String, S1, S2, Result) :-
    sub_string(String, BS1, S1_len, AS1, S1),
    sub_string(String, 0, BS1, Z0, SBS1),
    sub_string(String, Z1, AS1, 0, SAS1),
    string_concat(SBS1, S2, T0),
    string_concat(T0, SAS1, Result).

name_check_i(Name, Result) :-
    replace(Name, "ii", "i", Result).
name_check_i(Name, Result) :-
    Result = Name.
name_check_n(Name, Result) :-
    replace(Name, "nnn", "nn", Result).
name_check_n(Name, Result) :-
    Result = Name.

name(Proton, Name) :-
    digit(Proton, N1),
    string_concat(N1, "ium", T0),
    name_check_i(T0, T1),
    name_check_n(T1, Name).
