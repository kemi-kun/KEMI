:- module(str,[replace/4]).

%!  replace
%
%
replace(String, S1, S2, Result) :-
    sub_string(String, BS1, _, AS1, S1),
    sub_string(String, 0, BS1, _, Before),
    sub_string(String, _, AS1, 0, After),
    string_concat(Before, S2, T0),
    string_concat(T0, After, Result).