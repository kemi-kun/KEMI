use_module('starter').

:- begin_tests(test).

test(param) :-
    assertion(isotope('H', 'H-2')).

:- end_tests(test).
