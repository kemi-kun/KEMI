:- module(utilities, [extract_term/2]).

extract_term(Term, Args) :-
    Term =.. [_|Args].
