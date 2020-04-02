:- module(ialternative,[alternative_name/2]).
:- use_module(table_ix,[table_ix_name/2]).


alternative_name(Formula, Name) :-
    hydrogen_name(Formula, Name);
    % acid_nomenclature(Fomrula, Name);
    table_ix_name(Formula, Name).


hydrogen_name(Formula, Name) :-
    fail.
