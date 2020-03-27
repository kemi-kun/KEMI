:- use_module(library(pcre),[re_compile/3]).
:- use_module(utils,[add_dict/4,join_dict/3,re_matchsub_mul/5]).
:- use_module(facts,[num_protons/2,en/2,element_name/2,multiplicative_prefix/2]).

% idea -> actively remove things before?
contains(Formula, Elements) :-
    % math enclosed groups // with no commas inside sq brackets
    % (?:\((?<paren>[^)]*)\)|\[(?<bracket>[^]]*)]|{(?<braces>[^}]*)})(?<num>[1-9][0-9]*)*
    re_compile("(?:\\((?<paren>[^)]*)\\)|\\[(?<bracket>[^]]*)]|{(?<braces>[^}]*)})(?<num>[1-9][0-9]*)*",Regex,[]),
    re_matchsub_mul(Regex,Formula,Subs,[], Leftover),
    Leftover \= Formula,
    contains(Leftover, E0),
    writeln("asdfg"),
    contains_(Subs, E1),
    join_dict(E0, E1, Elements).
contains_([H|T], Elements) :-
    writeln("ppp1"),
    ( % Get enclosed formula
        get_dict(paren, H, Enclosed);
        get_dict(bracket, H, Enclosed);
        get_dict(braces, H, Enclosed)
    ),
    get_dict(num, H, MulString),
    (
        MulString \= "" ->
            number_string(Multiple, MulString),
            writeln(Multiple),
            contains(Enclosed, E1, Multiple), !;
        contains(Enclosed, E1)
    ),
    (
        contains_(T, E0),
        join_dict(E0, E1, Elements),
        !;
        join_dict(_{}, E1, Elements)
    ).
contains(BaseFormula, Elements) :-
    writeln("ppp2"),
    % Match elements, their isotope, and number of atoms
    % (?<isotope>[1-9][0-9]*)?(?<elem>[A-Z][a-z]*)(?<atoms>[1-9][0-9]*)?
    re_compile("(?<isotope>[1-9][0-9]*)?(?<elem>[A-Z][a-z]*)(?<atoms>[1-9][0-9]*)?",Regex,[]),
    re_matchsub_mul(Regex,BaseFormula,Subs,[], Leftover),
    contains_base_(Subs, Elements).
contains_base_([H|T], Elements) :-
    % writeln(H),
    get_dict(elem, H, Symbol),
    element_name(Symbol, _), % must be element
    atom_string(AtomSymbol, Symbol),
    (
        get_dict(atoms, H, T0) -> 
            number_string(NumAtoms, T0);
        NumAtoms is 1
    ),
    % writeln(NumAtoms),
    (
        contains_base_(T, E0),
        add_dict(AtomSymbol, E0, NumAtoms, Elements),
        !;
        add_dict(AtomSymbol, _{}, NumAtoms, Elements)
    ).
contains(BaseFormula, Elements, Multiple) :-
    writeln("ppp3"),
    nonvar(Multiple),
    % Match elements, their isotope, and number of atoms
    % (?<isotope>[1-9][0-9]*)?(?<elem>[A-Z][a-z]*)(?<atoms>[1-9][0-9]*)?
    re_compile("(?<isotope>[1-9][0-9]*)?(?<elem>[A-Z][a-z]*)(?<atoms>[1-9][0-9]*)?",Regex,[]),
    re_matchsub_mul(Regex,BaseFormula,Subs,[], Leftover),
    contains_base_(Subs, Elements, Multiple).
contains_base_([H|T], Elements, Multiple) :-
    nonvar(Multiple),
    % writeln(H),
    get_dict(elem, H, Symbol),
    element_name(Symbol, _), % must be element
    atom_string(AtomSymbol, Symbol),
    (
        get_dict(atoms, H, T0) -> 
            number_string(T1, T0),
            writeln("hi"),
            NumAtoms is T1 * Multiple;
        writeln("ho"),
        NumAtoms is 1 * Multiple
    ),
    % writeln(NumAtoms),
    (
        contains_base_(T, E0),
        add_dict(AtomSymbol, E0, NumAtoms, Elements),
        !;
        add_dict(AtomSymbol, _{}, NumAtoms, Elements)
    ).


