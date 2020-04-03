:- module(uchem,[count_atoms/2,get_net_charge/2,get_num_atoms/3,get_num_elements/2,get_all_elements/2,get_element/3]).

:- use_module(elements).
:- use_module(facts).
:- use_module(support).
:- use_module(ustr).
:- use_module(ucollections).
:- use_module(unum).


element_quantity(Symbol, Quantity) :-
    Quantity > 0,
    element_fact(_, _, Symbol, _, _).
formula_to_quantified(Raw, Elements) :-
    re_split("([1-9][0-9]*)"/n, Raw, [RawSymbol, Quantity|_], []),
    element_fact(_, _, Symbol, _, _),
    Elements =.. [element_quantity, Symbol, Quantity],
    call(Elements),
    atom_string(RawSymbol, Symbol).

%!  extract_elements_from_formula(+Formula:string, -Elements:list) is det.
%
%   Return a list of element_quantity(Symbol, Quantity)
%
%   Note: Error when the symbol has 1 letter
extract_elements_from_formula(Formula, Elements) :-
    extract_elements_(Formula, 0, "", Elements).

determine_multiplicity(Formula, Result) :- 
    formula_to_quantified(Formula, Result).
determine_multiplicity(Formula, Result) :- 
    Result =.. [element_quantity, Formula, 1],
    call(Result).

extract_elements_(Formula, Start, _, _) :-
    string_length(Formula, Length),
    Start = Length.
extract_elements_(Formula, Start, String, End) :-
    string_length(Formula, Length),
    Start < Length,
    Final is Length - 1 - Start,
    sub_string(Formula, Final, _, Start, Out),
    Trim is Start + 1,
    string_concat(Out, String, ConcatString),
    (
        is_upper(Out) -> extract_elements_(Formula, Trim, "", End2); 
        extract_elements_(Formula, Trim, ConcatString, End2), !
    ),
    (
        is_upper(Out) -> determine_multiplicity(ConcatString, Result), 
                         Sth = [Result];
        Sth = []
    ),
    append(End2, Sth, End).

extract_term(Term, Args) :-
    Term =.. [_|Args].


%!  get_element(+Formula: string, +Index: int, -Element: atom) is det.
%
%   Get element at `Index` position in formula `Formula`.
%   (index starts at 0)
%   Return false if there's no element at `Index`
%
get_element(Formula, Index, Element) :-
    count_atoms(Formula, Atoms),
    nth0(Index, Atoms, Element-_Amount).


%!  get_all_elements(+Formula:string, +ElementSet:list) is det.
%!  get_all_elements(+Formula:string, -ElementSet:list) is det.
%!  get_all_elements(-Formula:string, -ElementSet:list) is failure.
%!  get_all_elements(-Formula:string, +ElementSet:list) is failure.
%
%   Return a set (a list without duplicate) of elements in
%   formula `Formula`
%
%   get_all_elements("ClOF", R).
%   get_all_elements("PH5", R).
%
get_all_elements(Formula, ElementSet) :-
    nonvar(Formula),
    remove_chars(Formula, "()[]{}0123456789+-", Formula_),
    extract_symbols_(Formula_, SymbolList),
    list_to_set(SymbolList, SymbolSet),
    maplist(element_symbol, ElementSet, SymbolSet).

extract_symbols_(Formula, SymbolList) :-
    Formula = "" -> SymbolList = [];
    element_fact(_, _, Symbol, _, _),       % TODO: use `element_symbol` instead
    string_length(Symbol, L),
    sub_string(Formula, 0, L, A0, Symbol),
    sub_string(Formula, L, A0, 0, Rest),
    extract_symbols_(Rest, SymbolList_),
    append([Symbol], SymbolList_, SymbolList),
    !.



%!  get_num_elements(+Formula: string, -Amount: int) is det.
%
%   Return the total number of types of elements in formula `Formula`.
%
get_num_elements(Formula, Amount) :-
    get_all_elements(Formula, Elements),
    length(Elements, Amount).


sorted_by_en_(List, SortedList) :-
    map_list_to_pairs(en, List, ElementEnPairs),
    keysort(ElementEnPairs, ElementEnSorted),
    pairs_values(ElementEnSorted, SortedList).


%!  sorted(+Key: string, +List: list, -SortedList: list) is det.
%
%   Sort list `List` by `Key` (ascending order)
%   TODO: sort by alphabet
%
%   sorted("en", ['sodium', 'chlorine', 'hydrogen'], ['sodium', 'hydrogen', 'chlorine']).
sorted(Key, List, SortedList) :-
    (
        Key = "en", sorted_by_en_(List, SortedList);
        Key = "alphabet", sort(0, @=<, List, SortedList);
        Key = "", false
    ),
    !.


%! get_num_atoms(+Formula:string, +Element:atom, -NumAtoms:int) is det.
%
%  Return the amount of element `Element` in formula `Formula`
%
get_num_atoms(Formula, Element, NumAtoms) :-
    count_atoms(Formula, Atoms),
    dict_pairs(Dict, _, Atoms),
    get_dict(Element, Dict, NumAtoms).


%! count_atoms(+Formula:string, -Atoms:list(Element-Amount)) is det.
%
%  Count the number of each elements in the formula, in order of discovery.
%
count_atoms(Formula, Atoms) :-
    nonvar(Formula),
    remove_isotope_info(Formula, Formula1),     % Can remove once isotopes are considred in this KB
    count_atoms_(Formula1, 1, Atoms).

count_atoms_(Formula, Multiplicity, Atoms) :-
    % Deals with enclosed formula recursion
    re_compile("(?:\\((?<paren_enclosed>[^)]*)\\)|\\[(?<bracket_enclosed>[^]]*)]|{(?<braces_enclosed>[^}]*)})(?:(?<ion>(?<num_ions>[1-9][0-9]*)?[+-])|(?<multiple>[1-9][0-9]*))?", Regex, []),
    re_match(Regex, Formula,[]) ->
        re_finditer(Regex, Formula, Matches_, []),
        maplist(dict_remove_on_cond(value_is_empty_string), Matches_, Matches),

        maplist(get_dict_optional(['paren_enclosed', 'bracket_enclosed', 'braces_enclosed']), Matches, EnclosedList),
        maplist(get_dict_or_default("1", 'multiple'), Matches, MultipleStrList),
        maplist(number_string, MultipleList_, MultipleStrList),
        maplist(multiply(Multiplicity), MultipleList_, MultipleList),
        maplist(count_atoms_, EnclosedList, MultipleList, AtomsList),
        foldl(join_pairs_by_keys(plus), AtomsList, [], RecursedAtoms),
        
        maplist(get_dict(0), Matches, FullMatches),
        foldl(remove, FullMatches, Formula, Leftovers),
        
        count_atoms__(Leftovers, Multiplicity, CurrentAtoms),  % Should call the bottom
        join_pairs_by_keys(plus, RecursedAtoms, CurrentAtoms, Atoms),
        !;
    count_atoms__(Formula, Multiplicity, Atoms).

count_atoms__(Formula, Multiplicity, Atoms) :-
    % Deals with base formula
    re_finditer("(?<sham>[ημ](?:[2-9][0-9]*)?-)?(?<symbol>[A-Z][a-z]*)(?<num>[1-9][0-9]*)?", Formula, Matches_, []),
    maplist(dict_remove_on_cond(value_is_empty_string), Matches_, Matches),

    maplist(get_dict('symbol'), Matches, Symbols),
    maplist(element_symbol, Elements, Symbols),
    
    maplist(get_dict_or_default("1", 'num'), Matches, BaseAmountStrs),
    maplist(number_string, BaseAmounts, BaseAmountStrs),
    
    maplist(multiply(Multiplicity), BaseAmounts, TrueAmounts),
    pairs_keys_values(Atoms, Elements, TrueAmounts).

remove(Sub, String, Result) :-
    sub_string(String, BS, _, AS, Sub),
    sub_string(String, 0, BS, _, Before),
    sub_string(String, _, AS, 0, After),
    string_concat(Before, After, Result).


%!  remove_isotope_info(+Formula:string, +NewFormula:string) is semidet.
%!  remove_isotope_info(+Formula:string, -NewFormula:string) is det.
%!  remove_isotope_info(-Formula:string, -NewFormula:string) is failure.   # one-way function
%!  remove_isotope_info(-Formula:string, +NewFormula:string) is failure.   # one-way function
%
%   Removes all isotope information from the formula.
%
remove_isotope_info(Formula, NewFormula) :-
    nonvar(Formula),
    % matches front-formula isotope declaration
    re_replace("(?:^\\[(?<isotopes_info>[^]]*,[^]]*)])", "", Formula, Formula1),
    % matches in-formula isotopes
    re_finditer("(?:\\[(?<neutrons>[1-9][0-9]*)(?<element>[A-Z][a-z]*)])", Formula1, Matches, []),
    foldl(replace_match(0, 'element'), Matches, Formula1, NewFormula).

replace_match(Key0, Key1, Match, S0, S1) :-
    get_dict(Key0, Match, Match0),
    get_dict(Key1, Match, Match1),
    replace(S0, Match0, Match1, S1).

% Regex patterns
%
% in-formula isotope:
%   (?:\[(?<neutrons>[1-9][0-9]*)(?<element>[A-Z][a-z]*)])
%
% front-formula isotopde declaration:  % TODO: deal with one element decl
%   (?:^\[(?<isotopes_info>[^]]*,[^]]*)])
%
% enclosed group & multiplicity & ions:
%   Note: somehow prolog doesn't support non-capturing gorup?
%   (?:\(([ημ](?:[2-9][0-9]*)?-)?(?<paren_enclosed>[^)]*)\)|\[(?<bracket_enclosed>[^]]*)]|{(?<braces_enclosed>[^}]*)})(?:(?<ion>(?<num_ions>[1-9][0-9]*)?[+-])|(?<multiple>[1-9][0-9]*))?
%   (?:\((?<sham1>[ημ](?:[2-9][0-9]*)?-)?(?<paren_enclosed>[^)]*)\)|\[(?<sham2>[ημ](?:[2-9][0-9]*)?-)?(?<bracket_enclosed>[^]]*)]|{(?<sham3>[ημ](?:[2-9][0-9]*)?-)?(?<braces_enclosed>[^}]*)})(?:(?<ion>(?<num_ions>[1-9][0-9]*)?[+-])|(?<multiple>[1-9][0-9]*))?
%
% individual elements & amounts:
%   (?<symbol>[A-Z][a-z]*)(?<num>[1-9][0-9]*)?
%
% individual elements & amounts & sham:
%   (?<sham>[ημ](?:[2-9][0-9]*)?-)?(?<symbol>[A-Z][a-z]*)(?<num>[1-9][0-9]*)?



%!  get_num_charge_str_(+Formula: string, -ChargeStr: string) is det.
%!  get_num_charge_str_(+Formula: string, +ChargeStr: string) is semidet.
%!  get_num_charge_str_(-Formula: string, -ChargeStr: string) is failure.
%
%   Get a string of charge from `Formula`
%
%   get_num_charge_str_("[AA]300-", "-300").
%   get_num_charge_str_("+200[B2B]", "+200").
%
get_num_charge_str_(Formula, ChargeStr) :-
    (
        re_matchsub("^(?<charge>[+\\-][1-9][0-9]*).*$", Formula, SubDict_, []) -> SubDict = SubDict_, !;
        re_matchsub("^(?<charge>[+\\-]).*", Formula, SubDict_, []) -> SubDict = SubDict_;
        re_matchsub("^.*?(?<charge>([1-9][0-9]*)?[+\\-])$", Formula, SubDict_, []) -> SubDict = SubDict_;
        SubDict = rematch{charge: ""}
    ),
    get_dict(charge, SubDict, ChargeStr_),
    (
        string_concat(Number, "+", ChargeStr_) -> string_concat("+", Number, ChargeStr), !;
        string_concat(Number, "-", ChargeStr_) -> string_concat("-", Number, ChargeStr);
        ChargeStr = ChargeStr_
    ).
% get_num_charge_str_("[Na]200+", R).
% get_num_charge_str_("+200[Na]", R).

%!  get_net_charge(+Formula: string, -NetCharge: int) is det.
%!  get_net_charge(+Formula: string, -NetCharge: int) is semidet.
%!  get_net_charge(-Formula: string, +NetCharge: int) is failure.
%
%   Get net charge from `Formula`
%   Return 0 if on charge is found in `Formula`
%
%   Note: Need parentheses to separate compound and charge
%     examples [formula], [formula]charge+/-, [formula]+/-, 
%              +/-[formula], +/-charge[formula]
%
get_net_charge(Formula, NetCharge) :-
    get_num_charge_str_(Formula, ChargeStr),
    string_length(ChargeStr, Length),
    Length > 1,
    number_chars(NetCharge, ChargeStr),
    !.
get_net_charge(Formula, NetCharge) :-
    get_num_charge_str_(Formula, ChargeStr),
    (
        ChargeStr = "+" -> NetCharge = 1, !;
        ChargeStr = "-" -> NetCharge = -1, !;
        ChargeStr = "" -> NetCharge = 0
    ).
