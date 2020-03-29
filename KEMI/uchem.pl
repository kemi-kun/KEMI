:- module(uchem,[remove_parentheses_/2,get_net_charge/2,get_num_atoms/3,get_num_elements/2,get_all_elements/2,get_element/3]).
:- use_module('facts',[en/2,element_fact/5]).
:- use_module('ustr',[split/2]).

%! remove_parentheses_(+String: string, +Result: string) is semidet.
%! remove_parentheses_(+String: string, -Result: string) is det.
%
%  Remove "(", ")", "[", "]", "{" and "}" from `String`.
%
% remove_parentheses_("NaCl", "NaCl").
% remove_parentheses_("[Al(POCl3)6]3+", "AlPOCl363+").
% remove_parentheses_("H2[PtCl6]", "H2PtCl6").
% remove_parentheses_("ab[(c)][d2](3)", "abcd23").
remove_parentheses_(String, Result) :-
    re_split("[(\\)\\[\\]\\{\\}]", String, R1, []),
    exclude(is_parens_, R1, Result_),
    % delete(R2, "", Result_),
    atomics_to_string(Result_, Result).
is_parens_(Elem) :- split("()[]{}", X), member(Elem, X).

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

%! get_element(+Formula: string, +Index: integer, -Element: atom) is det.
%
%  Get element at `Index` position in formula `Formula`.
%  (index starts at 0)
%  Return false if there's no element at `Index`
%
get_element(Formula, Index, Element) :-
    remove_parentheses_(Formula, Formula_),
    extract_elements_from_formula(Formula_, Elements),
    nth0(Index, Elements, ElementQuantity),
    extract_term(ElementQuantity, [Symbol|_]),
    element_fact(Element, _, Symbol, _, _).

%! get_all_elements(+Formula:string, +ElementSet:list) is det.
%! get_all_elements(+Formula:string, -ElementSet:list) is det.
%! get_all_elements(-Formula:string, -ElementSet:list) is failure.
%! get_all_elements(-Formula:string, +ElementSet:list) is failure.
%
%  Return a set (a list without duplicate) of elements in
%  formula `Formula`
%
get_all_elements(Formula, ElementSet) :-
    nonvar(Formula),
    remove_parentheses_(Formula, Formula_),
    extract_elements_from_formula(Formula_, ElementQuantities),
    extract_element_quantity(ElementQuantities, ElementList),
    list_to_set(ElementList, ElementSet),
    !.

% [element_quantity("Na", 1), element_quantity("Cl", 1)] => [sodium, chlorine]
extract_element_quantity([], []).
extract_element_quantity(ElementQuantities, Elements) :-
    ElementQuantities = [EleQuanH|EleQuanT],
    extract_term(EleQuanH, [SymbolH|_]),
    extract_element_quantity(EleQuanT, EleT),
    element_fact(EleH, _, SymbolH, _, _),
    append([EleH], EleT, Elements),
    !.


% get_all_elements("ClOF", R).
% get_all_elements("PH5", R).

%! get_num_elements(+Formula: string, -Amount: integer) is det.
%
%  Return total number (num of type) of elements in
%  formula `Formula`
%
get_num_elements(Formula, Amount) :-
    remove_parentheses_(Formula, Formula_),
    extract_elements_from_formula(Formula_, ElementQuantities),
    extract_element_quantity(ElementQuantities, ElementList),
    list_to_set(ElementList, ElementSet),
    length(ElementSet, Amount),
    !.


sorted_by_en_(List, SortedList) :-
    map_list_to_pairs(en, List, ElementEnPairs),
    keysort(ElementEnPairs, ElementEnSorted),
    pairs_values(ElementEnSorted, SortedList).


%! sorted(+Key: string, +List: list, -SortedList: list) is det.
%
%  Sort list `List` by `Key` (ascending order)
%  TODO: sort by alphabet
%
% sorted("en", ['sodium', 'chlorine', 'hydrogen'], ['sodium', 'hydrogen', 'chlorine']).
sorted(Key, List, SortedList) :-
    (
        Key = "en", sorted_by_en_(List, SortedList);
        Key = "alphabet", sort(0, @=<, List, SortedList);
        Key = "", false
    ),
    !.


%! get_num_atoms(+Formula: string,+Element: atom, -Amount: integer) is det.
%
%  Return amount of element in 
%  formula `Formula`
%
get_num_atoms(Formula, Element, Amount) :-
    remove_parentheses_(Formula, Formula_),
    extract_elements_from_formula(Formula_, ElementQuantities),
    extract_quantity_from_element(ElementQuantities,Element, Amount).
 
extract_quantity_from_element([],_,_).
extract_quantity_from_element(ElementQuantities,Element,Amount) :-
    ElementQuantities = [EleQuanH|EleQuanT],
    element_fact(Element, _, Symbol, _, _),
    extract_term(EleQuanH, [EleH|Num]),
    (EleH = Symbol -> Amount is Num ;
    extract_quantity_from_element(EleQuanT, Element, Amount)
    ),
    !.


%!  get_num_charge_str_(+Formula: string, -ChargeStr: string) is det.
%!  get_num_charge_str_(+Formula: string, +ChargeStr: string) is semidet.
%!  get_num_charge_str_(-Formula: string, -ChargeStr: string) is failure.
%
%   Get a string of charge from `Formula`
%
%   get_num_charge_str_("[AA]300-", "-300").
%   get_num_charge_str_("+200[B2B]", "+200").
get_num_charge_str_(Formula, ChargeStr) :-
    (
        re_matchsub("^.*?(?<charge>([1-9][0-9]*)?[+\\-]?)$", Formula, SubDict_, []) -> SubDict = SubDict_;
        re_matchsub("^(?<charge>[+-]?([1-9][0-9]*)?)?.*$", Formula, SubDict_, []) -> SubDict = SubDict_
    ),
    get_dict(charge, SubDict, ChargeStr_),
    (
        string_concat(Number, "+", ChargeStr_) -> string_concat("+", Number, ChargeStr), !;
        string_concat(Number, "-", ChargeStr_) -> string_concat("-", Number, ChargeStr);
        ChargeStr = ChargeStr_
    ).

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
