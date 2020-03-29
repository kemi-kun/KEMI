:- use_module(elements, [group/2, element_name/2, group_/2]).
:- use_module(facts, [alternative_element_name_fact/2]).


%! append_suffix(+Element: atom, +Suffix: string, -Result: string) is det.
%! append_suffix(+Element: atom, +Suffix: string, +Result: string) is semidet.
%
%  Append `Suffix` to the name of `Element`
%
append_suffix(Element, Suffix, Result) :-
    (
        Suffix = "ide" -> idify(Element, Result);
        Suffix = "ane" -> anify(Element, Result);
        Suffix = "yl" -> ylify(Element, Result);
        Suffix = "ylidene" -> ylidenify(Element, Result);
        Suffix = "ylidyne" -> ylidynify(Element, Result)
    ).


idify_(ElementName, [], Result) :-
    % ElemntName doesn't match any suffix in SuffixList
    Result = ElementName,
    !.
idify_(ElementName, SuffixList, Result) :-
    SuffixList = [ESuffixH|ESuffixT],
    (
        string_concat(Root, ESuffixH, ElementName) -> Result = Root, !;
        idify_(ElementName, ESuffixT, Result)
    ).
%! idify(+Element: atom, -Result: string) is det.
%! idify(+Element: atom, +Result: string) is semidet.
%
%  Add suffix -ide to the name of `Element`
%  TODO: (-Element, +Result)
idify(Element, Result) :-
    % Add -ide to the end of Group 18 which ends with -on
    group(Element, 18),
    element_name(Element, Name),
    string_concat(_, "on", Name),
    string_concat(Name, "ide", Result),
    !.
idify(Element, Result) :-
    element_name(Element, Name),
    not(group(Element, 18)),
    SuffixList = [
        "ogen", "orus", "ygen", "ese", "ine", "ium", "en",
        "ic", "on", "um", "ur", "y"
    ],
    idify_(Name, SuffixList, Name_),
    string_concat(Name_, "ide", Result),
    !.


anify_(ElementName, [], Result) :-
    % ElemntName doesn't match any suffix in SuffixList
    Result = ElementName,
    !.
anify_(ElementName, SuffixList, Result) :-
    SuffixList = [ESuffixH|ESuffixT],
    (
        string_concat(Root, ESuffixH, ElementName) -> Result = Root, !;
        anify_(ElementName, ESuffixT, Result)
    ).
%! anify(+Element: atom, -Result: string) is det.
%! anify(+Element: atom, +Result: string) is semidet.
%
%  Add suffix -ane to the name of `Element`
%  TODO: (-Element, +Result)
anify(Element, Result) :-
    (
        alternative_element_name_fact(Element, Name_) -> Name = Name_;
        not(alternative_element_name_fact(Element, _)),
        element_name(Element, Name)
    ),
    SuffixList = [
        "inium", "onium", "urium", "aium", "eium", "enic",
        "icon", "inum", "ogen", "orus", "gen", "ine", "ium",
        "on", "um", "ur"
    ],
    anify_(Name, SuffixList, Root_),
    (
        string_concat(TempStr, "e", Root_) ->
            string_concat(TempStr, "i", Root);
        Root = Root_
    ),
    string_concat(Root, "ane", Result).


ylify(Element, Result) :-
    false.
    % element_name(Element, Name),
    % (
    %     string_concat(NameElideE, "e", Name) -> Name_ = NameElideE;
    %     Name_ = Name
    % ),
    % string_concat(Name_, "yl", Result).


ylidenify(Element, Result) :-
    false.


ylidynify(Element, Result) :-
    false.


%! replace_prefix(
%                   +Name1: string, -Name2: string, 
%                   +Prefix1: string, +Prefix2: string
%                ) is det.
%! replace_prefix(-Name1, +Name2, +Prefix1, +Prefix2) is det.
%! replace_prefix(+Name1, +Name2, +Prefix1, +Prefix2) is semidet.
%
%  Replace `Prefix1` in `Name1` with `Prefix2` in `Name2`
%
replace_prefix(Name1, Name2, Prefix1, Prefix2) :-
    nonvar(Name1),
    string_concat(Root, Prefix1, Name1),
    string_concat(Root, Prefix2, Name2),
    !.
replace_prefix(Name1, Name2, Prefix1, Prefix2) :-
    string_concat(Root, Prefix2, Name2),
    string_concat(Root, Prefix1, Name1).


%! get_standard_bonding_number(+Element: atom, -BondingNum: integer) is det.
%
%  X is group of elemnt 
%  If 13 ≤ X ≤ 15 -> X - 10 
%  Else If 16 ≤ X ≤ 17 -> 18 - X
%  Else FAIL
%
get_standard_bonding_number(Element, BondingNum):-
    Element \= hydrogen -> group_(Element, X),
    (
        13 =< X, X =< 15 -> BondingNum is X - 10;
        16 =< X, X =< 18 -> BondingNum is 18 - X;
        false
    ),
    !.