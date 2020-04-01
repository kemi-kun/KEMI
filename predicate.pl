:- module(predicate,[append_suffix/3]).

:- use_module(elements, [group/2, element_name/2, group_/2]).
:- use_module(facts, [alternative_element_name/2,latin_element_name_fact/2]).


%! append_suffix(+ElementName: string, +Suffix: string, -Result: string) is det.
%! append_suffix(+ElementName: string, +Suffix: string, +Result: string) is semidet.
%
%  Append `Suffix` to the name of `Element`
%
append_suffix(ElementName, Suffix, Result) :-
    (
        Suffix = "ide" -> idify(ElementName, Result);
        Suffix = "ane" -> anify(ElementName, Result);
        Suffix = "yl" -> ylify(ElementName, Result);
        Suffix = "ylidene" -> ylidenify(ElementName, Result);
        Suffix = "ylidyne" -> ylidynify(ElementName, Result)
    ).


%! get_root_name_(+ElementName: string, SuffixList: list, Result: string) is det.
%
%  Match suffix with element name
%  Then return a name without that suffix (root name)
%
get_root_name_(ElementName, [], Result) :-
    % ElemntName doesn't match any suffix in SuffixList
    Result = ElementName,
    !.
get_root_name_(ElementName, SuffixList, Result) :-
    var(ElementName), nonvar(Result) -> 
        get_root_name_re_(ElementName, SuffixList, Result);
    SuffixList = [ESuffixH|ESuffixT],
    (
        string_concat(Root, ESuffixH, ElementName) -> Result = Root, !;
        get_root_name_(ElementName, ESuffixT, Result)
    ).
get_root_name_re_(ElementName, SuffixList, Result) :-
    var(ElementName),
    SuffixList = [ESuffixH|ESuffixT],
    string_concat(Result, ESuffixH, Name),
    (
        element_name(_, Name) -> ElementName = Name;
        get_root_name_(ElementName, ESuffixT, Result)
    ).


%!  idify(+ElementName:string, +Result:string) is semidet.
%!  idify(+ElementName:string, -Result:string) is semidet.
%!  idify(-ElementName:string, -Result:string) is multi.
%!  idify(-ElementName:string, +Result:string) is semidet.
%
%   Add suffix -ide to the name of `Element`.
%
idify(ElementName, Result) :-
    var(ElementName), nonvar(Result),
    idify_(ElementName, Result),
    !.
idify(ElementName, Result) :-
    idify_(ElementName, Result).
idify_(ElementName, Result) :-
    element_name(Element, ElementName),
    (
        Element = zinc ->
            string_concat(ElementName, "ide", Result);

        group(Element, 18),
        string_concat(_, "on", ElementName) ->
            string_concat(ElementName, "ide", Result);

        latin_element_name_fact(Element, AltName),
        not(latin_name_exception(Element)),
        member(X, ["ium", "um"]),
        string_concat(Root, X, AltName) ->
            string_concat(Root, "ide", Result);

        member(Y, ["ogen", "orus", "ygen", "ese", "ine", "ium",
                   "en", "ic", "on", "um", "ur", "y"]),
        string_concat(Root, Y, ElementName) ->
            string_concat(Root, "ide", Result)
    ).

latin_name_exception(antimony).
latin_name_exception(mercury).
latin_name_exception(potassium).
latin_name_exception(sodium).
latin_name_exception(tungsten).
latin_name_exception(caesium).


%!  anify(+ElementName:string, +Result:string) is semidet.
%!  anify(+ElementName:string, -Result:string) is semidet.
%!  anify(-ElementName:string, -Result:string) is multi.
%!  anify(-ElementName:string, +Result:string) is semidet.
%
%   Add suffix -ane to `ElementName
%
anify(ElementName, Result) :-
    var(ElementName), nonvar(Result),
    anify_(ElementName, Result),
    !.
anify(ElementName, Result) :-
    anify_(ElementName, Result).
anify_(ElementName, Result) :-
    element_name(Element, ElementName),
    (
        alternative_element_name(Element, Name), not(latin_name_exception(Element)) -> true;
        anify_element_root_exception(Element, Name) -> true;
        element_name(Element, Name)
    ),
    (   % only 13-17 suffixes are considred
        member(Suffix, ["anium", "inium", "onium", "urium",
                        "aium", "eium", "enic", "icon", "inum", "orus", "ogen", % "ogen" is not in 13-17
                        "gen", "ine", "ium",
                        "on", "um", "ur",
                        ""]),
        string_concat(Root_, Suffix, Name) ->
            (
                string_concat(Temp, "y", Root_) ->
                    string_concat(Temp, "i", Root);
                Root = Root_
            ),
            string_concat(Root, "ane", Result)
    ),

    % check
    ExcludedNames = [
        "carbane", "aluminane", "bismane", "oxane", "thiane", 
        "selenane", "tellurane", "polonane"
    ],
    not(member(Result, ExcludedNames)).

anify_element_root_exception(nitrogen, "az"). % from "azote"
anify_element_root_exception(indium, "indig"). % from "indigo"

%! ylify(+ElementName: string, -Result: string) is det.
%! ylify(+ElementName: string, +Result: string) is semidet.
%
%  If `ElementName` have ﬁnal -e, remove -e and add suffix -yl
%  Add suffix -yl to `ElementName`
%  
ylify(ElementName, Result) :-
    (
        string_concat(NameElideE, "e", ElementName) -> Name_ = NameElideE;
        Name_ = ElementName
    ),
    (
        Name_ = ElementName -> string_concat(Name_, "yl", Result);
        string_concat(Name_, "yl", Result)
    ),
    !.


%! ylidenify(+ElementName: string, -Result: string) is det.
%! ylidenify(+ElementName: string, +Result: string) is semidet.
%
%  If `ElementName` have ﬁnal -e, remove -e and add suffix -ylidene
%  Add suffix -ylidene to `ElementName`
%  
ylidenify(ElementName, Result) :-
    (
        string_concat(NameElideE, "e", ElementName) -> Name_ = NameElideE;
        Name_ = ElementName
    ),
    (
        Name_ = ElementName -> string_concat(Name_, "ylidene", Result);
        string_concat(Name_, "ylidene", Result)
    ),
    !.


%! ylidynify(+ElementName: string, -Result: string) is det.
%! ylidynify(+ElementName: string, +Result: string) is semidet.
%
%  If `ElementName` have ﬁnal -e, remove -e and add suffix -ylidyne
%  Add suffix -ylidyne to `ElementName`
%  
ylidynify(ElementName, Result) :-
    (
        string_concat(NameElideE, "e", ElementName) -> Name_ = NameElideE;
        Name_ = ElementName
    ),
    (
        Name_ = ElementName -> string_concat(Name_, "ylidyne", Result);
        string_concat(Name_, "ylidyne", Result)
    ),
    !.


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
%! get_standard_bonding_number(+Element: atom, +BondingNum: integer) is semidet.
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