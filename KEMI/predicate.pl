:- use_module(elements, [group/2,element_name/2]).


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
%  TODO: -Element, +Result
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


anify(Element, Result) :-
    false.


ylify(Element, Result) :-
    false.


ylidenify(Element, Result) :-
    false.


ylidynify(Element, Result) :-
    false.