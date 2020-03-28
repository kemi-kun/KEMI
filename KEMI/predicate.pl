:- use_module(elements, [element_name/2]).


append_suffix(Element, Suffix, Result) :-
    (
        Suffix = "ide" -> idify(Element, Result);
        Suffix = "ane" -> anify(Element, Result);
        Suffix = "yl" -> ylify(Element, Result);
        Suffix = "ylidene" -> ylidenify(Element, Result);
        Suffix = "ylidyne" -> ylidynify(Element, Result)
    ).

% group(_, Number) :-
%     Number = 17.

idify(Element, Result) :-
    group(Element, 18),
    element_name(Element, Name),
    string_concat(_, "on", Name),
    string_concat(Name, "ide", Result),
    !.
idify(Element, Result) :-
    element_name(Element, Name),
    not(group(Element, 18)),
    (
        string_concat(Root, "en", Name), !;
        string_concat(Root, "ese", Name), !;
        string_concat(Root, "ic", Name), !;
        string_concat(Root, "ine", Name), !;
        string_concat(Root, "ium", Name), !;
        string_concat(Root, "ogen", Name), !;
        string_concat(Root, "on", Name), !;
        string_concat(Root, "orus", Name), !;
        string_concat(Root, "um", Name), !;
        string_concat(Root, "ur", Name), !;
        string_concat(Root, "yen", Name), !;
        string_concat(Root, "y", Name), !;
        string_concat(Name, "ide", Result), !
    ),
    string_concat(Root, "ide", Result),
    !.
    
anify(Element, Result) :-
    false.

ylify(Element, Result) :-
    false.

ylidenify(Element, Result) :-
    false.

ylidynify(Element, Result) :-
    false.