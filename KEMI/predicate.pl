append_suffix(Element, Suffix, Result) :-
    (
        Suffix = "ide" -> idify(Element, Result);
        Suffix = "ane" -> anify(Element, Result);
        Suffix = "yl" -> ylify(Element, Result);
        Suffix = "ylidene" -> ylidenify(Element, Result);
        Suffix = "ylidyne" -> ylidynify(Element, Result)
    ).

idify(Element, Result) :-
    false.

anify(Element, Result) :-
    false.

ylify(Element, Result) :-
    false.

ylidenify(Element, Result) :-
    false.

ylidynify(Element, Result) :-
    false.