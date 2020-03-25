%! list_remove(+In: list, +Element: atom, -Out: list) is det.
%! list_remove(-In: list, +Element: atom, +Out: list) is det.
%  
%  Remove the first appearance of `Element` in list `In`
%  and return the result list as `Out`.
%  Return false if `Element` is not in `In`
%  
list_remove(In, Element, Out) :-
    select(Element, In, Out), 
    !.
