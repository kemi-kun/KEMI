/* 

Implements:
- IR-3 Elements
- Table VI
- Group
- Period
*/
:- use_module(facts,[element_fact/5]).

%! period(+Element:string, -Period:integer) is det.
%! period(-Element:string, +Period:integer) is multi.
%! period(-Element:string, -Period:integer) is multi.
%
%  True if element Element is in period Period.
%
period(Element, Period) :-
    element_fact(Element, _, _, Z, _),
    (
        Z =<   2 -> Period is 1;
        Z =<  10 -> Period is 2;
        Z =<  18 -> Period is 3;
        Z =<  36 -> Period is 4;
        Z =<  54 -> Period is 5;
        Z =<  86 -> Period is 6;
        Z =< 118 -> Period is 7;
        Period is 8
    ).

%! period2(+Element:string, -Period:integer) is multi.
%! period2(-Element:string, +Period:integer) is multi.
%! period2(-Element:string, -Period:integer) is multi.
%
%  True if element Element is in period Period.
%
period2(Element, Period) :-
    element_fact(Element, _, _, Z, _),
    List = [0, 2, 10, 18, 36, 54, 86, 118],
        nth0(Period, List, Proton),
        nth1(Period, List, Proton0),
        Z > Proton0,
        Z =< Proton.
