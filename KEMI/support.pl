:- module(support,[multiplicative_prefix/2]).
:- use_module(utils,[split_decimal/2]).
:- use_module(strutils,[join/3]).
:- use_module(listutils,[enumerate/2]).
:- use_module(facts,[multiplicative_prefix_fact/2,multiplicative_affix_fact/2]).


%!  get_affix(+Num, +Digit, +Affix) is semidet.
%!  get_affix(-Num, -Digit, +Affix) is det.
%!  get_affix(-Num, +Digit, +Affix) is det.
%!  get_affix(+Num, -Digit, +Affix) is det.
%!  get_affix(+Num, +Digit, -Affix) is det.
%!  get_affix(+Num, -Digit, -Affix) is det.     # TODO: Fix this, should be multi
%!  get_affix(-Num, +Digit, -Affix) is multi.
%!  get_affix(-Num, -Digit, -Affix) is multi.   # TODO: Fix this, should solbed when +Num -Digit is solved
%
get_affix(Num, Digit, Affix) :-
    Number = Num * Digit,
    multiplicative_affix_fact(Number, Affix) -> true;
        multiplicative_affix_fact(Num, Affix_),
        (
            Digit = 1 -> string_concat(Affix_, "conta", Affix);
            Digit = 2 -> string_concat(Affix_, "cta", Affix);
            Digit = 3 -> string_concat(Affix_, "lia", Affix)
        ).
