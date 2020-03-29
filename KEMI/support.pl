:- module(support,[multiplicative_prefix/2]).
:- use_module(utils,[split_decimal/3,split_digits/2]).
:- use_module(ustr,[join/3]).
:- use_module(ulist,[enumerate/2,range/4]).
:- use_module(facts,[multiplicative_prefix_fact/2,multiplicative_affix_fact/2,complex_multiplicative_prefix_fact/2]).


%!	multiplicative_prefix(+Number, +Prefix) det.
%!	multiplicative_prefix(+Number, -Prefix) det.
%!	multiplicative_prefix(-Number, -Prefix) det.    # TODO: Fix (returns first fact)
%!	multiplicative_prefix(-Number, +Prefix) semidet. (ERROR when Prefix is wrong)
%
%
multiplicative_prefix(Number, Prefix) :-
    multiplicative_prefix_fact(Number, Prefix) -> true;
    multiplicative_prefix_(Number, Prefix).
multiplicative_prefix_(Number, Prefix) :-
    Number = 0 -> Prefix = "";
    multiplicative_affix_fact(Number, Prefix) -> true;
    nonvar(Number),
    split_decimal(Number, First, Rest),
    get_affix(First, CurrentPart),
    multiplicative_prefix_(Rest, RecursePart),
    string_concat(RecursePart, CurrentPart, Prefix).

%!  get_affix(+Number, +Affix) is semidet.
%!  get_affix(+Number, -Affix) is semidet.
%!  get_affix(-Number, -Affix) is ERROR.
%!  get_affix(-Number, +Affix) is semidet.
%
%   True when `Number` is a number with the form "[1-9]0*" and `Affix` is its generated affix
%   OR when affix is found in fact.
%
get_affix(Number, Affix) :-
    multiplicative_affix_fact(Number, Affix) -> true;
    gen_affix_(Number, Affix), !.

%!  gen_affix_(+Number, +Affix) is semidet.
%!  gen_affix_(+Number, -Affix) is semidet.
%!  gen_affix_(-Number, -Affix) is ERROR.
%!  gen_affix_(-Number, +Affix) is ERROR.
%
%   True when `Number` is a number with the form "[1-9]0*" and `Affix` is its generated affix.
%
gen_affix_(Number, Affix) :-
    split_digits(Number, [Num|T]),
    forall(member(E, T), E =:= 0),
    length(T, NumZeroes),
    multiplicative_affix_fact(Num, Affix_),
    (
        NumZeroes = 1 -> string_concat(Affix_, "conta", Affix);
        NumZeroes = 2 -> string_concat(Affix_, "cta", Affix);
        NumZeroes = 3 -> string_concat(Affix_, "lia", Affix)
    ).

%!	multiplicative_prefix(+Number, +Prefix) det.
%!	multiplicative_prefix(+Number, -Prefix) det.
%!	multiplicative_prefix(-Number, -Prefix) det.        # TODO: Fix (returns first fact)
%!	multiplicative_prefix(-Number, +Prefix) failure.    # TODO: FIx
%
%   Note: Depends on multiplicative_prefix/2.
%
complex_multiplicative_prefix(Number, Prefix) :-
    complex_multiplicative_prefix_fact(Number, Prefix) -> true;
    multiplicative_prefix(Number, SimplePrefix),
    string_concat(SimplePrefix, "kis", Prefix).
