:- module(icompositional,[boron_hydride_stoichiometric/2,general_stoichiometric/2,addition_compound_cn/2,ion_cn/2,binary_compound_cn/2,homonuclear_cn/2]).

:- use_module(elements,[element_name/2,group/2]).
:- use_module(facts,[addition_compound_exception/2]).
:- use_module(nomenclature,[iupac_name/2]).
:- use_module(predicate,[append_suffix/3]).
:- use_module(inorganic,[additive/2,substitutive/2,compositional/2]).
:- use_module(support,[get_neutral_specie/2,multiplicative_prefix/2,mul_prefix_except_mono/2]).
:- use_module(uchem,[count_atoms/2,get_net_charge/2,get_num_atoms/3,get_num_elements/2,get_all_elements/2,get_element/3]).
:- use_module(utils,[value_is_empty_string/1,dict_remove_on_cond/3,get_dict_or_default/4]).
:- use_module(ustr,[join/3]).
:- use_module(ialternative,[alternative/2]).


homonuclear_cn(Formula, Name) :-
    not(cation(Formula)),
    not(anion(Formula)),
    homonuclear(Formula),
    get_all_elements(Formula, Elements),
    memberchk(Element, Elements),
    get_num_atoms(Formula, Element, Amount),
    element_name(Element, ElementName),
    (
        group(Element, 18) -> mul_prefix_except_mono(Amount, MulPrefix);
        multiplicative_prefix(Amount, MulPrefix)
    ),
    string_concat(MulPrefix, ElementName, Name).
 
homonuclear(Formula) :-
    get_all_elements(Formula, Elements),
    length(Elements,1).


%!  binary_compound_cn(+Formula: string, -Name: string) is det.
%!  binary_compound_cn(+Formula: string, +Name: string) is det.
%!  binary_compound_cn(-Formula: string, +Name: string) is ERROR.
%
%   IR-5.2 p.81-82
%
binary_compound_cn(Formula, Name) :-
    (
        binary_compound(Formula),
        not(cation(Formula)),
        not(anion(Formula))
    ),
    get_element(Formula, 0, EPosElement),
    get_element(Formula, 1, ENegElement),
    element_name(EPosElement,EPosElementName),
    element_name(ENegElement,ENegElementName),
    append_suffix(ENegElementName, "ide", NegativePart),
    get_num_atoms(Formula,EPosElement, NumPositive),
    mul_prefix_except_mono(NumPositive, EPosMulPrefix),
    get_num_atoms(Formula, ENegElement,  NumNegative),
    mul_prefix_except_mono(NumNegative, ENegMulPrefixEx),
    string_concat(EPosMulPrefix, EPosElementName, EPos),
    string_concat(EPos, " ", EPos_),
    string_concat(ENegMulPrefixEx, NegativePart, ENeg),
    string_concat(EPos_, ENeg, Name),
    !.
 
binary_compound(Formula) :-
    get_all_elements(Formula, Elements),
    length(Elements, 2),
    !.
 
get_various_name_(Formula, EPos, NumNegative, NegativePart, Name) :-
    fail.


cation(Formula) :-
    get_net_charge(Formula, NetCharge),
    NetCharge > 0.

anion(Formula) :-
    get_net_charge(Formula, NetCharge),
    NetCharge < 0.

monoatomic(Formula) :-
    get_all_elements(Formula, Elements),
    length(Elements, 1),
    Elements = [Element0|_],
    get_num_atoms(Formula, Element0, NumAtom),
    NumAtom is 1.

homopolyatomic(Formula) :-
    get_all_elements(Formula, Elements),
    length(Elements, 1),
    Elements = [Element0|_],
    get_num_atoms(Formula, Element0, NumAtom),
    NumAtom > 1.

get_ion_part_(NetCharge, IonSign, ChargeStr) :-
    string_concat("(", NetCharge, ChargeStr1),
    string_concat(ChargeStr1, IonSign, ChargeStr2),
    string_concat(ChargeStr2, ")", ChargeStr).


ion_cn(Formula, Name) :-
    cation_cn(Formula, Name);
    anion_cn(Formula, Name).


cation_cn(Formula, Name) :-
    monoatomic_cation_cn(Formula, Name);
    homopolyatomic_cation_cn(Formula, Name).


%!  monoatomic_cation_cn(+Formula: string, -Name: string) is multi.
%!  monoatomic_cation_cn(+Formula: string, +Name: string) is semidet.
%!  monoatomic_cation_cn(-Formula: string, +Name: string) is ERROR.
%
%   IR-5.3.2.2 p.82-83
%
monoatomic_cation_cn(Formula, Name) :-
    nonvar(Name) ->
        monoatomic_cation_cn_(Formula, Name), !;
    monoatomic_cation_cn_(Formula, Name).
% monoatomic_cation_cn("[Na]+", "sodium(1+)")
monoatomic_cation_cn_(Formula, Name) :-
    monoatomic(Formula), 
    cation(Formula),
    get_element(Formula, 0, Element),
    element_name(Element, ElementName),
    get_net_charge(Formula, NetCharge),
    get_ion_part_(NetCharge, "+", ChargeStr),
    string_concat(ElementName, ChargeStr, Name),
    !.

%!  homopolyatomic_cation_cn(+Formula: string, -Name: string) is multi.
%!  homopolyatomic_cation_cn(+Formula: string, +Name: string) is nondet.
%!  homopolyatomic_cation_cn(-Formula: string, +Name: string) is failure.
%
%   IR-5.3.2.3 p.83
%   [O2]+ => dioxygen(1+)
%
homopolyatomic_cation_cn(Formula, Name) :-
    homopolyatomic(Formula),
    cation(Formula),
    get_neutral_specie(Formula, NeutralSpecie),
    compositional(NeutralSpecie, NeutralSpecieName),
    get_net_charge(Formula, NetCharge),
    get_ion_part_(NetCharge, "+", ChargeStr),
    string_concat(NeutralSpecieName, ChargeStr, Name).


anion_cn(Formula, Name) :-
    monoatomic_anion_cn(Formula, Name);
    homopolyatomic_anion_cn(Formula, Name).


%!  monoatomic_anion_cn(+Formula: string, -Name: string) is multi.
%!  monoatomic_anion_cn(+Formula: string, +Name: string) is semidet.
%!  monoatomic_anion_cn(-Formula: string, +Name: string) is ERROR.
%
%   IR-5.3.3.2 p.84-85
%
monoatomic_anion_cn(Formula, Name) :-
    nonvar(Name) ->
        monoatomic_anion_cn_(Formula, Name), !;
    monoatomic_anion_cn_(Formula, Name).
% monoatomic_anion_cn("Cl-", "chloride(1-)").
monoatomic_anion_cn_(Formula, Name) :-
    monoatomic(Formula),
    anion(Formula),
    get_all_elements(Formula, Elements),
    Elements = [Element|_],
    element_name(Element, ElementName),
    append_suffix(ElementName, "ide", IdeName),
    get_net_charge(Formula, NetCharge_),
    abs(NetCharge_, NetCharge),
    get_ion_part_(NetCharge, "-", ChargeStr),
    string_concat(IdeName, ChargeStr, Name).

%!  homopolyatomic_anion_cn(+Formula: string, -Name: string) is multi.
%!  homopolyatomic_anion_cn(+Formula: string, +Name: string) is nondet.
%!  homopolyatomic_anion_cn(-Formula: string, +Name: string) is failure.
%
%   IR-5.3.3.3 p.85
%   [O2]2- => dioxide(2-)
%
homopolyatomic_anion_cn(Formula, Name) :-
    homopolyatomic(Formula),
    anion(Formula),
    % to use append_suffix without error
    get_all_elements(Formula, Elements),
    Elements = [Element|_],
    element_name(Element, EName),
    append_suffix(EName, "ide", IdeName),
    get_neutral_specie(Formula, NeutralSpecie),
    compositional(NeutralSpecie, NeutralSpecieName),
    string_concat(MulPrefix, EName, NeutralSpecieName),
    string_concat(MulPrefix, IdeName, Name_),
    get_net_charge(Formula, NetCharge_),
    abs(NetCharge_, NetCharge),
    get_ion_part_(NetCharge, "-", ChargeStr),
    string_concat(Name_, ChargeStr, Name).


%!  addition_compound_cn(+Formula, +Name) is nondet.
%!  addition_compound_cn(+Formula, -Name) is multi.
%
% # IR-5.5 p.92-93 ← IR-4
% # BF3⋅2H2O ⇒ boron triﬂuoride—water (1/2)
% # 8Kr⋅46H2O ⇒ krypton—water (8/46)
%
addition_compound_cn(Formula, Name) :-
    addition_compound(Formula),
    split_addition_compound(Formula, Compounds, Amounts),
    join("/", Amounts, RatioPart),
    maplist(get_iupac_name_or_addition_compound_exception, Compounds, Names),
    join("\u2014", Names, NamePart),
    join("", [NamePart, " (", RatioPart, ")"], Name).

get_iupac_name_or_addition_compound_exception(Formula, Name) :-
    addition_compound_exception(Formula, Name) -> true;
    iupac_name(Formula, Name).

addition_compound(Formula) :-
    split_addition_compound(Formula, Compounds, _),
    length(Compounds, X),
    X > 1.

split_addition_compound(Formula, Compounds, Amounts) :-
    split_string(Formula, "\u22C5", "", Componenents),
    maplist(re_matchsub("(?<amount>[1-9][0-9]*)?(?<compound>.*)"), Componenents, Matches_),
    maplist(dict_remove_on_cond(value_is_empty_string), Matches_, Matches),
    maplist(get_dict('compound'), Matches, Compounds),
    maplist(get_dict_or_default("1", 'amount'), Matches, AmountStrs),
    maplist(number_string, Amounts, AmountStrs).

re_matchsub(Pattern, String, Sub) :- re_matchsub(Pattern, String, Sub, []).


general_stoichiometric(Formula, Name) :-
    split_generalized_salt_formula(Formula, EPCs, ENCs),
    maplist(cation_name, EPCs, EPCNames),
    maplist(anion_name, ENCs, ENCNames),
    append(EPCNames, ENCNames, PCNames),
    join(" ", EPCNames, Name).

split_generalized_salt_formula(Formula, EPCs, ENCs) :-
    count_atoms(Formula, Atoms),
    append(EPCs_, ENCs_, Atoms),
    (
        selectchk(hydrogen-_, EPCs_, EPCRest) ->
            sort(0, @=<, EPCRest, EPCRest),
            append(EPCRest, [hydrogen-_], EPCs);
        sort(0, @=<, EPCs_, EPCs_)
    ),
    sort(0, @=<, ENCs_, ENCs_),
    EPCs = EPCs_,
    ENCs = ENCs_.

%!  cation_name(+Formula, -Name) is nondet.
%!  cation_name(+Formula, +Name) is nondet.
cation_name(Formula, Name) :-
    cation_cn(Formula, Name);
    substitutive(Formula, Name);
    additive(Formula, Name);
    alternative(Formula, Name).
cation_name(Formula, Name) :-
    (
        not(cation_cn(Formula, Name)),
        monoatomic(Formula),
        get_all_elements(Formula, Elements),
        Elements = [Element|_],
        element_name(Element, Name)
    );
    (
        not(cation_cn(Formula, Name)),
        homopolyatomic(Formula),
        compositional(Formula, Name)
    ).

%!  anion_name(+Formula, -Name) is nondet.
%!  anion_name(+Formula, +Name) is nondet.
anion_name(Formula, Name) :-
    anion_cn(Formula, Name);
    substitutive(Formula, Name);
    additive(Formula, Name);
    alternative(Formula, Name).
anion_name(Formula, Name) :-
    (
        not(anion_cn(Formula, Name)),
        monoatomic(Formula),
        get_all_elements(Formula, Elements),
        Elements = [Element|_],
        element_name(Element, ElementName),
        append_suffix(ElementName, "ide", Name)
    );
    (
        not(anion_cn(Formula, Name)),
        homopolyatomic(Formula),
        get_all_elements(Formula, Elements),
        Elements = [Element|_],
        element_name(Element, EName),
        append_suffix(EName, "ide", IdeName),
        compositional(Formula, CName),
        string_concat(MulPrefix, EName, CName),
        string_concat(MulPrefix, IdeName, Name)
    ).

boron_hydride(Formula) :-
    get_all_elements(Formula,Elements),
    member(boron, Elements),
    member(hydrogen, Elements),
    !.
 
%!  boron_hydride_stoichiometric(+Formula: string, -Name: string) is det.
%!  boron_hydride_stoichiometric(+Formula: string, +Name: string) is det.
%!  boron_hydride_stoichiometric(-Formula: string, +Name: string) is ERROR.
%
%   IR-6.2.3.1
%
boron_hydride_stoichiometric(Formula, Name) :-
    (
        boron_hydride(Formula)
    ),
    get_num_atoms(Formula, boron, NumAtomB),
    get_num_atoms(Formula, hydrogen, NumAtomH),
    multiplicative_prefix(NumAtomB, MulPrefix),
    get_borane_atom_part_(NumAtomH, BoraneAtom),
    string_concat(MulPrefix, BoraneAtom, Name).
 
get_borane_atom_part_(NumAtom,Str) :-
    string_concat("borane(", NumAtom, Str0),
    string_concat(Str0, ")", Str).

