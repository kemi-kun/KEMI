:- module(icompositional,[boron_hydride_stoichiometric/2,general_stoichiometric/2,addition_compound_cn/2,ion_cn/2,binary_compound_cn/2,homonuclear_cn/2]).

:- use_module(elements,[element_name/2,element_symbol/2,group/2]).
:- use_module(facts,[addition_compound_exception/2,alternative_element_name/2]).
:- use_module(nomenclature,[iupac_name/2]).
:- use_module(predicate,[append_suffix/3]).
:- use_module(inorganic,[additive/2,substitutive/2,compositional/2]).
:- use_module(support,[get_neutral_specie/2,multiplicative_prefix/2,mul_prefix_except_mono/2]).
:- use_module(uchem,[count_atoms/2,get_net_charge/2,get_num_atoms/3,get_num_elements/2,get_all_elements/2,get_element/3]).
:- use_module(utils,[value_is_empty_string/1,dict_remove_on_cond/3,get_dict_or_default/4]).
:- use_module(ustr,[join/3]).
:- use_module(ialternative,[alternative/2]).


ionic(Formula) :- cation(Formula) -> true; anion(Formula).

cation(Formula) :-
    get_net_charge(Formula, NetCharge),
    NetCharge > 0.

anion(Formula) :-
    get_net_charge(Formula, NetCharge),
    NetCharge < 0.

monoatomic(Formula) :-
    count_atoms(Formula, Atoms),
    length(Atoms, 1),
    nth0(0, Atoms, _Element-Amount),
    Amount is 1.

homopolyatomic(Formula) :-
    count_atoms(Formula, Atoms),
    length(Atoms, 1),
    nth0(0, Atoms, _Element-Amount),
    Amount > 1.

heteropolyatomic(Formula) :-
    count_atoms(Formula, Atoms),
    length(Atoms, X),
    X > 1.


get_charge_str(Formula, ChargeStr) :-
    get_net_charge(Formula, NetCharge),
    NetCharge > 0 -> get_ion_part_(NetCharge, "+", ChargeStr),
    NetCharge < 0 -> get_ion_part_(NetCharge, "-", ChargeStr);
    fail.

get_ion_part_(NetCharge, IonSign, ChargeStr) :-
    abs(NetCharge, NC),
    string_concat("(", NC, ChargeStr1),
    string_concat(ChargeStr1, IonSign, ChargeStr2),
    string_concat(ChargeStr2, ")", ChargeStr).


% # IR-5.2 p.81
% # IR-3.4.3 p.61
% # S8 ⇒ octasulfur

%!  homonuclear_cn(+Formula:string, -Name:string) is det.
%
%   True when `Name` is the IUPAC name for `Formula`.
%
homonuclear_cn(Formula, Name) :-
    nonvar(Name) ->
        homonuclear_name_atom(Name, Atom),
        homonuclear_atom_formula(Atom, Formula);
    homonuclear_cn_(element_name, Formula, Name).

homonuclear_cn_(ElementNameFunction, Formula, Name) :-
    % check
    homonuclear(Formula),
    not(ionic(Formula)),

    get_all_elements(Formula, Elements),
    memberchk(Element, Elements),
    get_num_atoms(Formula, Element, Amount),
    call(ElementNameFunction, Element, ElementName),
    (
        group(Element, 18) -> mul_prefix_except_mono(Amount, MulPrefix);
        multiplicative_prefix(Amount, MulPrefix)
    ),
    string_concat(MulPrefix, ElementName, Name).
 
homonuclear(Formula) :-
    get_all_elements(Formula, Elements),
    length(Elements,1).

homonuclear_name_atom(Name, Atom) :-
    (
        alternative_element_name(Element, ElementName);
        element_name(Element, ElementName)
    ),
    string_concat(MulPrefix, ElementName, Name),
    (
        group(Element, 18) -> mul_prefix_except_mono(Amount, MulPrefix);
        multiplicative_prefix(Amount, MulPrefix)
    ),
    Atom = Element-Amount,
    !.

%!  homonuclear_atom_formula(+Atom:list(Element-Amount), Formula:string) is det.
%
homonuclear_atom_formula(Atom, Formula) :-
    Atom = Element-Amount,
    element_symbol(Element, Symbol),
    (
        Amount = 1 -> Formula = Symbol;
            number_string(Amount, AmountStr),
            string_concat(Symbol, AmountStr, Formula)
    ).


%!  binary_compound_cn(+Formula: string, -Name: string) is det.
%!  binary_compound_cn(+Formula: string, +Name: string) is det.
%!  binary_compound_cn(-Formula: string, +Name: string) is ERROR.
%
%   IR-5.2 p.81-82
%
binary_compound_cn(Formula, Name) :-
    (
        binary_compound(Formula),
        not(ionic(Formula))
    ),
    get_element(Formula, 0, EPosElement),
    get_element(Formula, 1, ENegElement),
    element_name(EPosElement,EPosElementName),
    element_name(ENegElement,ENegElementName),
    append_suffix(ENegElementName, "ide", NegativePart),
    get_num_atoms(Formula,EPosElement, NumPositive),
    mul_prefix_except_mono(NumPositive, EPosMulPrefix),
    get_num_atoms(Formula, ENegElement,  NumNegative),
    multiplicative_prefix(NumNegative, ENegMulPrefixEx),
    string_concat(EPosMulPrefix, EPosElementName, EPos),
    string_concat(EPos, " ", EPos_),
    string_concat(ENegMulPrefixEx, NegativePart, ENeg),
    string_concat(EPos_, ENeg, Name),
    !.
 
binary_compound(Formula) :-
    get_all_elements(Formula, Elements),
    length(Elements, 2),
    !.


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

%
%
%
%
%
%
%
general_stoichiometric(Formula, Name) :-
    ionic(Formula) ->
        general_stoichiometric_ion(Formula, Name);
    general_stoichiometric_(Formula, Name).

general_stoichiometric_(Formula, Name) :-
        split_generalized_salt_formula(Formula, EPCs, ENCs),
        maplist(homonuclear_atom_formula, EPCs, EPCFormulas),
        maplist(homonuclear_atom_formula, ENCs, ENCFormulas),
        maplist(cation_name, EPCFormulas, EPCNames),
        maplist(anion_name, ENCFormulas, ENCNames),
        append(EPCNames, ENCNames, PCNames),
        join(" ", PCNames, Name).

general_stoichiometric_ion(Formula, Name) :-
    get_neutral_specie(Formula, NeutralSpecie),
    general_stoichiometric_(NeutralSpecie, NeutralName),
    get_charge_str(Formula, ChargeStr),
    join("", ["(", NeutralName, ")", ChargeStr], Name).

%!  split_generalized_salt_formula(+Formula:string, -EPCs:list(Element-Amount), -ENCs:list(Element-Amount)) is mutli.
%
split_generalized_salt_formula(Formula, EPCs, ENCs) :-
    count_atoms(Formula, Atoms),
    append(EPCs_, ENCs_, Atoms),
    EPCs_ \= [], ENCs_ \= [],
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
    substitutive(Formula, Name);
    additive(Formula, Name);
    alternative(Formula, Name).
cation_name(Formula, Name) :-
    (
        monoatomic(Formula),
        get_all_elements(Formula, Elements),
        Elements = [Element|_],
        element_name(Element, Name)
    );
    (
        homopolyatomic(Formula),
        compositional(Formula, Name)
    ).

%!  anion_name(+Formula, -Name) is nondet.
%!  anion_name(+Formula, +Name) is nondet.
anion_name(Formula, Name) :-
    substitutive(Formula, Name);
    additive(Formula, Name);
    alternative(Formula, Name).
anion_name(Formula, Name) :-
    (
        monoatomic(Formula),
        get_all_elements(Formula, Elements),
        Elements = [Element|_],
        element_name(Element, ElementName),
        append_suffix(ElementName, "ide", Name)
    );
    (
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
    Elements = [boron, hydrogen].
 
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

