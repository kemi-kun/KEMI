:- module(icompositional,[boron_hydride_stoichiometric/2,general_stoichiometric/2,addition_compound_cn/2,ion_cn/2,binary_compound_cn/2,homonuclear_cn/2]).

:- use_module(nomenclature,[iupac_name/2]).
:- use_module(inorganic,[additive_name/2,substitutive_name/2,compositional_name/2]).
:- use_module(elements,[element_name/2,element_symbol/2,group/2]).
:- use_module(ialternative,[alternative_name/2]).

:- use_module(facts).
:- use_module(support).
:- use_module(uchem).
:- use_module(ucollections).
:- use_module(unums).
:- use_module(ustr).


ion(Formula) :- cation(Formula) -> true; anion(Formula).

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

%!  homonuclear_cn(+Formula:string, +Name:string) is semidet.
%!  homonuclear_cn(+Formula:string, -Name:string) is semidet.
%!  homonuclear_cn(-Formula:string, -Name:string) is multi.
%!  homonuclear_cn(-Formula:string, +Name:string) is semidet.
%
%   True when `Name` is a compostional homonuclear name for the homonuclear 
%   compound represented by `Formula`. False otherwise.
%
homonuclear_cn(Formula, Name) :-
    nonvar(Name) ->
        homonuclear_name_atom(Name, Atom),
        homonuclear_formula_atom(Formula, Atom);
    homonuclear_formula_atom(Formula, Atom),
    homonuclear_name_atom(Name, Atom),
    % chcek formula
    homonuclear(Formula),
    not(ion(Formula)).


%!  homonuclear_name_atom(?Name:string, ?Element:atom, ?Amount:int) is det/multi.
%
%   True when `Atom = Element-Amount` is the atoms in the formula representted by `Name`.
%   False when `Name` is not a compositional homonuclear compound name.
%
homonuclear_name_atom(Name, Element-Amount) :-
    nonvar(Name) ->
        homonuclear_name_atom_(Name, Element-Amount), !;
    nonvar(Element), nonvar(Amount) ->
        homonuclear_name_atom_(Name, Element-Amount), !;
    homonuclear_name_atom_(Name, Element-Amount).

homonuclear_name_atom_(Name, Element-Amount) :-
    between(1, 9999, Amount),
    (
        element_name(Element, ElementName);
        alternative_element_name(Element, ElementName)
    ),
    (
        Amount = 1, group(Element, 18) -> MulPrefix = "";
        multiplicative_prefix(Amount, MulPrefix)
    ),
    prepend_prefix(ElementName, MulPrefix, Name).


%!  homonuclear_formula_atom(?Formula:string, ?Element:atom, ?Amount:int) is det/multi.
%
homonuclear_formula_atom(Formula, Element-Amount) :-
    nonvar(Formula) ->
        split_symbol_num(Formula, Symbol, Amount),
        element_symbol(Element, Symbol);
    between(1, infinite, Amount),
    element_symbol(Element, Symbol),
    split_symbol_num(Formula, Symbol, Amount).

split_symbol_num(String, Symbol, Num) :-
    nonvar(String) ->
        re_matchsub("(?<symbol>[A-z]*)(?<numstr>[0-9]*)?", String, Sub, []),
        get_dict(symbol, Sub, Symbol),
        get_dict(numstr, Sub, NumStr),
        NumStr \= "1",
        (number_string(Num, NumStr) -> true; Num is 1);
    nonvar(Symbol), nonvar(Num) ->
        (
            Num = 1 -> String = Symbol;
            string_concat(Symbol, Num, String)
        ).


%
%
%
%
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
    nonvar(Formula),
    (
        binary_compound(Formula),
        not(ion(Formula))
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

%!  ion_cn(+Formula: string, +Name: string) is semidet.
%!  ion_cn(+Formula: string, -Name: string) is semidet.
%!  ion_cn(-Formula: string, +Name: string) is failure.
%!  ion_cn(-Formula: string, -Name: string) is failure.
%
%   @arg Formula – the chemical formula of the cationic or anionic compound
%        Name – the compositional name of the cationic or anionic compound
%
ion_cn(Formula, Name) :-
    nonvar(Formula),
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
    compositional_name(NeutralSpecie, NeutralSpecieName),
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
    compositional_name(NeutralSpecie, NeutralSpecieName),
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
    nonvar(Formula),
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
    nonvar(Formula),
    ion(Formula) ->
        general_stoichiometric_ion(Formula, Name);
    general_stoichiometric_(Formula, Name).

general_stoichiometric_(Formula, Name) :-
        split_generalized_salt_formula(Formula, EPCs, ENCs),
        maplist(homonuclear_formula_atom, EPCs, EPCFormulas),
        maplist(homonuclear_formula_atom, ENCs, ENCFormulas),
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
    substitutive_name(Formula, Name);
    additive_name(Formula, Name);
    alternative_name(Formula, Name).
cation_name(Formula, Name) :-
    (
        monoatomic(Formula),
        get_all_elements(Formula, Elements),
        Elements = [Element|_],
        element_name(Element, Name)
    );
    (
        homopolyatomic(Formula),
        compositional_name(Formula, Name)
    ).

%!  anion_name(+Formula, -Name) is nondet.
%!  anion_name(+Formula, +Name) is nondet.
anion_name(Formula, Name) :-
    substitutive_name(Formula, Name);
    additive_name(Formula, Name);
    alternative_name(Formula, Name).
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
        compositional_name(Formula, CName),
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

