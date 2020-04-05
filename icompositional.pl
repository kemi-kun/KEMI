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
%   @arg Formula – the chemical formula of the homonuclear molecule
%   @arg Name –  the stoichiometric name of the homonuclear molecule
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
    string_concat(MulPrefix, ElementName, Name).

%!  homonuclear_formula_atom(?Formula:string, ?Element:atom, ?Amount:int) is det/multi.
%
homonuclear_formula_atom(Formula, Element-Amount) :-
    nonvar(Formula) ->
        split_term(Formula, Symbol, Amount),
        element_symbol(Element, Symbol);
    between(1, infinite, Amount),
    element_symbol(Element, Symbol),
    split_term(Formula, Symbol, Amount).

split_term(String, Symbol, Amount) :-
    nonvar(String) ->
        re_matchsub("(?<symbol>[A-z]*)(?<numstr>[0-9]*)?", String, Sub, []),
        get_dict(symbol, Sub, Symbol),
        get_dict(numstr, Sub, NumStr),
        NumStr \= "1",
        (number_string(Amount, NumStr) -> true; Amount is 1);
    nonvar(Symbol), nonvar(Amount) ->
        (
            Amount = 1 -> String = Symbol;
            string_concat(Symbol, Amount, String)
        ).


%!  homonulcear(+Formula) is semidet.
%!  homonulcear(-Formula) is failure.
%
homonuclear(Formula) :-
    get_all_elements(Formula, Elements),
    length(Elements,1).


%!  binary_compound_cn(+Formula: string, +Name: string) is semidet.
%!  binary_compound_cn(+Formula: string, -Name: string) is semidet.
%!  binary_compound_cn(-Formula: string, -Name: string) is failure.
%!  binary_compound_cn(-Formula: string, +Name: string) is semidet.
%
%   @arg Formula – the chemical formula of the binary compound
%   @arg Name – the stoichiometric name of the binary compound
%   
%   IR-5.2 p.81-82
%
binary_compound_cn(Formula, Name) :-
    nonvar(Name) ->
        binary_compound_name_atoms(Name, Atom),
        binary_compound_formula_atoms(Formula, Atom);
    binary_compound_formula_atoms(Formula, Atom),
    binary_compound_name_atoms(Name, Atom),
    % chcek formula
    binary_compound(Formula),
    not(ion(Formula)).


%!  binary_compound(+Formula:string) is semidet.
%!  binary_compound(-Formula:string) is failure.
%
binary_compound(Formula) :-
    get_all_elements(Formula, Elements),
    length(Elements, 2).


%!  binary_compound_formula_atoms(+Formula:string, +Atoms:list(Element:atom-Amount:int) is semidet.
%!  binary_compound_formula_atoms(+Formula:string, -Atoms:list(Element:atom-Amount:int) is semidet.
%!  binary_compound_formula_atoms(-Formula:string, +Atoms:list(Element:atom-Amount:int) is det.
%!  binary_compound_formula_atoms(-Formula:string, -Atoms:list(Element:atom-Amount:int) is failure.
%
%
%   Note: doesn't check if `Formula` is in the correct form.
%
binary_compound_formula_atoms(Formula, Atoms) :-
    nonvar(Formula) ->
        count_atoms(Formula, Atoms);
    nonvar(Atoms) ->
        length(Atoms, 2),
        maplist(homonuclear_formula_atom, Terms, Atoms),
        join("", Terms, Formula);
    fail.


%!  binary_compound_name_atoms(+Name:string, +Atoms:list(Element:atom-Amount:int) is semidet.
%!  binary_compound_name_atoms(+Name:string, -Atoms:list(Element:atom-Amount:int) is semidet.
%!  binary_compound_name_atoms(-Name:string, +Atoms:list(Element:atom-Amount:int) is det.
%!  binary_compound_name_atoms(-Name:string, -Atoms:list(Element:atom-Amount:int) is failure.
%
%   True when `Atoms` is the list of `Element-Amount` represented by `Name`.
%   False when `Name` is not a stoichiometric name of binary compound.
%
binary_compound_name_atoms(Name, Atoms) :-
    nonvar(Name) ->
        binary_compound_name_atoms_(Name, Atoms);
    nonvar(Atoms) ->
        binary_compound_atoms_name_(Atoms, Name);
    fail.

binary_compound_name_atoms_(Name, Atoms) :-
    split_positive_negative(Name, PositivePart, NegativePart),
    electropositive_name_atom(PositivePart, PosElement-PosAmount),
    electronegative_name_atom(NegativePart, NegElement-NegAmount),
    Atoms = [PosElement-PosAmount, NegElement-NegAmount].

binary_compound_atoms_name_(Atoms, Name) :-
    Atoms = [PosElement-PosAmount, NegElement-NegAmount],
    electropositive_name_atom(PositivePart, PosElement-PosAmount),
    electronegative_name_atom(NegativePart, NegElement-NegAmount),
    split_positive_negative(Name, PositivePart, NegativePart).


%!  split_positive_negative(-Name:string, +PositivePart:string, +NegativePart) is semidet.
split_positive_negative(Name, PositivePart, NegativePart) :-
    nonvar(PositivePart), nonvar(NegativePart),
    string_concat(PositivePart, " ", T),
    string_concat(T, NegativePart, Name),
    !.

%!  split_positive_negative(+Name:string, -PositivePart:string, -NegativePart) is semidet.
split_positive_negative(Name, PositivePart, NegativePart) :-
    nonvar(Name),
    re_matchsub("(?<positive>[a-z]+) (?<negative>[a-z]+)", Name, Sub, []),
    get_dict(positive, Sub, PositivePart),
    get_dict(negative, Sub, NegativePart).


%!  electropositive_name_atom(+Name:string, +Element:atom-Amount:int) is semidet.
%!  electropositive_name_atom(+Name:string, -Element:atom-Amount:int) is semidet.
%!  electropositive_name_atom(-Name:string, -Element:atom-Amount:int) is multi.
%!  electropositive_name_atom(-Name:string, +Element:atom-Amount:int) is det.
electropositive_name_atom(Name, Element-Amount) :-
    nonvar(Name) ->
        electropositive_name_atom_(Name, Element-Amount), !;
    nonvar(Element), nonvar(Amount) ->
        electropositive_name_atom_(Name, Element-Amount), !;
    electropositive_name_atom_(Name, Element-Amount).

electropositive_name_atom_(Name, Element-Amount) :-
    between(1, 9999, Amount),
    element_name(Element, ElementName),
    (
        Amount = 1 -> MulPrefix = "";
        multiplicative_prefix(Amount, MulPrefix)
    ),
    string_concat(MulPrefix, ElementName, Name).


%!  electronegative_name_atom(+Name:string, +Element:atom-Amount:int) is semidet.
%!  electronegative_name_atom(+Name:string, -Element:atom-Amount:int) is semidet.
%!  electronegative_name_atom(-Name:string, -Element:atom-Amount:int) is multi.
%!  electronegative_name_atom(-Name:string, +Element:atom-Amount:int) is det.
electronegative_name_atom(Name, Element-Amount) :-
    nonvar(Name) ->
        electronegative_name_atom_loose_(Name, Element-Amount), !;
    nonvar(Element), nonvar(Amount) ->
        electronegative_name_atom_(Name, Element-Amount), !;
    electronegative_name_atom_(Name, Element-Amount).

electronegative_name_atom_(Name, Element-Amount) :-
    between(1, 9999, Amount),
    element_name(Element, ElementName),
    append_suffix(ElementName, "ide", IdeName),
    (
        Amount = 1, Element \= oxygen -> MulPrefix = "";
        multiplicative_prefix(Amount, MulPrefix)
    ),
    prepend_prefix(IdeName, MulPrefix, Name).

electronegative_name_atom_loose_(Name, Element-Amount) :-
    between(1, 9999, Amount),
    element_name(Element, ElementName),
    append_suffix(ElementName, "ide", IdeName),
    (
        Amount = 1, Element \= oxygen, MulPrefix = "";
        multiplicative_prefix(Amount, MulPrefix)
    ),
    prepend_prefix(IdeName, MulPrefix, Name).


%!  ion_cn(+Formula: string, +Name: string) is semidet.
%!  ion_cn(+Formula: string, -Name: string) is semidet.
%!  ion_cn(-Formula: string, +Name: string) is failure.
%!  ion_cn(-Formula: string, -Name: string) is failure.
%
%   @arg Formula – the chemical formula of the cationic or anionic compound
%   @arg Name – the compositional name of the cationic or anionic compound
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
    (
        nonvar(Name) ->
            homoatomic_ion_name_atom(Name, Element-1, Charge),
            homoatomic_ion_formula_atom(Formula, Element-1, Charge);
        nonvar(Formula) ->
            homoatomic_ion_formula_atom(Formula, Element-1, Charge),
            homoatomic_ion_name_atom(Name, Element-1, Charge)
    ),
    % chcek formula
    monoatomic(Formula),
    cation(Formula).
% monoatomic_cation_cn("[Na]+", "sodium(1+)")


%!  homoatomic_ion_formula_atom(+Formula:string, +Element:atom-Amount:int, +Charge:int) is semidet.
%!  homoatomic_ion_formula_atom(+Formula:string, -Element:atom-Amount:int, -Charge:int) is semidet.
%!  homoatomic_ion_formula_atom(-Formula:string, -Element:atom-Amount:int, -Charge:int) is failure.
%!  homoatomic_ion_formula_atom(-Formula:string, +Element:atom-Amount:int, +Charge:int) is det.
%
homoatomic_ion_formula_atom(Formula, Element-Amount, Charge) :-
    nonvar(Formula) ->
        count_atoms(Formula, Atoms),
        Atoms = [Element-Amount],
        get_net_charge(Formula, Charge);
    nonvar(Element), nonvar(Amount), nonvar(Charge) ->
        homoatomic_ion_atom_formula_(Element-Amount, Charge, Formula);
    fail.

%   homoatomic_ion_atom_formula_(+Element-Amount, +Charge, -Formula) is det.
homoatomic_ion_atom_formula_(Element-Amount, Charge, Formula) :-
    homonuclear_formula_atom(Term, Element-Amount),
    (
        Charge = 1 ->
            ChargePart = "+";
        Charge = -1 ->
            ChargePart = "-";
        charge_string(ChargePart, Charge)
    ),
    join("", ["(", Term, ")", ChargePart], Formula).


%!  homoatomic_ion_name_atom(+Name:string, +Element:atom-Amount:int, +Charge:int) is semidet.
%!  homoatomic_ion_name_atom(+Name:string, -Element:atom-Amount:int, -Charge:int) is semidet.
%!  homoatomic_ion_name_atom(-Name:string, -Element:atom-Amount:int, -Charge:int) is failure.
%!  homoatomic_ion_name_atom(-Name:string, +Element:atom-Amount:int, +Charge:int) is det.
%
homoatomic_ion_name_atom(Name, Element-Amount, Charge) :-
    nonvar(Name) ->
        homoatomic_ion_name_atom_(Name, Element-Amount, Charge);
    nonvar(Element), nonvar(Amount), nonvar(Charge) ->
        homoatomic_ion_atom_name_(Element-Amount, Charge, Name), !.

homoatomic_ion_name_atom_(Name, Element-Amount, Charge) :-
    nonvar(Name),
    re_matchsub("(?<name_part>[a-z]+)\\((?<charge_part>[1-9][0-9]*[+-])\\)", Name, Sub, []),
    get_dict(name_part, Sub, NamePart),
    get_dict(charge_part, Sub, ChargePart),
    charge_string(ChargePart, Charge),
    (
        Charge > 0 ->
            electropositive_name_atom(NamePart, Element-Amount);
        Charge < 0 ->
            electronegative_name_atom(NamePart, Element-Amount)
    ).

homoatomic_ion_atom_name_(Element-Amount, Charge, Name) :-
    between(1, infinite, Amount),
    MaxCharge is 9 * Amount,        % +9, -5
    between(1, MaxCharge, Charge_),
    (
        Charge is Charge_ ->
            electropositive_name_atom(NamePart, Element-Amount);
        Charge is -Charge_ ->
            electronegative_name_atom(NamePart, Element-Amount)
    ),
    charge_string(ChargePart, Charge),
    join("", [NamePart, "(", ChargePart, ")"], Name).


charge_string(ChargePart, Charge) :-
    nonvar(Charge),
    Charge_ is abs(Charge),
    number_string(Charge_, NumStr),
    (
        Charge > 0 ->
            string_concat(NumStr, "+", ChargePart);
        Charge < 0 ->
            string_concat(NumStr, "-", ChargePart)
    ), !.

charge_string(ChargePart, Charge) :-
    nonvar(ChargePart),
    re_matchsub("(?<num>[1-9][0-9]*)(?<sign>[+-])", ChargePart, Sub, []),
    get_dict(num, Sub, NumStr),
    get_dict('sign', Sub, Sign),
    (
        Sign = "+" ->
            number_string(Charge, NumStr);
        Sign = "-" ->
            number_string(Num_, NumStr),
            Charge is -Num_
    ).

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


%!  addition_compound_cn(+Formula: string, +Name: string) is semidet.
%!  addition_compound_cn(+Formula: string, -Name: string) is semidet.
%!  addition_compound_cn(-Formula: string, +Name: string) is failure.
%!  addition_compound_cn(-Formula: string, -Name: string) is failure.
%
% # IR-5.5 p.92-93 ← IR-4
% # BF3⋅2H2O ⇒ boron triﬂuoride—water (1/2)
% # 8Kr⋅46H2O ⇒ krypton—water (8/46)
%
%   @arg Formula – the chemical formula of the addition compound
%   @arg Name – the compositional name of the addition compound
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


%!  general_stoichiometric(+Formula: string, +Name: string) is semidet.
%!  general_stoichiometric(+Formula: string, -Name: string) is semidet.
%!  general_stoichiometric(-Formula: string, +Name: string) is failure.
%!  general_stoichiometric(-Formula: string, -Name: string) is failure.
%
%   @arg Formula – the generalized salt formula
%   @arg Name –  the stoichiometric name of the generalized salt formula
%
general_stoichiometric(Formula, Name) :-
    nonvar(Formula),
    ion(Formula) ->
        general_stoichiometric_ion(Formula, Name);
    general_stoichiometric_(Formula, Name).

general_stoichiometric_(Formula, Name) :-
        split_generalized_salt_formula(Formula, EPCs, ENCs),
        maplist(homonuclear_formula_atom, EPCFormulas, EPCs),
        maplist(homonuclear_formula_atom, ENCFormulas, ENCs),
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

