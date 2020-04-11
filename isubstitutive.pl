:- module(isubstitutive,[
    boron_hydride_structural_descriptor_name/2,
    parent_hydride_anion_sn/2,
    parent_hydride_cation_sn/2,
    parent_hydride_sn/2
    ]).

:- use_module(library(pcre)).

:- use_module(elements).
:- use_module(facts).
:- use_module(uchem).
:- use_module(ustr).
:- use_module(support).

:- use_module(icompositional, [boron_hydride_stoichiometric_name/2]).


parent_hydride_sn(Formula, Name) :-
    (
        mononuclear_parent_hydride_sn(Formula, Name) -> true;
        homopolynuclear_parent_hydride_sn(Formula, Name) -> true;
        heteronuclear_parent_hydride_sn(Formula, Name)
    ).


mononuclear_parent_hydride_sn(Formula, Name) :-
    nonvar(Formula) ->
        mononuclear_parent_hydride_formula_name(Formula, Name);
    nonvar(Name) ->
        mononuclear_parent_hydride_name_atoms(Name, Atoms),
        mononuclear_parent_hydride_atoms_formula(Atoms, Formula);
    fail.


mononuclear_parent_hydride_formula_name(Formula, Name) :-
    count_atoms(Formula, Atoms),
    selectchk(hydrogen-NumHydrogen, Atoms, [Element-1]),
    get_standard_bonding_number(Element, SBN),
    parent_name_of(Element, ParentName),
    (
        SBN = NumHydrogen ->
            % Standard Bonding Number
            Name = ParentName;

        % Non-Standard Bonding Number
        join("", ["\u03bb", NumHydrogen, "-", ParentName], Name)
    ).


mononuclear_parent_hydride_atoms_formula(Atoms, Formula) :-
    nonvar(Atoms),
    selectchk(hydrogen-NumHydrogen, Atoms, [Element-1]),
    element_symbol(Element, ElementSymbol),
    join("", [ElementSymbol, "H", NumHydrogen], Formula).


mononuclear_parent_hydride_name_atoms(Name, Atoms) :-
    nonvar(Name) ->
    (
        % Non-Standard Bonding Number
        re_matchsub("^\u03bb(?<num>[1-9][0-9]*)-(?<parent_name>[a-z]+)$", Name, Sub, []) ->
            get_dict(num, Sub, NumHydrogen),
            get_dict(parent_name, Sub, ParentName),
            parent_name_of(Element, ParentName),
            Atoms = [hydrogen-NumHydrogen, Element-1];

        % Standard Bonding Number
        parent_name_of(Element, Name),
        get_standard_bonding_number(Element, SBN),
        Atoms = [hydrogen-SBN, Element-1]
    ).
    % generate
    %
    % element_fact(Element, _, _, _, _),
    % get_standard_bonding_number(Element, SBN),
    % parent_name_of(Element, ParentName),
    % (
    %     ParentName = Name ->
    %         Atoms = [hydrogen-SBN, Element-1];
    % ).


mononuclear_parent_hydride(Formula) :-
    count_atoms(Formula, [hydrogen-NumHydrogen, Element-NumElement]),
    element_fact(Element, _, _, _, _),
    between(1, infinite, NumHydrogen),
    between(1, infinite, NumElement),
    Element \= boron,
    Element \= carbon,
    group(Element, X),
    between(13, 17, X).


parent_name_of(Element, Name) :-
    (
        parent_name_exception(Element, _, Name) -> true;
            element_name(Element, ElementName),
            append_suffix(ElementName, "ane", Name)
    ).
    % % check
    % ExcludedNames = [
    %     "carbane", "aluminane", "bismane", "oxane", "thiane", 
    %     "selenane", "tellurane", "polonane"
    % ],
    % not(member(Name, ExcludedNames)).


homopolynuclear_parent_hydride_sn(Formula, Name) :-
    fail.

heteronuclear_parent_hydride_sn(Formula, Name) :-
    fail.



parent_hydride_ion_sn(Formula, Name) :-
    (
        parent_hydride_cation_sn(Formula, Name) -> true;
        parent_hydride_anion_sn(Formula, Name)
    ).


parent_hydride_cation_sn(Formula, Name) :-
    fail.

parent_hydride_anion_sn(Formula, Name) :-
    fail.


%!  boron_hydride_structural_descriptor_name(+Formula:string, +Name:string) is semidet.
%!  boron_hydride_structural_descriptor_name(+Formula:string, -Name:string) is semidet.
%!  boron_hydride_structural_descriptor_name(-Formula:string, -Name:string) is failure.
%!  boron_hydride_structural_descriptor_name(-Formula:string, +Name:string) is semidet.
%
boron_hydride_structural_descriptor_name(Formula, Name) :-
    nonvar(Formula) ->
        boron_hydride_stoichiometric_name(Formula, StoiName),
        count_atoms(Formula, [boron-NumBoron, hydrogen-NumHydrogen]),
        boron_hydride_structural_descriptor(NumBoron, NumHydrogen, Descriptor_),
        string_concat(Descriptor_, "-", Descriptor),
        string_concat(Descriptor, StoiName, Name);
    nonvar(Name) ->
        re_matchsub("^(?<descriptor>[a-z]+)-(?<stoi_name>.*)$", Name, Sub, []),
        get_dict(stoi_name, Sub, StoiName),
        boron_hydride_stoichiometric_name(Formula, StoiName),
        count_atoms(Formula, [boron-NumBoron, hydrogen-NumHydrogen]),
        get_dict(descriptor, Sub, Descriptor),
        boron_hydride_structural_descriptor(NumBoron, NumHydrogen, Descriptor);
    fail.


%!  boron_hydride_structural_descriptor(+NumBoron:int, +NumHydrogen:int, -StructuralDescriptor:string) is semidet.
%!  boron_hydride_structural_descriptor(?NumBoron:int, ?NumHydrogen:int, -StructuralDescriptor:string) is multi.
%!  boron_hydride_structural_descriptor(?NumBoron:int, ?NumHydrogen:int, +StructuralDescriptor:string) is semidet.
%!  boron_hydride_structural_descriptor(-NumBoron:int, -NumHydrogen:int, +StructuralDescriptor:string) is nondet.
%
boron_hydride_structural_descriptor(NumBoron, NumHydrogen, StructuralDescriptor) :-
    maplist(nonvar_, [NumBoron, NumHydrogen, StructuralDescriptor], NonVars),
    foldl(plus_, NonVars, 0, V),
    (
        V >= 2 -> boron_hydride_structural_descriptor_(NumBoron, NumHydrogen, StructuralDescriptor), !;
        boron_hydride_structural_descriptor_(NumBoron, NumHydrogen, StructuralDescriptor)
    ).

nonvar_(A, B) :- B = nonvar(A).
plus_(A, V0, V1) :-
    A -> V1 is V0 + 1;
    V1 is V0.

%!  boron_hydride_structural_descriptor_(?NumBoron:int, ?NumHydrogen:int, ?StructuralDescriptor:string) is nondet.
boron_hydride_structural_descriptor_(NumBoron, NumHydrogen, StructuralDescriptor) :-
    (nonvar(NumHydrogen) -> A is NumHydrogen-10, B is NumHydrogen-2, between(A, B, NumBoron); true),
    between(1, infinite, NumBoron),
    (
        X = 2,  StructuralDescriptor = "closo";
        X = 4,  StructuralDescriptor = "nido";
        X = 6,  StructuralDescriptor = "arachno";
        X = 8,  StructuralDescriptor = "hypho";
        X = 10, StructuralDescriptor = "klado"
    ),
    plus(NumBoron, X, NumHydrogen).
