:- module(inorganic,[additive/2,substitutive/2,compositional/2,inorganic_name/2]).

:- use_module(icompositional,[boron_hydride_stoichiometric/2,general_stoichiometric/2,addition_compound_cn/2,ion_cn/2,binary_compound_cn/2,homonuclear_cn/2]).
:- use_module(isubstitutive,[parent_hydride_anion_sn/2,parent_hydride_cation_sn/2,parent_hydride_sn/2]).
:- use_module(iadditive,[polynulcear_entity_an/2,mononuclear_entity_an/2]).
:- use_module(ialternative,[alternative/2]).


inorganic_name(Formula, Name) :-
    compositional(Formula, Name);
    substitutive(Formula, Name);
    additive(Formula, Name);
    alternative(Formula, Name).

compositional(Formula, Name) :-
    stoichiometric(Formula, Name);
    homonuclear_cn(Formula, Name);
    binary_compound_cn(Formula, Name);
    ion_cn(Formula, Name);
    addition_compound_cn(Formula, Name).

stoichiometric(Formula, Name) :-
    general_stoichiometric(Formula, Name);
    boron_hydride_stoichiometric(Formula, Name).

substitutive(Formula, Name) :-
    parent_hydride_sn(Formula, Name);
    parent_hydride_cation_sn(Formula, Name);
    parent_hydride_anion_sn(Formula, Name).

additive(Formula, Name) :-
    mononuclear_entity_an(Formula, Name);
    polynulcear_entity_an(Formula, Name).
