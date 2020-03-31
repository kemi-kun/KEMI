/**
Tests functions in elements.pl
*/
:- begin_tests(test_elements).
:- use_module(elements,[atoms/2,contains/2]).

test('[elements.pl] Generic') :-
    assertion(contains("NaCl", [atoms("Na", 1), atoms("Cl", 1)])),
    assertion(contains("S8", [atoms("S", 8)])).

test('[elements.pl] Red book IR-5.2') :-
    assertion(contains("HCl", [atoms("H", 1), atoms("Cl", 1)])),
    assertion(contains("NO", [atoms("N", 1), atoms("O", 1)])),
    assertion(contains("NO2", [atoms("N", 1), atoms("O", 2)])),
    assertion(contains("N2O4", [atoms("N", 2), atoms("O", 4)])),
    assertion(contains("OCl2", [atoms("O", 1), atoms("Cl", 2)])),
    assertion(contains("O2Cl", [atoms("O", 2), atoms("Cl", 1)])),
    assertion(contains("Fe3O4", [atoms("Fe", 3), atoms("O", 4)])),
    assertion(contains("SiC", [atoms("Si", 1), atoms("C", 1)])),
    assertion(contains("SiCl4", [atoms("Si", 1), atoms("Cl", 4)])),
    assertion(contains("Ca3P2", [atoms("Ca", 3), aoms("P", 2)])),
    assertion(contains("NiSn", [atoms("Ni", 1), atoms("Sn", 1)])),
    assertion(contains("Cu5Zn8", [atoms("Cu", 5), atoms("Zn", 8)])),
    assertion(contains("Cr23C6", [atoms("Cr", 23), atoms("C", 6)])).

test('[elements.pl] enclosing marks IR-2.2') :-
    % assertion(contains("[Rh3Cl(μ-Cl)(CO)3{μ3-Ph2PCH2P(Ph)CH2PPh2}2]+", [atoms("Rh", 13), atoms("Cl", 2), atoms("C", 7), atoms("O", 3), atoms("Ph", 10), atoms("P", 6), atoms("H", 8)])),
    assertion(contains("[Fe(η5-C5H5)2]", [atoms("Fe", 1), atoms("C", 10), atoms("H", 10)])),
    assertion(contains("[Pt(η2-C2H4)Cl2(NH3)]", [atoms("Pt", 1), atoms("C", 2), atoms("H", 7), atoms("Cl", 2), atoms("N", 1)])),
    assertion(contains("[PH(O)(OH)2]", [atoms("P", 1), atoms("H", 3), atoms("O", 3)])),
    assertion(contains("[{Pt(η2-C2H4)Cl(μ-Cl)}2]", [atoms("Pt", 2), atoms("C", 4), atoms("H", 8), atoms("Cl", 4)])),
    assertion(contains("[BH4]-", [atoms("B", 1), atoms("H", 4)])),
    assertion(contains("[Al(OH)(OH2)5]2+", [atoms("Al", 1), atoms("H", 11)], atoms("O", 6))),
    assertion(contains("(Mg)[Cr2]O4", [atoms("Mg", 1), atoms("Cr", 2), atoms("O", 4)])),
    % Isotopes
    assertion(contains("H2[15N]NH2", [atoms("H", 4), atoms("N", 2)])),
    assertion(contains("[18O,32P]H3PO4", [atoms("H", 3), atoms("P", 1), atoms("O", 4)])),
    assertion(contains("SiH3[SiH2]8SiH3", [atoms("Si", 10), atoms("H", 22)])),
    assertion(contains("Ca3(PO4)2", [atoms("Ca", 3), atoms("P", 2), atoms("O", 8)])),
    assertion(contains("[Te(N3)6]", [atoms("Te", 1), atoms("N", 18)])),
    assertion(contains("(NO3)-", [atoms("N", 1), atoms("O", 3)])),
    assertion(contains("NO3-", [atoms("N", 1), atoms("O", 3)])),
    % assertion(contains("[FeH(H2)(Ph2PCH2CH2PPh2)2]+", [atoms("Fe", 1), atoms("H", 11), atoms("Ph", 8), atoms("P", 4), atoms("C", 4)])),
    assertion(contains("PH(O)(OH)2", [atoms("P", 1), atoms("H", 3), atoms("O", 3)])),
    assertion(contains("[Co(NH3)5(ONO)][PF6]2", [atoms("Co", 1), atoms("N", 6), atoms("H", 15), atoms("O", 2), atoms("P", 2), atoms("F", 12)])),
    % en is some ligand abbreviation
    % assertion(contains("[Co(en)3]3+", [])),
    % TODO: Find another more common one
    assertion(contains("NO(2*)-", [atoms("N", 1), atoms("O", 1)])),
    % TODO: Think of what to do with solid-state
    % assertion(contains("K(Br,Cl)", [])),
    % assertion(contains("(Mg)[Cr2]O4", [])),
    % State/Phase
    assertion(contains("HCl(g)", [atoms("H", 1), atoms("Cl", 1)])),
    assertion(contains("ClOO*", [atoms("Cl", 1), atoms("O", 2)])),
    % Addition compounds
    assertion(contains("3CdSO4·8H2O", [atoms("Cd", 3), atoms("S", 1), atoms("O", 12), atoms("H", 16)])),
    % Roman numerals
    % TODO: Handle it?
    assertion(contains("[CoIICoIIIW12O42]7-", [atoms("Co", 2), atoms("W", 12), atoms("O", 42)])),
    assertion(contains("[MnVIIO4]-", [atoms("Mn", 1), atoms("O", 4)])),
    assertion(contains("[Pt(PPh3)4]", [atoms("Pt", 1), atoms("P", 4), atoms("Ph", 12)])).

:- end_tests(test_elements).
