

:- begin_tests(test_starter).
:- use_module(ion, [binary_stoichiometric_name/2]).

test(binary_stoichiometric_name) :-
    assertion(binary_stoichiometric_name("NaCl", "sodium chloride")),
    % Red book IR-5.2 tests
    assertion(binary_stoichiometric_name("HCl", "hydrogen chloride")),
    assertion(binary_stoichiometric_name("NO", "nitrogen oxide")),
    assertion(binary_stoichiometric_name("NO2", "nitrogen dioxide")),
    assertion(binary_stoichiometric_name("N2O4", "dinitrogen tetraoxide")),
    assertion(binary_stoichiometric_name("OCl2", "oxygen dichloride")),
    assertion(binary_stoichiometric_name("O2Cl", "dioxygen chloride")),
    assertion(binary_stoichiometric_name("Fe3O4", "triiron tetraoxide")),
    assertion(binary_stoichiometric_name("SiC", "silicon carbide")),
    assertion(binary_stoichiometric_name("SiCl4", "silicon tetrachloride")),
    assertion(binary_stoichiometric_name("Ca3P2", "tricalcium diphosphide")),
    assertion(binary_stoichiometric_name("NiSn", "nickel stannide")),
    assertion(binary_stoichiometric_name("Cu5Zn8", "pentacopper octazincide")),
    assertion(binary_stoichiometric_name("Cr23C6", "tricosachromium hexacarbide")).

:- end_tests(test_starter).
