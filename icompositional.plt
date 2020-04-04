:- begin_tests(test_icompositional).
:- use_module(icompositional, [binary_compound_cn/2]).

test(binary_compound_cn) :-
    assertion(binary_compound_cn("NaCl", "sodium chloride")),
    % Red book IR-5.2 tests
    assertion(binary_compound_cn("HCl", "hydrogen chloride")),
    assertion(binary_compound_cn("NO", "nitrogen oxide")),
    assertion(binary_compound_cn("NO2", "nitrogen dioxide")),
    assertion(binary_compound_cn("N2O4", "dinitrogen tetraoxide")),
    assertion(binary_compound_cn("OCl2", "oxygen dichloride")),
    assertion(binary_compound_cn("O2Cl", "dioxygen chloride")),
    assertion(binary_compound_cn("Fe3O4", "triiron tetraoxide")),
    assertion(binary_compound_cn("SiC", "silicon carbide")),
    assertion(binary_compound_cn("SiCl4", "silicon tetrachloride")),
    assertion(binary_compound_cn("Ca3P2", "tricalcium diphosphide")),
    assertion(binary_compound_cn("NiSn", "nickel stannide")),
    assertion(binary_compound_cn("Cu5Zn8", "pentacopper octazincide")),
    assertion(binary_compound_cn("Cr23C6", "tricosachromium hexacarbide")).

:- end_tests(test_icompositional).
