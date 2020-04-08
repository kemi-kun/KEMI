/* Contains facts for the KEMI knowledge base

Contents:
- element names (e.g. "hydrogen", "helium", ...)
- alternate element names (e.g. "natrium", "kallium", ...)
- element symbols (e.g. "H", "He", "Li", ...)
- simple multiplicative prefixes (e.g. mono, di, tri, ...)
- complex multiplicative prefixes (e.g. bis, tris, ...)
- pauling's electronegativity
*/
:- module(facts,[
    addition_compound_exception/2,
    parent_name_exception/2,
    complex_multiplicative_prefix_fact/2,
    multiplicative_prefix_fact/2,
    multiplicative_affix_fact/2,
    numerical_root_fact/2,
    alternative_element_name/2,
    latin_element_name_fact/2,
    en/2,
    element_fact/5
    ]).


% From KEMI base: ElementFact
% element_fact(Element: atom, Name: string, Symbol: string, AtomicNumber: integer, AtomicWeight: real)
element_fact(hydrogen,      "hydrogen",      "H",  1,   1.007).
element_fact(helium,        "helium",        "He", 2,   4.002).
element_fact(lithium,       "lithium",       "Li", 3,   6.941).
element_fact(beryllium,     "beryllium",     "Be", 4,   9.012).
element_fact(boron,         "boron",         "B",  5,   10.811).
element_fact(carbon,        "carbon",        "C",  6,   12.011).
element_fact(nitrogen,      "nitrogen",      "N",  7,   14.007).
element_fact(oxygen,        "oxygen",        "O",  8,   15.999).
element_fact(fluorine,      "fluorine",      "F",  9,   18.998).
element_fact(neon,          "neon",          "Ne", 10,  20.18).
element_fact(sodium,        "sodium",        "Na", 11,  22.99).
element_fact(magnesium,     "magnesium",     "Mg", 12,  24.305).
element_fact(aluminium,     "aluminium",     "Al", 13,  26.982).
element_fact(silicon,       "silicon",       "Si", 14,  28.086).
element_fact(phosphorus,    "phosphorus",    "P",  15,  30.974).
element_fact(sulfur,        "sulfur",        "S",  16,  32.065).
element_fact(chlorine,      "chlorine",      "Cl", 17,  35.453).
element_fact(argon,         "argon",         "Ar", 18,  39.948).
element_fact(potassium,     "potassium",     "K",  19,  39.098).
element_fact(calcium,       "calcium",       "Ca", 20,  40.078).
element_fact(scandium,      "scandium",      "Sc", 21,  44.956).
element_fact(titanium,      "titanium",      "Ti", 22,  47.867).
element_fact(vanadium,      "vanadium",      "V",  23,  50.942).
element_fact(chromium,      "chromium",      "Cr", 24,  51.996).
element_fact(manganese,     "manganese",     "Mn", 25,  54.938).
element_fact(iron,          "iron",          "Fe", 26,  55.845).
element_fact(cobalt,        "cobalt",        "Co", 27,  58.933).
element_fact(nickel,        "nickel",        "Ni", 28,  58.693).
element_fact(copper,        "copper",        "Cu", 29,  63.546).
element_fact(zinc,          "zinc",          "Zn", 30,  65.38).
element_fact(gallium,       "gallium",       "Ga", 31,  69.723).
element_fact(germanium,     "germanium",     "Ge", 32,  72.64).
element_fact(arsenic,       "arsenic",       "As", 33,  74.922).
element_fact(selenium,      "selenium",      "Se", 34,  78.96).
element_fact(bromine,       "bromine",       "Br", 35,  79.904).
element_fact(krypton,       "krypton",       "Kr", 36,  83.798).
element_fact(rubidium,      "rubidium",      "Rb", 37,  85.468).
element_fact(strontium,     "strontium",     "Sr", 38,  87.62).
element_fact(yttrium,       "yttrium",       "Y",  39,  88.906).
element_fact(zirconium,     "zirconium",     "Zr", 40,  91.224).
element_fact(niobium,       "niobium",       "Nb", 41,  92.906).
element_fact(molybdenum,    "molybdenum",    "Mo", 42,  95.96).
element_fact(technetium,    "technetium",    "Tc", 43,  98).
element_fact(ruthenium,     "ruthenium",     "Ru", 44,  101.07).
element_fact(rhodium,       "rhodium",       "Rh", 45,  102.906).
element_fact(palladium,     "palladium",     "Pd", 46,  106.42).
element_fact(silver,        "silver",        "Ag", 47,  107.868).
element_fact(cadmium,       "cadmium",       "Cd", 48,  112.411).
element_fact(indium,        "indium",        "In", 49,  114.818).
element_fact(tin,           "tin",           "Sn", 50,  118.71).
element_fact(antimony,      "antimony",      "Sb", 51,  121.76).
element_fact(tellurium,     "tellurium",     "Te", 52,  127.6).
element_fact(iodine,        "iodine",        "I",  53,  126.904).
element_fact(xenon,         "xenon",         "Xe", 54,  131.293).
element_fact(caesium,       "caesium",       "Cs", 55,  132.905).
element_fact(barium,        "barium",        "Ba", 56,  137.327).
element_fact(lanthanum,     "lanthanum",     "La", 57,  138.905).
element_fact(cerium,        "cerium",        "Ce", 58,  140.116).
element_fact(praseodymium,  "praseodymium",  "Pr", 59,  140.908).
element_fact(neodymium,     "neodymium",     "Nd", 60,  144.242).
element_fact(promethium,    "promethium",    "Pm", 61,  145).
element_fact(samarium,      "samarium",      "Sm", 62,  150.36).
element_fact(europium,      "europium",      "Eu", 63,  151.964).
element_fact(gadolinium,    "gadolinium",    "Gd", 64,  157.25).
element_fact(terbium,       "terbium",       "Tb", 65,  158.925).
element_fact(dysprosium,    "dysprosium",    "Dy", 66,  162.5).
element_fact(holmium,       "holmium",       "Ho", 67,  164.93).
element_fact(erbium,        "erbium",        "Er", 68,  167.259).
element_fact(thulium,       "thulium",       "Tm", 69,  168.934).
element_fact(ytterbium,     "ytterbium",     "Yb", 70,  173.054).
element_fact(lutetium,      "lutetium",      "Lu", 71,  174.967).
element_fact(hafnium,       "hafnium",       "Hf", 72,  178.49).
element_fact(tantalum,      "tantalum",      "Ta", 73,  180.948).
element_fact(tungsten,      "tungsten",      "W",  74,  183.84).
element_fact(rhenium,       "rhenium",       "Re", 75,  186.207).
element_fact(osmium,        "osmium",        "Os", 76,  190.23).
element_fact(iridium,       "iridium",       "Ir", 77,  192.217).
element_fact(platinum,      "platinum",      "Pt", 78,  195.084).
element_fact(gold,          "gold",          "Au", 79,  196.967).
element_fact(mercury,       "mercury",       "Hg", 80,  200.59).
element_fact(thallium,      "thallium",      "Tl", 81,  204.383).
element_fact(lead,          "lead",          "Pb", 82,  207.2).
element_fact(bismuth,       "bismuth",       "Bi", 83,  208.98).
element_fact(polonium,      "polonium",      "Po", 84,  210).
element_fact(astatine,      "astatine",      "At", 85,  210).
element_fact(radon,         "radon",         "Rn", 86,  222).
element_fact(francium,      "francium",      "Fr", 87,  223).
element_fact(radium,        "radium",        "Ra", 88,  226).
element_fact(actinium,      "actinium",      "Ac", 89,  227).
element_fact(thorium,       "thorium",       "Th", 90,  232.038).
element_fact(protactinium,  "protactinium",  "Pa", 91,  231.036).
element_fact(uranium,       "uranium",       "U",  92,  238.029).
element_fact(neptunium,     "neptunium",     "Np", 93,  237).
element_fact(plutonium,     "plutonium",     "Pu", 94,  244).
element_fact(americium,     "americium",     "Am", 95,  243).
element_fact(curium,        "curium",        "Cm", 96,  247).
element_fact(berkelium,     "berkelium",     "Bk", 97,  247).
element_fact(californium,   "californium",   "Cf", 98,  251).
element_fact(einsteinium,   "einsteinium",   "Es", 99,  252).
element_fact(fermium,       "fermium",       "Fm", 100, 257).
element_fact(mendelevium,   "mendelevium",   "Md", 101, 258).
element_fact(nobelium,      "nobelium",      "No", 102, 259).
element_fact(lawrencium,    "lawrencium",    "Lr", 103, 262).
element_fact(rutherfordium, "rutherfordium", "Rf", 104, 261).
element_fact(dubnium,       "dubnium",       "Db", 105, 262).
element_fact(seaborgium,    "seaborgium",    "Sg", 106, 266).
element_fact(bohrium,       "bohrium",       "Bh", 107, 264).
element_fact(hassium,       "hassium",       "Hs", 108, 267).
element_fact(meitnerium,    "meitnerium",    "Mt", 109, 268).
element_fact(darmstadtium,  "darmstadtium",  "Ds", 110, 271).
element_fact(roentgenium,   "roentgenium",   "Rg", 111, 272).
element_fact(copernicium,   "copernicium",   "Cn", 112, 285).
element_fact(nihonium,      "nihonium",      "Nh", 113, 284).
element_fact(flerovium,     "flerovium",     "Fl", 114, 289).
element_fact(moscovium,     "moscovium",     "Mc", 115, 288).
element_fact(livermorium,   "livermorium",   "Lv", 116, 292).
element_fact(tennessine,    "tennessine",    "Ts", 117, 295).
element_fact(oganesson,     "oganesson",     "Og", 118, 294).


% From KEMI base: AlternativeElementNameFact
% alternative_element_name(Element: atom, Name: string)
alternative_element_name(Element, Name) :-
    nonvar(Name),
    (
        latin_element_name_fact(Element, Name) -> true;
        american_element_name_fact(Element, Name) -> true;
        alternative_element_name_fact(Element, Name)
    ), !.
alternative_element_name(Element, Name) :-
    latin_element_name_fact(Element, Name);
    american_element_name_fact(Element, Name);
    alternative_element_name_fact(Element, Name).

latin_element_name_fact(antimony,  "stibium").
latin_element_name_fact(copper,    "cuprum").
latin_element_name_fact(gold,      "aurum").
latin_element_name_fact(iron,      "ferrum").
latin_element_name_fact(lead,      "plumbum").
latin_element_name_fact(mercury,   "hydrargyrum").
latin_element_name_fact(potassium, "kalium").   % neo
latin_element_name_fact(silver,    "argentum").
latin_element_name_fact(sodium,    "natrium").
latin_element_name_fact(tin,       "stannum").
alternative_element_name_fact(tungsten,  "wolfram").
american_element_name_fact(aluminium, "aluminum").
american_element_name_fact(caesium,   "cesium").


% From KEMI base: NumericalRootFact
% numerical_root_fact(Number: integer, Prefix: string)
numerical_root_fact(1, "un").
numerical_root_fact(2, "bi").
numerical_root_fact(3, "tri").
numerical_root_fact(4, "quad").
numerical_root_fact(5, "pent").
numerical_root_fact(6, "hex").
numerical_root_fact(7, "sept").
numerical_root_fact(8, "oct").
numerical_root_fact(9, "enn").
numerical_root_fact(0, "nil").


% From KEMI base: MultiplicativePrefixFact
% multiplicative_prefix_fact(number: integer, prefix: string)
multiplicative_prefix_fact(1, "mono").
multiplicative_prefix_fact(2, "di").

% From KEMI base: MultiplicativeAffixFact
% multiplicative_affix_fact(Number: integer, Prefix: string)
multiplicative_affix_fact(1, "hen").
multiplicative_affix_fact(2, "do").
multiplicative_affix_fact(3, "tri").
multiplicative_affix_fact(4, "tetra").
multiplicative_affix_fact(5, "penta").
multiplicative_affix_fact(6, "hexa").
multiplicative_affix_fact(7, "hepta").
multiplicative_affix_fact(8, "octa").
multiplicative_affix_fact(9, "nona").
multiplicative_affix_fact(10, "deca").
multiplicative_affix_fact(11, "undeca").
% multiplicative_affix_fact(12, "dodeca").
% multiplicative_affix_fact(13, "trideca").
% multiplicative_affix_fact(14, "tetradeca").
% multiplicative_affix_fact(15, "pentadeca").
% multiplicative_affix_fact(16, "hexadeca").
% multiplicative_affix_fact(17, "heptadeca").
% multiplicative_affix_fact(18, "octadeca").
% multiplicative_affix_fact(19, "nonadeca").
multiplicative_affix_fact(20, "icosa").
multiplicative_affix_fact(21, "henicosa").
multiplicative_affix_fact(22, "docosa").
multiplicative_affix_fact(23, "tricosa").
multiplicative_affix_fact(23, "tetracosa"). % Page 26 https://old.iupac.org/reports/provisional/abstract04/BB-prs310305/Chapter1.pdf
multiplicative_affix_fact(30, "triaconta").
% multiplicative_affix_fact(31, "hentriaconta").
% multiplicative_affix_fact(35, "pentatriaconta").
% multiplicative_affix_fact(40, "tetraconta").
% multiplicative_affix_fact(48, "octatetraconta").
% multiplicative_affix_fact(50, "pentaconta").
% multiplicative_affix_fact(52, "dopentaconta").
% multiplicative_affix_fact(60, "hexaconta").
% multiplicative_affix_fact(70, "heptaconta").
% multiplicative_affix_fact(80, "octaconta").
% multiplicative_affix_fact(90, "nonaconta").
multiplicative_affix_fact(100, "hecta").
multiplicative_affix_fact(200, "dicta").
% multiplicative_affix_fact(500, "pentacta").
multiplicative_affix_fact(1000, "kilia").
multiplicative_affix_fact(2000, "dilia").


% From KEMI base: ComplexMultiplicativePrefixFact
% complex_multiplicative_prefix_fact(Number: integer, Prefix: string)
complex_multiplicative_prefix_fact(2, "bis").
complex_multiplicative_prefix_fact(3, "tris").
% complex_multiplicative_prefix_fact(4, "tetrakis").
% complex_multiplicative_prefix_fact(5, "pentakis").
% complex_multiplicative_prefix_fact(6, "hexakis").
% complex_multiplicative_prefix_fact(7, "heptakis").
% complex_multiplicative_prefix_fact(8, "octakis").
% complex_multiplicative_prefix_fact(9, "nonakis").
% complex_multiplicative_prefix_fact(10, "decakis").


% From KEMI base: ParentNameException
% parent_name_exception(Formula: string, Name: string)
parent_name_exception("CH4", "methane").
parent_name_exception("NH3", "azane").
parent_name_exception("InH3", "indigane").


% From KEMI base: EN
% en(Element: atom, EN: real)
en(Element, EN) :-
    (
        pauling_electronegativity_fact(Element, EN), !;
        element_fact(Element, _, _, _, _), EN is 0, !;
        false
    ).
pauling_electronegativity_fact(hydrogen, 2.2).
pauling_electronegativity_fact(lithium, 0.98).
pauling_electronegativity_fact(beryllium, 1.57).
pauling_electronegativity_fact(boron, 2.04).
pauling_electronegativity_fact(carbon, 2.55).
pauling_electronegativity_fact(nitrogen, 3.04).
pauling_electronegativity_fact(oxygen, 3.44).
pauling_electronegativity_fact(fluorine, 3.98).
pauling_electronegativity_fact(sodium, 0.93).
pauling_electronegativity_fact(magnesium, 1.31).
pauling_electronegativity_fact(aluminum, 1.61).
pauling_electronegativity_fact(silicon, 1.9).
pauling_electronegativity_fact(phosphorus, 2.19).
pauling_electronegativity_fact(sulfur, 2.58).
pauling_electronegativity_fact(chlorine, 3.16).
pauling_electronegativity_fact(potassium, 0.82).
pauling_electronegativity_fact(calcium, 1).
pauling_electronegativity_fact(scandium, 1.36).
pauling_electronegativity_fact(titanium, 1.54).
pauling_electronegativity_fact(vanadium, 1.63).
pauling_electronegativity_fact(chromium, 1.66).
pauling_electronegativity_fact(manganese, 1.55).
pauling_electronegativity_fact(iron, 1.83).
pauling_electronegativity_fact(cobalt, 1.88).
pauling_electronegativity_fact(nickel, 1.91).
pauling_electronegativity_fact(copper, 1.9).
pauling_electronegativity_fact(zinc, 1.65).
pauling_electronegativity_fact(gallium, 1.81).
pauling_electronegativity_fact(germanium, 2.01).
pauling_electronegativity_fact(arsenic, 2.18).
pauling_electronegativity_fact(selenium, 2.55).
pauling_electronegativity_fact(bromine, 2.96).
pauling_electronegativity_fact(krypton, 3).
pauling_electronegativity_fact(rubidium, 0.82).
pauling_electronegativity_fact(strontium, 0.95).
pauling_electronegativity_fact(yttrium, 1.22).
pauling_electronegativity_fact(zirconium, 1.33).
pauling_electronegativity_fact(niobium, 1.6).
pauling_electronegativity_fact(molybdenum, 2.16).
pauling_electronegativity_fact(technetium, 1.9).
pauling_electronegativity_fact(ruthenium, 2.2).
pauling_electronegativity_fact(rhodium, 2.28).
pauling_electronegativity_fact(palladium, 2.2).
pauling_electronegativity_fact(silver, 1.93).
pauling_electronegativity_fact(cadmium, 1.69).
pauling_electronegativity_fact(indium, 1.78).
pauling_electronegativity_fact(tin, 1.96).
pauling_electronegativity_fact(antimony, 2.05).
pauling_electronegativity_fact(tellurium, 2.1).
pauling_electronegativity_fact(iodine, 2.66).
pauling_electronegativity_fact(xenon, 2.6).
pauling_electronegativity_fact(cesium, 0.79).
pauling_electronegativity_fact(barium, 0.89).
pauling_electronegativity_fact(lanthanum, 1.1).
pauling_electronegativity_fact(cerium, 1.12).
pauling_electronegativity_fact(praseodymium, 1.13).
pauling_electronegativity_fact(neodymium, 1.14).
pauling_electronegativity_fact(samarium, 1.17).
pauling_electronegativity_fact(gadolinium, 1.2).
pauling_electronegativity_fact(dysprosium, 1.22).
pauling_electronegativity_fact(holmium, 1.23).
pauling_electronegativity_fact(erbium, 1.24).
pauling_electronegativity_fact(thulium, 1.25).
pauling_electronegativity_fact(lutetium, 1.27).
pauling_electronegativity_fact(hafnium, 1.3).
pauling_electronegativity_fact(tantalum, 1.5).
pauling_electronegativity_fact(tungsten, 2.36).
pauling_electronegativity_fact(rhenium, 1.9).
pauling_electronegativity_fact(osmium, 2.2).
pauling_electronegativity_fact(iridium, 2.2).
pauling_electronegativity_fact(platinum, 2.28).
pauling_electronegativity_fact(gold, 2.54).
pauling_electronegativity_fact(mercury, 2).
pauling_electronegativity_fact(thallium, 1.62).
pauling_electronegativity_fact(lead, 2.33).
pauling_electronegativity_fact(bismuth, 2.02).
pauling_electronegativity_fact(polonium, 2).
pauling_electronegativity_fact(astatine, 2.2).
pauling_electronegativity_fact(francium, 0.7).
pauling_electronegativity_fact(radium, 0.9).
pauling_electronegativity_fact(actinium, 1.1).
pauling_electronegativity_fact(thorium, 1.3).
pauling_electronegativity_fact(protactinium, 1.5).
pauling_electronegativity_fact(uranium, 1.38).
pauling_electronegativity_fact(neptunium, 1.36).
pauling_electronegativity_fact(plutonium, 1.28).
pauling_electronegativity_fact(americium, 1.3).
pauling_electronegativity_fact(curium, 1.3).
pauling_electronegativity_fact(berkelium, 1.3).
pauling_electronegativity_fact(californium, 1.3).
pauling_electronegativity_fact(einsteinium, 1.3).
pauling_electronegativity_fact(fermium, 1.3).
pauling_electronegativity_fact(mendelevium, 1.3).
pauling_electronegativity_fact(nobelium, 1.3).
pauling_electronegativity_fact(lawrencium, 1.3).


% From KEMI base: FunctionalReplacementPrefixFact
functional_replacement_prefix_fact(chlorine, "chloro").

addition_compound_exception("H2O", "water").
