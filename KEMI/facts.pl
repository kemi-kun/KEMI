/* Contains facts for the KEMI knowledge base

Contents:
- element names (e.g. "hydrogen", "helium", ...)
- alternate element names (e.g. "natrium", "kallium", ...)
- element symbols (e.g. "H", "He", "Li", ...)
- simple multiplicative prefixes (e.g. mono, di, tri, ...)
- complex multiplicative prefixes (e.g. bis, tris, ...)
- pauling's electronegativity
*/


% From KEMI base: ElementNameFact
% element_name_fact(Element: atom, Name: string)
element_name_fact(hydrogen, "hydrogen").
element_name_fact(helium, "helium").
element_name_fact(lithium, "lithium").
element_name_fact(beryllium, "beryllium").
element_name_fact(boron, "boron").
element_name_fact(carbon, "carbon").
element_name_fact(nitrogen, "nitrogen").
element_name_fact(oxygen, "oxygen").
element_name_fact(fluorine, "fluorine").
element_name_fact(neon, "neon").
element_name_fact(sodium, "sodium").
element_name_fact(magnesium, "magnesium").
element_name_fact(aluminium, "aluminium").
element_name_fact(silicon, "silicon").
element_name_fact(phosphorus, "phosphorus").
element_name_fact(sulfur, "sulfur").
element_name_fact(chlorine, "chlorine").
element_name_fact(argon, "argon").
element_name_fact(potassium, "potassium").
element_name_fact(calcium, "calcium").
element_name_fact(scandium, "scandium").
element_name_fact(titanium, "titanium").
element_name_fact(vanadium, "vanadium").
element_name_fact(chromium, "chromium").
element_name_fact(manganese, "manganese").
element_name_fact(iron, "iron").
element_name_fact(cobalt, "cobalt").
element_name_fact(nickel, "nickel").
element_name_fact(copper, "copper").
element_name_fact(zinc, "zinc").
element_name_fact(gallium, "gallium").
element_name_fact(germanium, "germanium").
element_name_fact(arsenic, "arsenic").
element_name_fact(selenium, "selenium").
element_name_fact(bromine, "bromine").
element_name_fact(krypton, "krypton").
element_name_fact(rubidium, "rubidium").
element_name_fact(strontium, "strontium").
element_name_fact(yttrium, "yttrium").
element_name_fact(zirconium, "zirconium").
element_name_fact(niobium, "niobium").
element_name_fact(molybdenum, "molybdenum").
element_name_fact(technetium, "technetium").
element_name_fact(ruthenium, "ruthenium").
element_name_fact(rhodium, "rhodium").
element_name_fact(palladium, "palladium").
element_name_fact(silver, "silver").
element_name_fact(cadmium, "cadmium").
element_name_fact(indium, "indium").
element_name_fact(tin, "tin").
element_name_fact(antimony, "antimony").
element_name_fact(tellurium, "tellurium").
element_name_fact(iodine, "iodine").
element_name_fact(xenon, "xenon").
element_name_fact(caesium, "caesium").
element_name_fact(barium, "barium").
element_name_fact(lanthanum, "lanthanum").
element_name_fact(cerium, "cerium").
element_name_fact(praseodymium, "praseodymium").
element_name_fact(neodymium, "neodymium").
element_name_fact(promethium, "promethium").
element_name_fact(samarium, "samarium").
element_name_fact(europium, "europium").
element_name_fact(gadolinium, "gadolinium").
element_name_fact(terbium, "terbium").
element_name_fact(dysprosium, "dysprosium").
element_name_fact(holmium, "holmium").
element_name_fact(erbium, "erbium").
element_name_fact(thulium, "thulium").
element_name_fact(ytterbium, "ytterbium").
element_name_fact(lutetium, "lutetium").
element_name_fact(hafnium, "hafnium").
element_name_fact(tantalum, "tantalum").
element_name_fact(tungsten, "tungsten").
element_name_fact(rhenium, "rhenium").
element_name_fact(osmium, "osmium").
element_name_fact(iridium, "iridium").
element_name_fact(platinum, "platinum").
element_name_fact(gold, "gold").
element_name_fact(mercury, "mercury").
element_name_fact(thallium, "thallium").
element_name_fact(lead, "lead").
element_name_fact(bismuth, "bismuth").
element_name_fact(polonium, "polonium").
element_name_fact(astatine, "astatine").
element_name_fact(radon, "radon").
element_name_fact(francium, "francium").
element_name_fact(radium, "radium").
element_name_fact(actinium, "actinium").
element_name_fact(thorium, "thorium").
element_name_fact(protactinium, "protactinium").
element_name_fact(uranium, "uranium").
element_name_fact(neptunium, "neptunium").
element_name_fact(plutonium, "plutonium").
element_name_fact(americium, "americium").
element_name_fact(curium, "curium").
element_name_fact(berkelium, "berkelium").
element_name_fact(californium, "californium").
element_name_fact(einsteinium, "einsteinium").
element_name_fact(fermium, "fermium").
element_name_fact(mendelevium, "mendelevium").
element_name_fact(nobelium, "nobelium").
element_name_fact(lawrencium, "lawrencium").
element_name_fact(rutherfordium, "rutherfordium").
element_name_fact(dubnium, "dubnium").
element_name_fact(seaborgium, "seaborgium").
element_name_fact(bohrium, "bohrium").
element_name_fact(hassium, "hassium").
element_name_fact(meitnerium, "meitnerium").
element_name_fact(darmstadtium, "darmstadtium").
element_name_fact(roentgenium, "roentgenium").
element_name_fact(copernicium, "copernicium").
element_name_fact(nihonium, "nihonium").
element_name_fact(flerovium, "flerovium").
element_name_fact(moscovium, "moscovium").
element_name_fact(livermorium, "livermorium").
element_name_fact(tennessine, "tennessine").
element_name_fact(oganesson, "oganesson").


% not in KEMI base
% element_symbol_fact(Element: atom, Symbol: string).
element_symbol_fact(hydrogen, "H").
element_symbol_fact(helium, "He").
element_symbol_fact(lithium, "Li").
element_symbol_fact(beryllium, "Be").
element_symbol_fact(boron, "B").
element_symbol_fact(carbon, "C").
element_symbol_fact(nitrogen, "N").
element_symbol_fact(oxygen, "O").
element_symbol_fact(fluorine, "F").
element_symbol_fact(neon, "Ne").
element_symbol_fact(sodium, "Na").
element_symbol_fact(magnesium, "Mg").
element_symbol_fact(aluminium, "Al").
element_symbol_fact(silicon, "Si").
element_symbol_fact(phosphorus, "P").
element_symbol_fact(sulfur, "S").
element_symbol_fact(chlorine, "Cl").
element_symbol_fact(argon, "Ar").
element_symbol_fact(potassium, "K").
element_symbol_fact(calcium, "Ca").
element_symbol_fact(scandium, "Sc").
element_symbol_fact(titanium, "Ti").
element_symbol_fact(vanadium, "V").
element_symbol_fact(chromium, "Cr").
element_symbol_fact(manganese, "Mn").
element_symbol_fact(iron, "Fe").
element_symbol_fact(cobalt, "Co").
element_symbol_fact(nickel, "Ni").
element_symbol_fact(copper, "Cu").
element_symbol_fact(zinc, "Zn").
element_symbol_fact(gallium, "Ga").
element_symbol_fact(germanium, "Ge").
element_symbol_fact(arsenic, "As").
element_symbol_fact(selenium, "Se").
element_symbol_fact(bromine, "Br").
element_symbol_fact(krypton, "Kr").
element_symbol_fact(rubidium, "Rb").
element_symbol_fact(strontium, "Sr").
element_symbol_fact(yttrium, "Y").
element_symbol_fact(zirconium, "Zr").
element_symbol_fact(niobium, "Nb").
element_symbol_fact(molybdenum, "Mo").
element_symbol_fact(technetium, "Tc").
element_symbol_fact(ruthenium, "Ru").
element_symbol_fact(rhodium, "Rh").
element_symbol_fact(palladium, "Pd").
element_symbol_fact(silver, "Ag").
element_symbol_fact(cadmium, "Cd").
element_symbol_fact(indium, "In").
element_symbol_fact(tin, "Sn").
element_symbol_fact(antimony, "Sb").
element_symbol_fact(tellurium, "Te").
element_symbol_fact(iodine, "I").
element_symbol_fact(xenon, "Xe").
element_symbol_fact(caesium, "Cs").
element_symbol_fact(barium, "Ba").
element_symbol_fact(lanthanum, "La").
element_symbol_fact(cerium, "Ce").
element_symbol_fact(praseodymium, "Pr").
element_symbol_fact(neodymium, "Nd").
element_symbol_fact(promethium, "Pm").
element_symbol_fact(samarium, "Sm").
element_symbol_fact(europium, "Eu").
element_symbol_fact(gadolinium, "Gd").
element_symbol_fact(terbium, "Tb").
element_symbol_fact(dysprosium, "Dy").
element_symbol_fact(holmium, "Ho").
element_symbol_fact(erbium, "Er").
element_symbol_fact(thulium, "Tm").
element_symbol_fact(ytterbium, "Yb").
element_symbol_fact(lutetium, "Lu").
element_symbol_fact(hafnium, "Hf").
element_symbol_fact(tantalum, "Ta").
element_symbol_fact(tungsten, "W").
element_symbol_fact(rhenium, "Re").
element_symbol_fact(osmium, "Os").
element_symbol_fact(iridium, "Ir").
element_symbol_fact(platinum, "Pt").
element_symbol_fact(gold, "Au").
element_symbol_fact(mercury, "Hg").
element_symbol_fact(thallium, "Tl").
element_symbol_fact(lead, "Pb").
element_symbol_fact(bismuth, "Bi").
element_symbol_fact(polonium, "Po").
element_symbol_fact(astatine, "At").
element_symbol_fact(radon, "Rn").
element_symbol_fact(francium, "Fr").
element_symbol_fact(radium, "Ra").
element_symbol_fact(actinium, "Ac").
element_symbol_fact(thorium, "Th").
element_symbol_fact(protactinium, "Pa").
element_symbol_fact(uranium, "U").
element_symbol_fact(neptunium, "Np").
element_symbol_fact(plutonium, "Pu").
element_symbol_fact(americium, "Am").
element_symbol_fact(curium, "Cm").
element_symbol_fact(berkelium, "Bk").
element_symbol_fact(californium, "Cf").
element_symbol_fact(einsteinium, "Es").
element_symbol_fact(fermium, "Fm").
element_symbol_fact(mendelevium, "Md").
element_symbol_fact(nobelium, "No").
element_symbol_fact(lawrencium, "Lr").
element_symbol_fact(rutherfordium, "Rf").
element_symbol_fact(dubnium, "Db").
element_symbol_fact(seaborgium, "Sg").
element_symbol_fact(bohrium, "Bh").
element_symbol_fact(hassium, "Hs").
element_symbol_fact(meitnerium, "Mt").
element_symbol_fact(darmstadtium, "Ds").
element_symbol_fact(roentgenium, "Rg").
element_symbol_fact(copernicium, "Cn").
element_symbol_fact(nihonium, "Nh").
element_symbol_fact(flerovium, "Fl").
element_symbol_fact(moscovium, "Mc").
element_symbol_fact(livermorium, "Lv").
element_symbol_fact(tennessine, "Ts").
element_symbol_fact(oganesson, "Og").


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


% From KEMI base: AlternativeElementNameFact
% alternative_element_name_fact(Element: atom, Name: string)
alternative_element_name_fact(antimony, "stibium").
alternative_element_name_fact(copper, "cuprum").
alternative_element_name_fact(gold, "aurum").
alternative_element_name_fact(iron, "ferrum").
alternative_element_name_fact(lead, "plumbum").
alternative_element_name_fact(mercury, "hydrargyrum.").
alternative_element_name_fact(potassium, "kalium").
alternative_element_name_fact(silver, "argentum").
alternative_element_name_fact(sodium, "natrium").
alternative_element_name_fact(tin, "stannum").
alternative_element_name_fact(tungsten, "wolfram").
alternative_element_name_fact(aluminium, "aluminum").
alternative_element_name_fact(caesium, "cesium").
