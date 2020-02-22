/* 
    Facts

*/
:- module(elements,[en/2, element_name/2, extract_elements_from_formula/2]).
:- use_module(utilities, [extract_term/2]).


% Element
element_name("H", hydrogen).
element_name("He", helium).
element_name("Li", lithium).
element_name("Be", beryllium).
element_name("B", boron).
element_name("C", carbon).
element_name("N", nitrogen).
element_name("O", oxygen).
element_name("F", flourine).
element_name("Ne", neon).
element_name("Na", sodium).
element_name("Mg", magnesium).
element_name("Al", aluminum).
element_name("Si", silicon).
element_name("P", phosphorus).
element_name("S", sulfur).
element_name("Cl", chlorine).
element_name("Ar", argon).
element_name("K", potassium).
element_name("Ca", calcium).
element_name("Sc", scandium).
element_name("Ti", titanium).
element_name("V", vanadium).
element_name("Cr", chromium).
element_name("Mn", manganese).
element_name("Fe", iron).
element_name("Co", cobalt).
element_name("Ni", nickel).
element_name("Cu", copper).
element_name("Zn", zinc).
element_name("Ga", gallium).
element_name("Ge", germanium).
element_name("As", arsenic).
element_name("Se", selenium).
element_name("Br", bromine).
element_name("Kr", krypton).
element_name("Rb", rubidium).
element_name("Sr", strontium).
element_name("Y", yttrium).
element_name("Zr", zirconium).
element_name("Nb", niobium).
element_name("Mo", molybdenom).
element_name("Tc", technitium).
element_name("Ru", ruthenium).
element_name("Rh", rhodium).
element_name("Pd", palladium).
element_name("Ag", silver).
element_name("Cd", cadmium).
element_name("In", indium).
element_name("Sn", tin).
element_name("Sb", antimony).
element_name("Te", tellurium).
element_name("I", iodine).
element_name("Xe", xenon).
element_name("Cs", cesium).
element_name("Ba", barium).
element_name("La", lanthanum).
element_name("Ce", cerium).
element_name("Pr", praseodymium).
element_name("Nd", neodymium).
element_name("Pm", promethium).
element_name("Sm", samarium).
element_name("Eu", europium).
element_name("Gd", gadolinium).
element_name("Tb", terbium).
element_name("Dy", dysprosium).
element_name("Ho", holmium).
element_name("Er", erbium).
element_name("Tm", thulium).
element_name("Yb", ytterbium).
element_name("Lu", lutetium).
element_name("Hf", hafnium).
element_name("Ta", tantalum).
element_name("W", tungsten).
element_name("Re", rhenium).
element_name("Os", osmium).
element_name("Ir", iridium).
element_name("Pt", platinum).
element_name("Au", gold).
element_name("Hg", mercury).
element_name("Tl", thallium).
element_name("Pb", lead).
element_name("Bi", bismuth).
element_name("Po", polonium).
element_name("At", astatine).
element_name("Rn", radon).
element_name("Fr", francium).
element_name("Ra", radium).
element_name("Ac", actinium).
element_name("Th", thorium).
element_name("Pa", protactinium).
element_name("U", uranium).
element_name("Np", neptumium).
element_name("Pu", plutonium).
element_name("Am", americium).
element_name("Cm", curium).
element_name("Bk", berkelium).
element_name("Cf", californium).
element_name("Es", einsteinium).
element_name("Fm", fermium).
element_name("Md", mendelevium).
element_name("No", nobelium).
element_name("Lr", lawrencium).
element_name("Rf", rutherfordium).
element_name("Db", dubnium).
element_name("Sg", seaorgium).
element_name("Bh", bhorium).
element_name("Hs", hassium).
element_name("Mt", meitnerium).
element_name("Ds", darmstadtium).
element_name("Rg", roentgenium).
element_name("Cn", copernicium).
element_name("Nh", nihonium).
element_name("Fl", flerovium).
element_name("Mc", moscovium).
element_name("Lv", livermorium).
element_name("Ts", tennessine).
element_name("Og", oganesson).


% EN
en("H", 2.2).
en("He", 0).
en("Li", 0.98).
en("Be", 1.57).
en("B", 2.04).
en("C", 2.55).
en("N", 3.04).
en("O", 3.44).
en("F", 3.98).
en("Ne", 0).
en("Na", 0.93).
en("Mg", 1.31).
en("Al", 1.61).
en("Si", 1.9).
en("P", 2.19).
en("S", 2.58).
en("Cl", 3.16).
en("Ar", 0).
en("K", 0.82).
en("Ca", 1).
en("Sc", 1.36).
en("Ti", 1.54).
en("V", 1.63).
en("Cr", 1.66).
en("Mn", 1.55).
en("Fe", 1.83).
en("Co", 1.88).
en("Ni", 1.91).
en("Cu", 1.9).
en("Zn", 1.65).
en("Ga", 1.81).
en("Ge", 2.01).
en("As", 2.18).
en("Se", 2.55).
en("Br", 2.96).
en("Kr", 3).
en("Rb", 0.82).
en("Sr", 0.95).
en("Y", 1.22).
en("Zr", 1.33).
en("Nb", 1.6).
en("Mo", 2.16).
en("Tc", 1.9).
en("Ru", 2.2).
en("Rh", 2.28).
en("Pd", 2.2).
en("Ag", 1.93).
en("Cd", 1.69).
en("In", 1.78).
en("Sn", 1.96).
en("Sb", 2.05).
en("Te", 2.1).
en("I", 2.66).
en("Xe", 2.6).
en("Cs", 0.79).
en("Ba", 0.89).
en("La", 1.1).
en("Ce", 1.12).
en("Pr", 1.13).
en("Nd", 1.14).
en("Pm", 0).
en("Sm", 1.17).
en("Eu", 0).
en("Gd", 1.2).
en("Tb", 0).
en("Dy", 1.22).
en("Ho", 1.23).
en("Er", 1.24).
en("Tm", 1.25).
en("Yb", 0).
en("Lu", 1.27).
en("Hf", 1.3).
en("Ta", 1.5).
en("W", 2.36).
en("Re", 1.9).
en("Os", 2.2).
en("Ir", 2.2).
en("Pt", 2.28).
en("Au", 2.54).
en("Hg", 2).
en("Tl", 1.62).
en("Pb", 2.33).
en("Bi", 2.02).
en("Po", 2).
en("At", 2.2).
en("Rn", 0).
en("Fr", 0.7).
en("Ra", 0.9).
en("Ac", 1.1).
en("Th", 1.3).
en("Pa", 1.5).
en("U", 1.38).
en("Np", 1.36).
en("Pu", 1.28).
en("Am", 1.3).
en("Cm", 1.3).
en("Bk", 1.3).
en("Cf", 1.3).
en("Es", 1.3).
en("Fm", 1.3).
en("Md", 1.3).
en("No", 1.3).
en("Lr", 1.3).
en("Rf", 0).
en("Db", 0).
en("Sg", 0).
en("Bh", 0).
en("Hs", 0).
en("Mt", 0).
en("Ds", 0).
en("Rg", 0).
en("Cn", 0).
en("Nh", 0).
en("Fl", 0).
en("Mc", 0).
en("Lv", 0).
en("Ts", 0).
en("Og", 0).

%
%
% TODO: validate each element
%

element_quantity(Symbol, Quantity) :-
    Quantity > 0,
    element_name(Symbol, _).
    
formula_to_quantified(Raw, Elements) :-
    re_split("([1-9][0-9]*)"/n, Raw, [RawSymbol, Quantity|_], []),
    element_name(Symbol, _),
    Elements =.. [element_quantity, Symbol, Quantity],
    call(Elements),
    atom_string(RawSymbol, Symbol).

extract_elements_from_formula(Formula, Elements) :-
    extract_elements_(Formula, 0, "", Elements).

determine_multiplicity(Formula, Result) :- 
    formula_to_quantified(Formula, Result).

determine_multiplicity(Formula, Result) :- 
    Result =.. [element_quantity, Formula, 1],
    call(Result).
   

extract_elements_(Formula, Start, _, _) :-
    string_length(Formula, Length),
    Start = Length.

extract_elements_(Formula, Start, String, End) :-
    string_length(Formula, Length),
    Start < Length,
    Final is Length - 1 - Start,
    sub_string(Formula, Final, _, Start, Out),
    Trim is Start + 1,
    string_concat(Out, String, ConcatString),
    (
        is_upper(Out) -> 
        extract_elements_(Formula, Trim, "", End2); 
        extract_elements_(Formula, Trim, ConcatString, End2),
    !) ,
    (is_upper(Out) -> determine_multiplicity(ConcatString, Result), Sth = [Result]; Sth = []),
    append(End2, Sth, End).