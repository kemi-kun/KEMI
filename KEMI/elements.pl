/* 
    Facts

*/
:- module(elements,[element_name/2, metal/1,nonmetal/1, extract_elements_from_formula/2]).


% Elements
chemical_element(hydrogen).
chemical_element(carbon).
symbol('H').

% Element names
element_name(S, E) :- 
    chemical_element(E),
    symbol(S).

element_name('H', hydrogen).
element_name('He', helium).
element_name('Li', lithium).
element_name('Be', beryllium).
element_name('B', boron).
element_name('C', carbon).
element_name('N', nitrogen).
element_name('O', oxygen).
element_name('F', flourine).
element_name('Ne', neon).
element_name('Na', sodium).
element_name('Mg', magnesium).
element_name('Al', aluminum).
element_name('Si', silicon).
element_name('P', phosphorus).
element_name('S', sulfur).
element_name('Cl', chlorine).
element_name('Ar', argon).
element_name('K', potassium).
element_name('Ca', calcium).
element_name('Sc', scandium).
element_name('Ti', titanium).
element_name('V', vanadium).
element_name('Cr', chromium).
element_name('Mn', manganese).
element_name('Fe', iron).
element_name('Co', cobalt).
element_name('Ni', nickel).
element_name('Cu', copper).
element_name('Zn', zinc).
element_name('Ga', gallium).
element_name('Ge', germanium).
element_name('As', arsenic).
element_name('Se', selenium).
element_name('Br', bromine).
element_name('Kr', krypton).
element_name('Rb', rubidium).
element_name('Sr', strontium).
element_name('Y', yttrium).
element_name('Zr', zirconium).
element_name('Nb', niobium).
element_name('Mo', molybdenom).
element_name('Tc', technitium).
element_name('Ru', ruthenium).
element_name('Rh', rhodium).
element_name('Pd', palladium).
element_name('Ag', silver).
element_name('Cd', cadmium).
element_name('In', indium).
element_name('Sn', tin).
element_name('Sb', antimony).
element_name('Te', tellurium).
element_name('I', iodine).
element_name('Xe', xenon).
element_name('Cs', cesium).
element_name('Ba', barium).
element_name('La', lanthanum).
element_name('Ce', cerium).
element_name('Pr', praseodymium).
element_name('Nd', neodymium).
element_name('Pm', promethium).
element_name('Sm', samarium).
element_name('Eu', europium).
element_name('Gd', gadolinium).
element_name('Tb', terbium).
element_name('Dy', dysprosium).
element_name('Ho', holmium).
element_name('Er', erbium).
element_name('Tm', thulium).
element_name('Yb', ytterbium).
element_name('Lu', lutetium).
element_name('Hf', hafnium).
element_name('Ta', tantalum).
element_name('W', tungsten).
element_name('Re', rhenium).
element_name('Os', osmium).
element_name('Ir', iridium).
element_name('Pt', platinum).
element_name('Au', gold).
element_name('Hg', mercury).
element_name('Tl', thallium).
element_name('Pb', lead).
element_name('Bi', bismuth).
element_name('Po', polonium).
element_name('At', astatine).
element_name('Rn', radon).
element_name('Fr', francium).
element_name('Ra', radium).
element_name('Ac', actinium).
element_name('Th', thorium).
element_name('Pa', protactinium).
element_name('U', uranium).
element_name('Np', neptumium).
element_name('Pu', plutonium).
element_name('Am', americium).
element_name('Cm', curium).
element_name('Bk', berkelium).
element_name('Cf', californium).
element_name('Es', einsteinium).
element_name('Fm', fermium).
element_name('Md', mendelevium).
element_name('No', nobelium).
element_name('Lr', lawrencium).
element_name('Rf', rutherfordium).
element_name('Db', dubnium).
element_name('Sg', seaorgium).
element_name('Bh', bhorium).
element_name('Hs', hassium).
element_name('Mt', meitnerium).
element_name('Ds', darmstadtium).
element_name('Rg', roentgenium).
element_name('Cn', copernicium).
element_name('Nh', nihonium).
element_name('Fl', flerovium).
element_name('Mc', moscovium).
element_name('Lv', livermorium).
element_name('Ts', tennessine).
element_name('Og', oganesson).



metal("Na").
nonmetal("Cl").

%
%
% TODO: validate each element
%

extract_elements_from_formula(Formula, Elements) :-
    extract_elements_(Formula, 0, "", Elements).

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
    (is_upper(Out) -> Sth = [ConcatString]; Sth = []),
    append(End2, Sth, End).

    




