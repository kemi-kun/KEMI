% Hydrogen isotopes
num_protons('H', 1).   % Hydrogen
num_protons('H-2', 1). % Deuterium
num_protons('H-3', 1). % Tritium
isotope(A, B) :-
    num_protons(A, Z1),
    num_protons(B, Z2),
    Z1==Z2.
