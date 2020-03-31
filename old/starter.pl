% Hydrogen isotopes
num_protons('H', 1).   % Hydrogen
num_protons('H-2', 1). % Deuterium
num_protons('H-3', 1). % Tritium
isotope(A, B) :-
    num_protons(A, Z1),
    num_protons(B, Z2),
    Z1==Z2.

% % compound(methane, chemical_element(hydrogen), chemical_element(carbon)).

% extract_elements(String, Elements) :-
%     write_ln("base"),
%     string_length(String, Length),
%     Length = 1,
%     Elements = [String],
%     !.

% extract_elements(String, Elements) :-
%     length(Elements, ListLength),
%     write_ln("2"),
%     write_ln(String),
%     string_length(String, Length),
%     SubLength is Length-1,
%     Length > 1,
%     sub_string(String, 0, SubLength, 1, OutString),
%     write_ln(OutString),
    
%     extract_elements(OutString, ElementsB),
%     append(ElementsB, [String], Elements),
%    % NaCl
    
%     % append(Output, [], Output2),
%     true.