:- module(nomenclature,[iupac_name/2]).
:- use_module(organic,[organic_name/2]).
:- use_module(inorganic,[inorganic_name/2]).

%!  iupac_name(+Formula: string, +Name: string) is semidet.
%!  iupac_name(+Formula: string, -Name: string) is semidet.
%!  iupac_name(-Formula: string, +Name: string) is semidet.
%!  iupac_name(-Formula: string, -Name: string) is ERROR.
%
%   @arg Formula – the chemical formula of an element or a compound
%        Name – the IUPAC name of an element or a compound
%
iupac_name(Formula, Name) :-
    organic_name(Formula, Name);
    inorganic_name(Formula, Name).
