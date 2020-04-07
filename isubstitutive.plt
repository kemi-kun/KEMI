:- begin_tests(isubstitutive).
:- use_module(isubstitutive,[boron_hydride_structural_descriptor_name/2]).

test(boron_hydride_structural_descriptor_name):-
     % test + -
     boron_hydride_structural_descriptor_name("B2H4", A0),
     assertion(A0 == "closo-diborane(4)"),
     boron_hydride_structural_descriptor_name("B3H7", A1),
     assertion(A1 == "nido-triborane(7)"),
     boron_hydride_structural_descriptor_name("B2H8", A2),
     assertion(A2 == "arachno-diborane(8)"),
     boron_hydride_structural_descriptor_name("B2H10", A3),
     assertion(A3 == "hypho-diborane(10)"),
     boron_hydride_structural_descriptor_name("B2H12", A4),
     assertion(A4 == "klado-diborane(12)"),
  
     % test - +
     boron_hydride_structural_descriptor_name(B0, "closo-diborane(4)"),
     assertion(B0 == "B2H4"),
     boron_hydride_structural_descriptor_name(B1, "nido-triborane(7)"),
     assertion(B1 == "B3H7"),
     boron_hydride_structural_descriptor_name(B2, "arachno-diborane(8)"),
     assertion(B2 == "B2H8"),
     boron_hydride_structural_descriptor_name(B3, "hypho-diborane(10)"),
     assertion(B3 == "B2H10"),
     boron_hydride_structural_descriptor_name(B4, "klado-diborane(12)"),
     assertion(B4 == "B2H12"),
  
     % % test + +
     assertion(boron_hydride_structural_descriptor_name("B2H4", "closo-diborane(4)")),
     assertion(boron_hydride_structural_descriptor_name("B3H7", "nido-triborane(7)")),
     assertion(boron_hydride_structural_descriptor_name("B2H8", "arachno-diborane(8)")),
     assertion(boron_hydride_structural_descriptor_name("B2H10", "hypho-diborane(10)")),
     assertion(boron_hydride_structural_descriptor_name("B2H12", "klado-diborane(12)")),
     
     % % test fail case
     assertion(not(boron_hydride_structural_descriptor_name("B2H5", _))),
     assertion(not(boron_hydride_structural_descriptor_name("B2H7", _))),
     true.

:- end_tests(isubstitutive).