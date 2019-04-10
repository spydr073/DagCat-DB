-----------------------------------------------------------------------------------------[ Module ]
--{1
--                                                                              (\_/)
--                                                                              (o.O)
--                                                                              (> <)
--                                                                             #######
--                                                                           KILLER BUNNY
--                                                                             APPROVED

module UniProtTest.Organism

import Database.UniProt.Subject.Organism

%access export
%default total

%flag C "-O3"
%flag C "-g"

--}

---------------------------------------------------------------------------------------[ OS Field ]
--{1

os_tests : List (String, OS)
os_tests = [
(-- Test 1
"""OS   Escherichia coli.
""",
(MkOS "Escherichia coli" [])
),
(-- Test 2
"""OS   Homo sapiens (Human).
""",
(MkOS "Homo sapiens" [ "Human" ])
),
(-- Test 3
"""OS   Solanum melongena (Eggplant) (Aubergine).
""",
(MkOS "Solanum melongena" [ "Eggplant" , "Aubergine" ])
),
(-- Test 4
"""OS   Rous sarcoma virus (strain Schmidt-Ruppin A) (RSV-SRA) (Avian leukosis
OS   virus-RSA).
""",
(MkOS "Rous sarcoma virus" [ "strain Schmidt-Ruppin A"
                           , "RSV-SRA"
                           , "Avian leukosis virus-RSA"
                           ])
),
(-- Test 5
"""OS   Neurospora crassa (strain ATCC 24698 / 74-OR23-1A / CBS 708.71 / DSM
OS   1257 / FGSC 987).
""",
(MkOS "Neurospora crassa"
      [ "strain ATCC 24698 / 74-OR23-1A / CBS 708.71 / DSM 1257 / FGSC 987"])
),
(-- Test 6
"""OS   Halorhodospira halophila (strain DSM 244 / SL1) (Ectothiorhodospira
OS   halophila (strain DSM 244 / SL1)).
""",
(MkOS "Halorhodospira halophila"
      [ "strain DSM 244 / SL1"
      , "Ectothiorhodospira halophila (strain DSM 244 / SL1)"
      ])
)
]-- End

--}

---------------------------------------------------------------------------------------[ OG Field ]
--{1

og_tests : List (String, List OG)
og_tests = [
(-- Test 1
"""OG Hydrogenosome.
""",
[ Hydrogenosome ]
),
(-- Test 2
"""OG   Mitochondrion.
""",
[ Mitochondrion ]
),
(-- Test 3
"""OG   Nucleomorph.
""",
[ Nucleomorph ]
),
(-- Test 4
"""OG   Plasmid R6-5.
""",
[ Plasmid ["R6-5"] ]
),
(-- Test 5
"""OG   Plastid.
""",
[ Plastid PlastidSimple ]
),
(-- Test 6
"""OG   Plastid; Apicoplast.
""",
[ Plastid PlastidApicoplast ]
),
(-- Test 7
"""OG   Plastid; Chloroplast.
""",
[ Plastid PlastidChloroplast ]
),
(-- Test 8
"""OG   Plastid; Organellar chromatophore.
""",
[ Plastid PlastidOrganellarChromatophore ]
),
(-- Test 9
"""OG   Plastid; Cyanelle.
""",
[ Plastid PlastidCyanelle ]
),
(-- Test 10
"""OG   Plastid; Non-photosynthetic plastid.
""",
[ Plastid PlastidNonPhotosynthetic ]
),
(-- Test 11
"""OG   Plasmid R6-5, Plasmid IncFII R100 (NR1), and
OG   Plasmid IncFII R1-19 (R1 drd-19).
""",
[ Plasmid [ "R6-5"
          , "IncFII R100 (NR1)"
          , "IncFII R1-19 (R1 drd-19)"
          ]
]
)
]-- End

--}

---------------------------------------------------------------------------------------[ OC Field ]
--{1

oc_tests : List (String, OC)
oc_tests = [
(-- Test 1
"""OC   Eukaryota; Metazoa; Chordata; Craniata; Vertebrata; Euteleostomi;
OC   Mammalia; Eutheria; Euarchontoglires; Primates; Catarrhini; Hominidae;
OC   Homo.
""",
(MkOC [ "Eukaryota", "Metazoa", "Chordata", "Craniata"
      , "Vertebrata", "Euteleostomi", "Mammalia"
      , "Eutheria", "Euarchontoglires", "Primates"
      , "Catarrhini", "Hominidae", "Homo"
      ]
))
]-- End

--}

---------------------------------------------------------------------------------------[ OX Field ]
--{1

ox_tests : List (String, OX)
ox_tests = [
(-- Test 1
"""OX   NCBI_TaxID=9606;
""",
(MkOX "NCBI_TaxID" "9606")
),
(-- Test 2
"""OX   NCBI_TaxID=562;
""",
(MkOX "NCBI_TaxID" "562")
)
]-- End

--}

---------------------------------------------------------------------------------------[ OH Field ]
--{1

oh_tests : List (String, List OH)
oh_tests = [
(-- Test 1
"""OH   NCBI_TaxID=9481; Callithrix.
""",
[ MkOH ("NCBI_TaxID","9481") "Callithrix" [] ]
),
(-- Test 2
"""OH   NCBI_TaxID=9536; Cercopithecus hamlyni (Owl-faced monkey) (Hamlyn's monkey).
""",
[ MkOH ("NCBI_TaxID","9536") "Cercopithecus hamlyni"
       [ "Owl-faced monkey" , "Hamlyn's monkey" ]
]
),
(-- Test 3
"""OH   NCBI_TaxID=9539; Macaca (macaques).
""",
[ MkOH ("NCBI_TaxID","9539") "Macaca" ["macaques"] ]
),
(-- Test 4
"""OH   NCBI_TaxID=9598; Pan troglodytes (Chimpanzee).
""",
[ MkOH ("NCBI_TaxID","9598") "Pan troglodytes" ["Chimpanzee"] ]
)
]-- End

--}


