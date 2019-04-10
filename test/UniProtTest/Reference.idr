-----------------------------------------------------------------------------------------[ Module ]
--{1
--                                                                              (\_/)
--                                                                              (o.O)
--                                                                              (> <)
--                                                                             #######
--                                                                           KILLER BUNNY
--                                                                             APPROVED

module UniProtTest.Reference

import Database.UniProt.Subject.Reference

%access export
%default total

%flag C "-O3"
%flag C "-g"

--}

---------------------------------------------------------------------------------------[ RN Field ]
--{1

rn_tests : List (String, Int)
rn_tests = [
(-- Test 1
"RN [1]\n",
1
),
(-- Test 2
"RN [2]\n",
2
)
]-- End

--}

---------------------------------------------------------------------------------------[ RP Field ]
--{1

rp_tests : List (String, String)
rp_tests = [
(-- Test 1
"""RP   NUCLEOTIDE SEQUENCE [MRNA] (ISOFORMS A AND C), FUNCTION, INTERACTION
RP   WITH PKC-3, SUBCELLULAR LOCATION, TISSUE SPECIFICITY, DEVELOPMENTAL
RP   STAGE, AND MUTAGENESIS OF PHE-175 AND PHE-221.
""",
(Foldable.concat . List.intersperse " ")
  [ "NUCLEOTIDE SEQUENCE [MRNA] (ISOFORMS A AND C), FUNCTION, INTERACTION"
  , "WITH PKC-3, SUBCELLULAR LOCATION, TISSUE SPECIFICITY, DEVELOPMENTAL"
  , "STAGE, AND MUTAGENESIS OF PHE-175 AND PHE-221."
  ]
)
]-- End

--}

---------------------------------------------------------------------------------------[ RC Field ]
--{1

rc_tests : List (String, List (RCom, String))
rc_tests = [
(-- Test 1
"""RC   STRAIN=Bristol N2; PLASMID=R1 (R7268);
RC   TRANSPOSON=Tn3; STRAIN=AL.012, AZ.026, AZ.180, DC.005, GA.039, GA2181, IL.014, IL2.17,
RC   IN.018, KY.172, KY2.37, LA.013, MI.035, MN.001, MNb027, MS.040,
RC   NY.016, OH.036, TN.173, TN2.38, UT.002, and VA.015;
""",
[ (STRAIN     , "Bristol N2")
, (PLASMID    , "R1 (R7268)")
, (TRANSPOSON , "Tn3")
, (STRAIN, "AL.012, AZ.026, AZ.180, DC.005, GA.039, GA2181, IL.014, IL2.17, " ++
           "IN.018, KY.172, KY2.37, LA.013, MI.035, MN.001, MNb027, MS.040, " ++
           "NY.016, OH.036, TN.173, TN2.38, UT.002, and VA.015")
]
)
]-- End

--}

---------------------------------------------------------------------------------------[ RX Field ]
--{1

rx_tests : List (String, List (BibDB, String))
rx_tests = [
(-- Test 1
"""RX   PubMed=11134024; DOI=10.1074/jbc.M008990200;
RX   PubMed=11134024; DOI=10.1074/jbc.M008990200;
""",
[ (PubMed , "11134024")
, (DOI    , "10.1074/jbc.M008990200")
, (PubMed , "11134024")
, (DOI    , "10.1074/jbc.M008990200")
]
),
(-- Test 2
"""RX   DOI=10.1002/(SICI)1098-2795(199708)47:4<370::AID-MRD3>3.0.CO;2-H;
""",
[ (DOI, "10.1002/(SICI)1098-2795(199708)47:4<370::AID-MRD3>3.0.CO;2-H")
]
)
]-- End

--}

---------------------------------------------------------------------------------------[ RG Field ]
--{1

rg_tests : List (String, List String)
rg_tests = [
(-- Test 1
"""RG   The mouse genome sequencing consortium;
RG   The mouse genome sequencing consortium; The mouse genome
RG   sequencing consortium;
""",
[ "The mouse genome sequencing consortium"
, "The mouse genome sequencing consortium"
, "The mouse genome sequencing consortium"
]
)
]-- End

--}

---------------------------------------------------------------------------------------[ RA Field ]
--{1

ra_tests : List (String, List String)
ra_tests = [
(-- Test 1
"""RA   Zhang L., Wu S.-L., Rubin C.S.,
RA   Zhang L., Wu S.-L., Rubin C.S. ;
""",
[ "Zhang L." , "Wu S.-L.", "Rubin C.S."
, "Zhang L." , "Wu S.-L.", "Rubin C.S."
]
)
]-- End

--}

---------------------------------------------------------------------------------------[ RT Field ]
--{1

rt_tests : List (String, String)
rt_tests = [
(-- Test 1
"""RT   "A novel adapter protein employs a phosphotyrosine binding domain and
RT   exceptionally basic N-terminal domains to capture and localize an
RT   atypical protein kinase C: characterization of Caenorhabditis elegans
RT   C kinase adapter 1, a protein that avidly binds protein kinase C3.";
""",
(Foldable.concat . List.intersperse " ")
  [ "A novel adapter protein employs a phosphotyrosine binding domain and"
  , "exceptionally basic N-terminal domains to capture and localize an"
  , "atypical protein kinase C: characterization of Caenorhabditis elegans"
  , "C kinase adapter 1, a protein that avidly binds protein kinase C3."
  ]
)
]-- End

--}

---------------------------------------------------------------------------------------[ RL Field ]
--{1

rl_tests : List (String, String)
rl_tests = [
(-- Test 1
"""RL   J. Biol. Chem. 276:10463-10475(2001).
RL   J. Biol. Chem. 276:10463-10475(2001).
RL   (In) Magnusson S., Ottesen M., Foltmann B., Dano K., Neurath H.
RL   (eds.);
RL   Regulatory proteolytic enzymes and their inhibitors, pp.163-172,
RL   Pergamon Press, New York (1978).
""",
(Foldable.concat . List.intersperse " ")
  [ "J. Biol. Chem. 276:10463-10475(2001)."
  , "J. Biol. Chem. 276:10463-10475(2001)."
  , "(In) Magnusson S., Ottesen M., Foltmann B., Dano K., Neurath H. (eds.);"
  , "Regulatory proteolytic enzymes and their inhibitors, pp.163-172, " ++
    "Pergamon Press, New York (1978)."
  ]
)
]-- End

--}



