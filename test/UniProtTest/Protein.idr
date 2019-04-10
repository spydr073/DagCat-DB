-----------------------------------------------------------------------------------------[ Module ]
--{1
--                                                                              (\_/)
--                                                                              (o.O)
--                                                                              (> <)
--                                                                             #######
--                                                                           KILLER BUNNY
--                                                                             APPROVED

module UniProtTest.Protein

import Database.UniProt.Subject.Protein

%access export
%default total

%flag C "-O3"
%flag C "-g"

--}

---------------------------------------------------------------------------------------[ ID Field ]
--{1

id_tests : List (String, UID)
id_tests = [
(-- Test 1
"""ID   NA228_NEMVE             Reviewed;          85 AA.
""",
(MkUID "NA228_NEMVE" Reviewed 85)
),
(-- Test 2
"""ID   CYC_BOVIN               Reviewed;         104 AA.
"""",
(MkUID "CYC_BOVIN" Reviewed 104)
),
(-- Test 3
"""ID   GIA2_GIALA              Reviewed;         296 AA.
""",
(MkUID "GIA2_GIALA" Reviewed 296)
),
(-- Test 4
"""ID   Q5JU06_HUMAN            Unreviewed;       268 AA.
""",
(MkUID "Q5JU06_HUMAN" Unreviewed 268)
)
]-- End

--}

---------------------------------------------------------------------------------------[ AC Field ]
--{1

ac_tests : List (String, AC)
ac_tests = [
(-- Test 1
"""AC   B1NWR6; A7SCE0; B1NWR8; B1NWR9; B1NWS2; B1NWS7; B1NWS9; B5L633;
AC   B5L634;
""",
(MkAC ["B1NWR6", "A7SCE0", "B1NWR8", "B1NWR9", "B1NWS2", "B1NWS7", "B1NWS9",
       "B5L633", "B5L634"
      ])
),
(-- Test 2
"""AC   P00321;
""",
(MkAC ["P00321"])
),
(-- Test 3
"""AC   Q16653; O00713; O00714; O00715; Q13054; Q13055; Q14855; Q92891;
AC   Q92892; Q92893; Q92894; Q92895; Q93053; Q96KU9; Q96KV0; Q96KV1;
AC   Q99605;
""",
(MkAC ["Q16653", "O00713", "O00714", "O00715", "Q13054", "Q13055", "Q14855",
       "Q92891", "Q92892", "Q92893", "Q92894", "Q92895", "Q93053", "Q96KU9",
       "Q96KV0", "Q96KV1", "Q99605"
      ])
)
]-- End

--}

---------------------------------------------------------------------------------------[ DT Field ]
--{1

dt_tests : List (String, DT)
dt_tests = [
(-- Test 1
"""DT   05-OCT-2010, integrated into UniProtKB/Swiss-Prot.
DT   05-OCT-2010, sequence version 2.
DT   16-JAN-2019, entry version 34.
""",
(MkDT "05-OCT-2010" "Swiss-Prot"
      "05-OCT-2010" 2
      "16-JAN-2019" 34)
),
(-- Test 2
"""DT   01-OCT-1996, integrated into UniProtKB/Swiss-Prot.
DT   01-OCT-1996, sequence version 1.
DT   07-FEB-2006, entry version 49.
""",
(MkDT "01-OCT-1996" "Swiss-Prot"
      "01-OCT-1996" 1
      "07-FEB-2006" 49)
),
(-- Test 3
"""DT   01-FEB-1999, integrated into UniProtKB/TrEMBL.
DT   15-OCT-2000, sequence version 2.
DT   15-DEC-2004, entry version 5.
""",
(MkDT "01-FEB-1999" "TrEMBL"
      "15-OCT-2000" 2
      "15-DEC-2004" 5)
),
(-- Test 4
"""DT   25-OCT-2005, integrated into UniProtKB/Swiss-Prot.
DT   01-NOV-1996, sequence version 1.
DT   07-FEB-2006, entry version 35.
""",
(MkDT "25-OCT-2005" "Swiss-Prot"
      "01-NOV-1996" 1
      "07-FEB-2006" 35)
)
]-- End

--}

---------------------------------------------------------------------------------------[ DE Field ]
--{1

de_tests : List (String, DE)
de_tests = [
(-- Test 1
"""DE   RecName: Full=Annexin A5;
DE            Short=Annexin-5;
DE   AltName: Full=Annexin V;
DE   AltName: Full=Lipocortin V;
DE   AltName: Full=Endonexin II;
DE   AltName: Full=Calphobindin I;
DE   AltName: Full=CBP-I;
DE   AltName: Full=Placental anticoagulant protein I;
DE            Short=PAP-I;
DE   AltName: Full=PP4;
DE   AltName: Full=Thromboplastin inhibitor;
DE   AltName: Full=Vascular anticoagulant-alpha;
DE            Short=VAC-alpha;
DE   AltName: Full=Anchorin CII;
""",
(MkDE (Just $ MkName "Annexin A5" ["Annexin-5"] [])
      [ Simple $ MkName "Annexin V" [] []
      , Simple $ MkName "Lipocortin V" [] []
      , Simple $ MkName "Endonexin II" [] []
      , Simple $ MkName "Calphobindin I" [] []
      , Simple $ MkName "CBP-I" [] []
      , Simple $ MkName "Placental anticoagulant protein I"
                        ["PAP-I"] []
      , Simple $ MkName "PP4" [] []
      , Simple $ MkName "Thromboplastin inhibitor" [] []
      , Simple $ MkName "Vascular anticoagulant-alpha"
                             ["VAC-alpha"] []
      , Simple $ MkName "Anchorin CII" [] []
      ]
      []
      []
      []
      []
)),
(-- Test 2
"""DE   RecName: Full=Granulocyte colony-stimulating factor;
DE            Short=G-CSF;
DE   AltName: Full=Pluripoietin;
DE   AltName: Full=Filgrastim;
DE   AltName: Full=Lenograstim;
DE   Flags: Precursor;
""",
(MkDE (Just $ MkName "Granulocyte colony-stimulating factor" ["G-CSF"] [])
      [ Simple $ MkName "Pluripoietin" [] []
      , Simple $ MkName "Filgrastim"   [] []
      , Simple $ MkName "Lenograstim"  [] []
      ]
      []
      []
      []
      [ Precursor ]
)),
(-- Test 3
"""DE   RecName: Full=Corticotropin-lipotropin;
DE   AltName: Full=Pro-opiomelanocortin;
DE            Short=POMC;
DE   Contains:
DE     RecName: Full=NPP;
DE   Contains:
DE     RecName: Full=Melanotropin gamma;
DE     AltName: Full=Gamma-MSH;
DE   Contains:
DE     RecName: Full=Potential peptide;
DE   Contains:
DE     RecName: Full=Corticotropin;
DE     AltName: Full=Adrenocorticotropic hormone;
DE              Short=ACTH;
DE   Contains:
DE     RecName: Full=Melanotropin alpha;
DE     AltName: Full=Alpha-MSH;
DE   Contains:
DE     RecName: Full=Corticotropin-like intermediary peptide;
DE              Short=CLIP;
DE   Contains:
DE     RecName: Full=Lipotropin beta;
DE     AltName: Full=Beta-LPH;
DE   Contains:
DE     RecName: Full=Lipotropin gamma;
DE     AltName: Full=Gamma-LPH;
DE   Contains:
DE     RecName: Full=Melanotropin beta;
DE     AltName: Full=Beta-MSH;
DE   Contains:
DE     RecName: Full=Beta-endorphin;
DE   Contains:
DE     RecName: Full=Met-enkephalin;
DE   Flags: Precursor;
""",
(MkDE (Just $ MkName "Corticotropin-lipotropin" [] [])
      [ Simple $ MkName "Pro-opiomelanocortin" ["POMC"] [] ]
      []
      []
      [ ( Just $ MkName "NPP" [] []
        , []
        , []
        )
      , ( Just $ MkName "Melanotropin gamma" [] []
        , [ Simple $ MkName "Gamma-MSH" [] [] ]
        , []
        )
      , ( Just $ MkName "Potential peptide" [] []
        , []
        , []
        )
      , ( Just $ MkName "Corticotropin" [] []
        , [ Simple $ MkName "Adrenocorticotropic hormone"
                            ["ACTH"]
                            []
          ]
        , []
        )
      , ( Just $ MkName "Melanotropin alpha" [] []
        , [ Simple $ MkName "Alpha-MSH" [] [] ]
        , []
        )
      , ( Just $ MkName "Corticotropin-like intermediary peptide"
                         [ "CLIP" ]
                         []
        , []
        , []
        )
      , ( Just $ MkName "Lipotropin beta" [] []
        , [ Simple $ MkName "Beta-LPH" [] [] ]
        , []
        )
      , ( Just $ MkName "Lipotropin gamma" [] []
        , [ Simple $ MkName "Gamma-LPH" [] [] ]
        , []
        )
      , ( Just $ MkName "Melanotropin beta" [] []
        , [ Simple $ MkName "Beta-MSH" [] [] ]
        , []
        )
      , ( Just $ MkName "Beta-endorphin" [] []
        , []
        , []
        )
      , ( Just $ MkName "Met-enkephalin" [] []
        , []
        , []
        )
      ]
      [ Precursor ]

)),
(-- Test 4
"""DE   RecName: Full=CAD protein;
DE   Includes:
DE     RecName: Full=Glutamine-dependent carbamoyl-phosphate synthase;
DE              EC=6.3.5.5;
DE   Includes:
DE     RecName: Full=Aspartate carbamoyltransferase;
DE              EC=2.1.3.2;
DE   Includes:
DE     RecName: Full=Dihydroorotase;
DE              EC=3.5.2.3;
""",
(MkDE (Just $ MkName "CAD protein" [] [])
      []
      []
      [ (Just $ MkName
            "Glutamine-dependent carbamoyl-phosphate synthase"
            []
            ["6.3.5.5" ]
        , []
        , []
        )
      , (Just $ MkName "Aspartate carbamoyltransferase"
                       []
                       ["2.1.3.2"]
        , []
        , []
        )
      , (Just $ MkName "Dihydroorotase" [] ["3.5.2.3"]
        , []
        , []
        )
      ]
      []
      []
)),
(-- Test 5
"""DE   RecName: Full=Arginine biosynthesis bifunctional protein argJ;
DE   Includes:
DE     RecName: Full=Glutamate N-acetyltransferase;
DE              EC=2.3.1.35;
DE     AltName: Full=Ornithine acetyltransferase;
DE              Short=OATase;
DE     AltName: Full=Ornithine transacetylase;
DE   Includes:
DE     RecName: Full=Amino-acid acetyltransferase;
DE              EC=2.3.1.1;
DE     AltName: Full=N-acetylglutamate synthase;
DE              Short=AGS;
DE   Contains:
DE     RecName: Full=Arginine biosynthesis bifunctional protein argJ alpha chain;
DE   Contains:
DE     RecName: Full=Arginine biosynthesis bifunctional protein argJ beta chain;
""",
(MkDE (Just $ MkName "Arginine biosynthesis bifunctional protein argJ" [] [])
      []
      []
      [ ( Just $ MkName "Glutamate N-acetyltransferase" [] ["2.3.1.35"]
        , [ Simple $ MkName "Ornithine acetyltransferase" ["OATase"] []
          , Simple $ MkName "Ornithine transacetylase" [] []
          ]
        , []
        )
      , ( Just $ MkName "Amino-acid acetyltransferase" [] ["2.3.1.1"]
        , [ Simple $ MkName "N-acetylglutamate synthase" ["AGS"] []]
        , []
        )
      ]
      [ ( Just $ MkName "Arginine biosynthesis bifunctional protein argJ alpha chain" [] []
                        , []
                        , []
                        )
      , ( Just $ MkName "Arginine biosynthesis bifunctional protein argJ beta chain" [] []
                        , []
                        , []
                        )
      ]
      []
)),
(-- Test 6
"""DE   RecName: Full=Chondroitin proteoglycan 3;
DE   Flags: Precursor;
""",
(MkDE (Just $ MkName "Chondroitin proteoglycan 3" [] [])
      []
      []
      []
      []
      [ Precursor ]
)),
(-- Test 7
"""DE   RecName: Full=Dihydrodipicolinate reductase;
DE            Short=DHPR;
DE            EC=1.3.1.26;
DE   Flags: Fragment;
""",
(MkDE (Just $ MkName "Dihydrodipicolinate reductase" ["DHPR"] ["1.3.1.26"])
      []
      []
      []
      []
      [ Fragment ]
))
]-- End

--}

---------------------------------------------------------------------------------------[ GN Field ]
--{1

gn_tests : List (String, List GN)
gn_tests = [
(-- Test 1
"""GN   Name=FBA1; OrderedLocusNames=CAALFM_C401750CA;
GN   ORFNames=CaO19.12088, CaO19.4618;
GN   and
GN   Name=Jon99Ciii; Synonyms=SER2, SER5, Ser99Db; ORFNames=CG15519;
GN   and
GN   Name=hns; Synonyms=bglY, cur, drdX, hnsA, msyA, osmZ, pilG, topS;
GN   OrderedLocusNames=b1237, c1701, z2013, ECs1739;
""",
[ MkGN (Just "FBA1")
       []
       [ "CAALFM_C401750CA" ]
       [ "CaO19.12088", "CaO19.4618" ]
, MkGN (Just "Jon99Ciii")
       ["SER2", "SER5", "Ser99Db"]
       []
       ["CG15519"]

, MkGN (Just "hns")
       [ "bglY", "cur", "drdX", "hnsA", "msyA"
       , "osmZ", "pilG", "topS" ]
       [ "b1237", "c1701", "z2013", "ECs1739" ]
       []
]
),
(-- Test 2
"""GN   ORFNames=FV3-001R;
""",
[MkGN Nothing [] [] ["FV3-001R"]]
),
(-- Test 3
"""GN   Name=FBA1; OrderedLocusNames=CAALFM_C401750CA;
GN   ORFNames=CaO19.12088, CaO19.4618;
GN   and
GN   Name=Jon99Cii; Synonyms=SER1, SER5, Ser99Da; ORFNames=CG7877;
GN   and
GN   Name=Jon99Cii;
""",
[ MkGN (Just "FBA1")
       []
       [ "CAALFM_C401750CA" ]
       [ "CaO19.12088", "CaO19.4618" ]
, MkGN (Just "Jon99Cii")
       ["SER1", "SER5", "Ser99Da" ]
       []
       ["CG7877"]
, MkGN (Just "Jon99Cii")
       []
       []
       []
]
),
(-- Test 4
"""GN   Name=Jon99Cii; Synonyms=SER1, SER5, Ser99Da; ORFNames=CG7877;
GN   and
GN   Name=Jon99Ciii; Synonyms=SER2, SER5, Ser99Db; ORFNames=CG15519;
""",
[ MkGN (Just "Jon99Cii")
       ["SER1", "SER5", "Ser99Da" ]
       []
       ["CG7877"]
, MkGN (Just "Jon99Ciii")
       ["SER2", "SER5", "Ser99Db"]
       []
       ["CG15519"]
]
),
(-- Test 5
"""GN   Name=hns; Synonyms=bglY, cur, drdX, hnsA, msyA, osmZ, pilG, topS;
GN   OrderedLocusNames=b1237, c1701, z2013, ECs1739;
""",
[ MkGN (Just "hns")
       [ "bglY", "cur", "drdX", "hnsA", "msyA" , "osmZ", "pilG", "topS" ]
       [ "b1237", "c1701", "z2013", "ECs1739" ]
       []
]
),
(-- Test 6
"""GN   Name=ACTIA;
GN   and
GN   Name=ACTIB;
""",
[ MkGN (Just "ACTIA") [] [] []
, MkGN (Just "ACTIB") [] [] []
]
)
]-- End

--}


