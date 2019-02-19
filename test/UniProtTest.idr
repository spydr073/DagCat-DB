-----------------------------------------------------------------------------------------[ Module ]
--{1
--                                                                              (\_/)
--                                                                              (o.O)
--                                                                              (> <)
--                                                                             #######
--                                                                           KILLER BUNNY
--                                                                             APPROVED

module UniProtTest

import Database.UniProt.Types
import Database.UniProt.Parser

import Testing.Unit

%flag C "-O3"
%flag C "-g"

--}

---------------------------------------------------------------------------------------[ ID Field ]
--{1

id_test_1 : String
id_test_1 = "ID   NA228_NEMVE             Reviewed;          85 AA.\n"

id_test_2 :  String
id_test_2 = "ID   CYC_BOVIN               Reviewed;         104 AA.\n"

id_test_3 :  String
id_test_3 = "ID   GIA2_GIALA              Reviewed;         296 AA.\n"

id_test_4 :  String
id_test_4 = "ID   Q5JU06_HUMAN            Unreviewed;       268 AA.\n"

runTest_id : IO ()
runTest_id = do
  putStrLn "ID Tests:"
  case parse parseID id_test_1 of
    Left err => putStrLn err
    Right v  => assertEq "Test 1" (MkUID "NA228_NEMVE" Reviewed 85) v
  case parse parseID id_test_2 of
    Left err => putStrLn err
    Right v  => assertEq "Test 2" (MkUID "CYC_BOVIN" Reviewed 104) v
  case parse parseID id_test_3 of
    Left err => putStrLn err
    Right v  => assertEq "Test 3" (MkUID "GIA2_GIALA" Reviewed 296) v
  case parse parseID id_test_4 of
    Left err => putStrLn err
    Right v  => assertEq "Test 4" (MkUID "Q5JU06_HUMAN" Unreviewed 268) v

--}

---------------------------------------------------------------------------------------[ AC Field ]
--{1

ac_test_1 : String
ac_test_1 = """AC   B1NWR6; A7SCE0; B1NWR8; B1NWR9; B1NWS2; B1NWS7; B1NWS9; B5L633;
AC   B5L634;
"""

ac_test_2 : String
ac_test_2 = "AC   P00321;\n"

ac_test_3 : String
ac_test_3  = """AC   Q16653; O00713; O00714; O00715; Q13054; Q13055; Q14855; Q92891;
AC   Q92892; Q92893; Q92894; Q92895; Q93053; Q96KU9; Q96KV0; Q96KV1;
AC   Q99605;
"""

runTest_ac : IO ()
runTest_ac = do
  putStrLn "AC Tests:"
  case parse parseAC ac_test_1 of
    Left err => putStrLn err
    Right v  => assertEq "Test 1" (MkAC ["B1NWR6", "A7SCE0", "B1NWR8", "B1NWR9",
                                         "B1NWS2", "B1NWS7", "B1NWS9", "B5L633",
                                         "B5L634"
                                        ]) v
  case parse parseAC ac_test_2 of
    Left err => putStrLn err
    Right v  => assertEq "Test 2" (MkAC ["P00321"]) v
  case parse parseAC ac_test_3 of
    Left err => putStrLn err
    Right v  => assertEq "Test 3" (MkAC ["Q16653", "O00713", "O00714", "O00715",
                                         "Q13054", "Q13055", "Q14855", "Q92891",
                                         "Q92892", "Q92893", "Q92894", "Q92895",
                                         "Q93053", "Q96KU9", "Q96KV0", "Q96KV1",
                                         "Q99605"
                                        ]) v

--}

---------------------------------------------------------------------------------------[ DT Field ]
--{1

dt_test_1 : String
dt_test_1 = """DT   05-OCT-2010, integrated into UniProtKB/Swiss-Prot.
DT   05-OCT-2010, sequence version 2.
DT   16-JAN-2019, entry version 34.
"""

dt_test_2 : String
dt_test_2 = """DT   01-OCT-1996, integrated into UniProtKB/Swiss-Prot.
DT   01-OCT-1996, sequence version 1.
DT   07-FEB-2006, entry version 49.
"""

dt_test_3 : String
dt_test_3 = """DT   01-FEB-1999, integrated into UniProtKB/TrEMBL.
DT   15-OCT-2000, sequence version 2.
DT   15-DEC-2004, entry version 5.
"""

dt_test_4 : String
dt_test_4 = """DT   25-OCT-2005, integrated into UniProtKB/Swiss-Prot.
DT   01-NOV-1996, sequence version 1.
DT   07-FEB-2006, entry version 35.
"""

runTest_dt : IO ()
runTest_dt = do
  putStrLn "DT Tests:"
  case parse parseDT dt_test_1 of
    Left err => putStrLn err
    Right v  => assertEq "Test 1" (MkDT "05-OCT-2010" "Swiss-Prot"
                                        "05-OCT-2010" 2
                                        "16-JAN-2019" 34) v
  case parse parseDT dt_test_2 of
    Left err => putStrLn err
    Right v  => assertEq "Test 2" (MkDT "01-OCT-1996" "Swiss-Prot"
                                        "01-OCT-1996" 1
                                        "07-FEB-2006" 49) v
  case parse parseDT dt_test_3 of
    Left err => putStrLn err
    Right v  => assertEq "Test 3" (MkDT "01-FEB-1999" "TrEMBL"
                                        "15-OCT-2000" 2
                                        "15-DEC-2004" 5) v
  case parse parseDT dt_test_4 of
    Left err => putStrLn err
    Right v  => assertEq "Test 4" (MkDT "25-OCT-2005" "Swiss-Prot"
                                        "01-NOV-1996" 1
                                        "07-FEB-2006" 35) v

--}

---------------------------------------------------------------------------------------[ DE Field ]
--{1

de_test_1 : String
de_test_1 = """DE   RecName: Full=Annexin A5;
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
"""

de_test_2 : String
de_test_2 = """DE   RecName: Full=Granulocyte colony-stimulating factor;
DE            Short=G-CSF;
DE   AltName: Full=Pluripoietin;
DE   AltName: Full=Filgrastim;
DE   AltName: Full=Lenograstim;
DE   Flags: Precursor;
"""

de_test_3 : String
de_test_3 = """DE   RecName: Full=Corticotropin-lipotropin;
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
"""

de_test_4 : String
de_test_4 = """DE   RecName: Full=CAD protein;
DE   Includes:
DE     RecName: Full=Glutamine-dependent carbamoyl-phosphate synthase;
DE              EC=6.3.5.5;
DE   Includes:
DE     RecName: Full=Aspartate carbamoyltransferase;
DE              EC=2.1.3.2;
DE   Includes:
DE     RecName: Full=Dihydroorotase;
DE              EC=3.5.2.3;
"""

de_test_5 : String
de_test_5 = """DE   RecName: Full=Arginine biosynthesis bifunctional protein argJ;
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
"""

de_test_6 : String
de_test_6 = """DE   RecName: Full=Chondroitin proteoglycan 3;
DE   Flags: Precursor;
"""

de_test_7 : String
de_test_7 = """DE   RecName: Full=Dihydrodipicolinate reductase;
DE            Short=DHPR;
DE            EC=1.3.1.26;
DE   Flags: Fragment;
"""


runTest_de : IO ()
runTest_de = do
  putStrLn "DE Tests:"
  case parse parseDE de_test_1 of
    Left err => putStrLn err
    Right v  => assertEq "Test 1" (MkDE (Just $ MkName "Annexin A5" ["Annexin-5"] [])
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
                                        [] [] [] []
                                  ) v
  case parse parseDE de_test_2 of
    Left err => putStrLn err
    Right v  => assertEq "Test 2" (MkDE (Just $ MkName "Granulocyte colony-stimulating factor"
                                                       ["G-CSF"] [])
                                        [ Simple $ MkName "Pluripoietin" [] []
                                        , Simple $ MkName "Filgrastim" [] []
                                        , Simple $ MkName "Lenograstim" [] []
                                        ]
                                        []
                                        []
                                        []
                                        [ Precursor ]
                                  ) v
  case parse parseDE de_test_3 of
    Left err => putStrLn err
    Right v  => assertEq "Test 3" (MkDE (Just $ MkName "Corticotropin-lipotropin" [] [])
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
                                  ) v
  case parse parseDE de_test_4 of
    Left err => putStrLn err
    Right v  => assertEq "Test 4" (MkDE (Just $ MkName "CAD protein" [] [])
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
                                  ) v
  case parse parseDE de_test_5 of
    Left err => putStrLn err
    Right v  => assertEq "Test 5" (MkDE (Just $ MkName
                                               "Arginine biosynthesis bifunctional protein argJ"
                                               [] [])
                                        []
                                        []
                                        [ ( Just $ MkName "Glutamate N-acetyltransferase"
                                                          []
                                                          ["2.3.1.35"]
                                          , [ Simple $ MkName "Ornithine acetyltransferase"
                                                              ["OATase"] []
                                            , Simple $ MkName "Ornithine transacetylase" [] []
                                            ]
                                          , []
                                          )
                                        , ( Just $ MkName "Amino-acid acetyltransferase"
                                                          []
                                                          ["2.3.1.1"]
                                          , [ Simple $ MkName "N-acetylglutamate synthase"
                                                              ["AGS"] []]
                                          , []
                                          )
                                        ]
                                        [ ( Just $ MkName
                            "Arginine biosynthesis bifunctional protein argJ alpha chain" [] []
                                          , []
                                          , []
                                          )
                                        , ( Just $ MkName
                            "Arginine biosynthesis bifunctional protein argJ beta chain" [] []
                                          , []
                                          , []
                                          )
                                        ]
                                        []
                                  ) v
  case parse parseDE de_test_6 of
    Left err => putStrLn err
    Right v  => assertEq "Test 6" (MkDE (Just $ MkName "Chondroitin proteoglycan 3" [] [])
                                         []
                                         []
                                         []
                                         []
                                         [ Precursor ]
                                  ) v
  case parse parseDE de_test_7 of
    Left err => putStrLn err
    Right v  => assertEq "Test 7" (MkDE (Just $ MkName "Dihydrodipicolinate reductase"
                                                       ["DHPR"]
                                                       ["1.3.1.26"]
                                        )
                                        []
                                        []
                                        []
                                        []
                                        [ Fragment ]
                                  ) v

--}

---------------------------------------------------------------------------------------[ GN Field ]
--{1

gn_test_1 : String
gn_test_1 = """GN   Name=FBA1; OrderedLocusNames=CAALFM_C401750CA;
GN   ORFNames=CaO19.12088, CaO19.4618;
GN   and
GN   Name=Jon99Ciii; Synonyms=SER2, SER5, Ser99Db; ORFNames=CG15519;
GN   and
GN   Name=hns; Synonyms=bglY, cur, drdX, hnsA, msyA, osmZ, pilG, topS;
GN   OrderedLocusNames=b1237, c1701, z2013, ECs1739;
"""

gn_test_2 : String
gn_test_2 = """GN   ORFNames=FV3-001R;
"""

gn_test_3 : String
gn_test_3 = """GN   Name=FBA1; OrderedLocusNames=CAALFM_C401750CA;
GN   ORFNames=CaO19.12088, CaO19.4618;
GN   and
GN   Name=Jon99Cii; Synonyms=SER1, SER5, Ser99Da; ORFNames=CG7877;
GN   and
GN   Name=Jon99Cii;
"""

gn_test_4 : String
gn_test_4 = """GN   Name=Jon99Cii; Synonyms=SER1, SER5, Ser99Da; ORFNames=CG7877;
GN   and
GN   Name=Jon99Ciii; Synonyms=SER2, SER5, Ser99Db; ORFNames=CG15519;
"""

gn_test_5 : String
gn_test_5 = """GN   Name=hns; Synonyms=bglY, cur, drdX, hnsA, msyA, osmZ, pilG, topS;
GN   OrderedLocusNames=b1237, c1701, z2013, ECs1739;
"""

runTest_gn : IO ()
runTest_gn = do
  putStrLn "DT Tests:"
  case parse parseGN gn_test_1 of
    Left err => putStrLn err
    Right v  => assertEq "Test 1" ([ MkGN (Just "FBA1")
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
                                   ]) v
  case parse parseGN gn_test_2 of
    Left err => putStrLn err
    Right v  => assertEq "Test 2" ([MkGN Nothing [] [] ["FV3-001R"]]) v
  case parse parseGN gn_test_3 of
    Left err => putStrLn err
    Right v  => assertEq "Test 3" ([ MkGN (Just "FBA1")
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
                                   ]) v
  case parse parseGN gn_test_4 of
    Left err => putStrLn err
    Right v  => assertEq "Test 4" ([ MkGN (Just "Jon99Cii")
                                          ["SER1", "SER5", "Ser99Da" ]
                                          []
                                          ["CG7877"]
                                   , MkGN (Just "Jon99Ciii")
                                          ["SER2", "SER5", "Ser99Db"]
                                          []
                                          ["CG15519"]
                                   ]) v
  case parse parseGN gn_test_5 of
    Left err => putStrLn err
    Right v  => assertEq "Test 5" ([MkGN (Just "hns")
                                          [ "bglY", "cur", "drdX", "hnsA", "msyA"
                                          , "osmZ", "pilG", "topS" ]
                                          [ "b1237", "c1701", "z2013", "ECs1739" ]
                                          []
                                   ]) v

--}

---------------------------------------------------------------------------------------[ OS Field ]
--{1

os_test_1 : String
os_test_1 = "OS   Escherichia coli.\n"

os_test_2 : String
os_test_2 = "OS   Homo sapiens (Human).\n"

os_test_3 : String
os_test_3 = "OS   Solanum melongena (Eggplant) (Aubergine).\n"

os_test_4 : String
os_test_4 = """OS   Rous sarcoma virus (strain Schmidt-Ruppin A) (RSV-SRA) (Avian leukosis
OS   virus-RSA).
"""

os_test_5 : String
os_test_5 = """OS   Neurospora crassa (strain ATCC 24698 / 74-OR23-1A / CBS 708.71 / DSM
OS   1257 / FGSC 987).
"""

os_test_6 : String
os_test_6 = """OS   Halorhodospira halophila (strain DSM 244 / SL1) (Ectothiorhodospira
OS   halophila (strain DSM 244 / SL1)).
"""

runTest_os : IO ()
runTest_os = do
  putStrLn "OS Tests:"
  case parse parseOS os_test_1 of
    Left err => putStrLn err
    Right v  => assertEq "Test 1" (MkOS "Escherichia coli" []) v
  case parse parseOS os_test_2 of
    Left err => putStrLn err
    Right v  => assertEq "Test 2" (MkOS "Homo sapiens" [ "Human" ]) v
  case parse parseOS os_test_3 of
    Left err => putStrLn err
    Right v  => assertEq "Test 3" (MkOS "Solanum melongena"
                                         [ "Eggplant"
                                         , "Aubergine"
                                         ]) v
  case parse parseOS os_test_4 of
    Left err => putStrLn err
    Right v  => assertEq "Test 4" (MkOS "Rous sarcoma virus"
                                        [ "strain Schmidt-Ruppin A"
                                        , "RSV-SRA"
                                        , "Avian leukosis virus-RSA"
                                        ]) v
  case parse parseOS os_test_5 of
    Left err => putStrLn err
    Right v  => assertEq "Test 5" (MkOS "Neurospora crassa"
              [ "strain ATCC 24698 / 74-OR23-1A / CBS 708.71 / DSM 1257 / FGSC 987"]) v
  case parse parseOS os_test_6 of
    Left err => putStrLn err
    Right v  => assertEq "Test 6" (MkOS "Halorhodospira halophila"
                                        [ "strain DSM 244 / SL1"
                                        , "Ectothiorhodospira halophila (strain DSM 244 / SL1)"
                                        ]) v

--}

---------------------------------------------------------------------------------------[ OG Field ]
--{1

og_test_1 : String
og_test_1 = "OG Hydrogenosome.\n"

og_test_2 : String
og_test_2 = "OG   Mitochondrion.\n"

og_test_3 : String
og_test_3 = "OG   Nucleomorph.\n"

og_test_4 : String
og_test_4 = "OG   Plasmid R6-5.\n"

og_test_5 : String
og_test_5 = "OG   Plastid.\n"

og_test_6 : String
og_test_6 = "OG   Plastid; Apicoplast.\n"

og_test_7 : String
og_test_7 = "OG   Plastid; Chloroplast.\n"

og_test_8 : String
og_test_8 = "OG   Plastid; Organellar chromatophore.\n"

og_test_9 : String
og_test_9 = "OG   Plastid; Cyanelle.\n"

og_test_10 : String
og_test_10 = "OG   Plastid; Non-photosynthetic plastid.\n"

og_test_11 : String
og_test_11 = """OG   Plasmid R6-5, Plasmid IncFII R100 (NR1), and
OG   Plasmid IncFII R1-19 (R1 drd-19).
"""

runTest_og : IO ()
runTest_og = do
  putStrLn "OG Tests:"
  case parse parseOG og_test_1 of
    Left err => putStrLn err
    Right v  => assertEq "Test 1" [Hydrogenosome] v
  case parse parseOG og_test_2 of
    Left err => putStrLn err
    Right v  => assertEq "Test 2" [Mitochondrion] v
  case parse parseOG og_test_3 of
    Left err => putStrLn err
    Right v  => assertEq "Test 3" [Nucleomorph] v
  case parse parseOG og_test_4 of
    Left err => putStrLn err
    Right v  => assertEq "Test 4" [Plasmid ["R6-5"]] v
  case parse parseOG og_test_5 of
    Left err => putStrLn err
    Right v  => assertEq "Test 5" [Plastid PlastidSimple] v
  case parse parseOG og_test_6 of
    Left err => putStrLn err
    Right v  => assertEq "Test 6" [Plastid PlastidApicoplast] v
  case parse parseOG og_test_7 of
    Left err => putStrLn err
    Right v  => assertEq "Test 7" [Plastid PlastidChloroplast] v
  case parse parseOG og_test_8 of
    Left err => putStrLn err
    Right v  => assertEq "Test 8" [Plastid PlastidOrganellarChromatophore] v
  case parse parseOG og_test_9 of
    Left err => putStrLn err
    Right v  => assertEq "Test 9" [Plastid PlastidCyanelle] v
  case parse parseOG og_test_10 of
    Left err => putStrLn err
    Right v  => assertEq "Test 10" [Plastid PlastidNonPhotosynthetic] v
  case parse parseOG og_test_11 of
    Left err => putStrLn err
    Right v  => assertEq "Test 11" [ Plasmid [ "R6-5"
                                             , "IncFII R100 (NR1)"
                                             , "IncFII R1-19 (R1 drd-19)"
                                             ]
                                   ] v

--}

---------------------------------------------------------------------------------------[ OC Field ]
--{1

oc_test_1 : String
oc_test_1 = """OC   Eukaryota; Metazoa; Chordata; Craniata; Vertebrata; Euteleostomi;
OC   Mammalia; Eutheria; Euarchontoglires; Primates; Catarrhini; Hominidae;
OC   Homo.
"""

runTest_oc : IO ()
runTest_oc = do
  putStrLn "OG Tests:"
  case parse parseOC oc_test_1 of
    Left err => putStrLn err
    Right v  => assertEq "Test 1" (MkOC [ "Eukaryota", "Metazoa", "Chordata", "Craniata"
                                        , "Vertebrata", "Euteleostomi", "Mammalia"
                                        , "Eutheria", "Euarchontoglires", "Primates"
                                        , "Catarrhini", "Hominidae", "Homo"
                                        ]
                                  ) v

--}

---------------------------------------------------------------------------------------[ OX Field ]
--{1

ox_test_1 : String
ox_test_1 = "OX   NCBI_TaxID=9606;\n"

ox_test_2 : String
ox_test_2 = "OX   NCBI_TaxID=562;\n"

runTest_ox : IO ()
runTest_ox = do
  case parse parseOX ox_test_1 of
    Left err => putStrLn err
    Right v  => assertEq "Test 1" (MkOX "NCBI_TaxID" "9606") v
  case parse parseOX ox_test_2 of
    Left err => putStrLn err
    Right v  => assertEq "Test 1" (MkOX "NCBI_TaxID" "562") v

--}

---------------------------------------------------------------------------------------[ OH Field ]
--{1

oh_test_1 : String
oh_test_1 = "OH   NCBI_TaxID=9481; Callithrix.\n"

oh_test_2 : String
oh_test_2 = "OH   NCBI_TaxID=9536; Cercopithecus hamlyni (Owl-faced monkey) (Hamlyn's monkey).\n"

oh_test_3 : String
oh_test_3 = "OH   NCBI_TaxID=9539; Macaca (macaques).\n"

oh_test_4 : String
oh_test_4 = "OH   NCBI_TaxID=9598; Pan troglodytes (Chimpanzee).\n"

runTest_oh : IO ()
runTest_oh = do
  case parse parseOH oh_test_1 of
    Left err => putStrLn err
    Right v  => assertEq "Test 1" [MkOH ("NCBI_TaxID","9481") "Callithrix" []] v
  case parse parseOH oh_test_2 of
    Left err => putStrLn err
    Right v  => assertEq "Test 2" [MkOH ("NCBI_TaxID","9536")
                                        "Cercopithecus hamlyni"
                                        [ "Owl-faced monkey"
                                        , "Hamlyn's monkey"
                                        ]
                                  ] v
  case parse parseOH oh_test_3 of
    Left err => putStrLn err
    Right v  => assertEq "Test 3" [MkOH ("NCBI_TaxID","9539")
                                        "Macaca"
                                        ["macaques"]
                                  ] v
  case parse parseOH oh_test_4 of
    Left err => putStrLn err
    Right v  => assertEq "Test 4" [MkOH ("NCBI_TaxID","9598")
                                        "Pan troglodytes"
                                        ["Chimpanzee"]
                                  ] v

--}

---------------------------------------------------------------------------------[ Refrence Field ]
--{1

ref_test_1 : String
ref_test_1 = """RN   [1]
RP   NUCLEOTIDE SEQUENCE [MRNA] (ISOFORMS A AND C), FUNCTION, INTERACTION
RP   WITH PKC-3, SUBCELLULAR LOCATION, TISSUE SPECIFICITY, DEVELOPMENTAL
RP   STAGE, AND MUTAGENESIS OF PHE-175 AND PHE-221.
RC   STRAIN=Bristol N2; PLASMID=R1 (R7268);
RC   TRANSPOSON=Tn3; STRAIN=AL.012, AZ.026, AZ.180, DC.005, GA.039, GA2181, IL.014, IL2.17,
RC   IN.018, KY.172, KY2.37, LA.013, MI.035, MN.001, MNb027, MS.040,
RC   NY.016, OH.036, TN.173, TN2.38, UT.002, and VA.015;
RX   PubMed=11134024; DOI=10.1074/jbc.M008990200;
RX   PubMed=11134024; DOI=10.1074/jbc.M008990200;
RG   The mouse genome sequencing consortium;
RG   The mouse genome sequencing consortium; The mouse genome
RG   sequencing consortium;
RA   Zhang L., Wu S.-L., Rubin C.S.,
RA   Zhang L., Wu S.-L., Rubin C.S. ;
RT   "A novel adapter protein employs a phosphotyrosine binding domain and
RT   exceptionally basic N-terminal domains to capture and localize an
RT   atypical protein kinase C: characterization of Caenorhabditis elegans
RT   C kinase adapter 1, a protein that avidly binds protein kinase C3.";
RL   J. Biol. Chem. 276:10463-10475(2001).
RL   J. Biol. Chem. 276:10463-10475(2001).
RL   (In) Magnusson S., Ottesen M., Foltmann B., Dano K., Neurath H.
RL   (eds.);
RL   Regulatory proteolytic enzymes and their inhibitors, pp.163-172,
RL   Pergamon Press, New York (1978).
"""

runTest_ref : IO ()
runTest_ref = do
  case parse parseRef ref_test_1 of
    Left err => putStrLn err
    Right v  => putStrLn $ show v
--}

---------------------------------------------------------------------------------------[ CC Field ]
--{1

cc_test_1 : String
cc_test_1 = """CC   -!- ALLERGEN: Causes an allergic reaction in human. Binds to IgE.
CC       Partially heat-labile allergen that may cause both respiratory and
CC       food-allergy symptoms in patients with the bird-egg syndrome.
CC   -!- ALLERGEN: Causes an allergic reaction in human. Minor allergen of
CC       bovine dander.
CC   -!- ALTERNATIVE PRODUCTS:
CC       Event=Alternative splicing; Named isoforms=3;
CC         Comment=Additional isoforms seem to exist. Experimental
CC         confirmation may be lacking for some isoforms;
CC       Name=1; Synonyms=AIRE-1;
CC         IsoId=O43918-1; Sequence=Displayed;
CC       Name=2; Synonyms=AIRE-2;
CC         IsoId=O43918-2; Sequence=VSP_004089;
CC       Name=3; Synonyms=AIRE-3;
CC         IsoId=O43918-3; Sequence=VSP_004089, VSP_004090;
CC   -!- ALTERNATIVE PRODUCTS:
CC       Event=Alternative initiation; Named isoforms=2;
CC       Name=Alpha;
CC         IsoId=P51636-1; Sequence=Displayed;
CC       Name=Beta;
CC         IsoId=P51636-2; Sequence=VSP_018696;
CC   -!- BIOPHYSICOCHEMICAL PROPERTIES:
CC       pH dependence:
CC         Optimum pH is 8-10;
CC       Temperature dependence:
CC         Highly active at low temperatures, even at 0 degree Celsius.
CC         Thermolabile;
CC   -!- BIOPHYSICOCHEMICAL PROPERTIES:
CC       Kinetic parameters:
CC         KM=98 uM for ATP;
CC         KM=688 uM for pyridoxal;
CC         Vmax=1.604 mmol/min/mg enzyme;
CC       pH dependence:
CC         Optimum pH is 6.0. Active from pH 4.5 to 10.5;
"""

runTest_cc : IO ()
runTest_cc = do
  case parse parseCC cc_test_1 of
    Left err => putStrLn err
    Right v  => putStrLn $ show v

--}

---------------------------------------------------------------------------------------[ DR Field ]
--{1

dr_test_1 : String
dr_test_1 = """DR   ArachnoServer; AS000173; kappa-hexatoxin-Hv1b.
DR   Araport; AT4G08920; -.
DR   Bgee; ENSRNOG00000001873; Expressed in 9 organ(s), highest expression level in skeletal muscle tissue.
DR   EMBL; X60307; CAA42852.1; -; Genomic_DNA.
DR   EMBL; X65124; CAA46250.1; -; Genomic_DNA.
DR   EMBL; Z28227; CAA82072.1; -; Genomic_DNA.
DR   BindingDB; P06709; -.
DR   BioCyc; EcoCyc:USHA-MONOMER; -.
DR   CCDS; CCDS18166.1; -. [O89019-1]
DR   BioGrid; 69392; 1.
DR   BioMuta; TF; -.
DR   BRENDA; 3.5.99.5; 3804.
DR   CarbonylDB; Q14789; -.
DR   CAZy; GH109; Glycoside Hydrolase Family 109.
"""

runTest_dr : IO ()
runTest_dr = do
  case parse parseDR dr_test_1 of
    Left err => putStrLn err
    Right v  => putStrLn $ show v

--}

---------------------------------------------------------------------------------------[ PE Field ]
--{1

pe_test_1 : String
pe_test_1 = "PE    1: Evidence at protein level;\n"

pe_test_2 : String
pe_test_2 = "PE    2: Evidence at transcript level;\n"

pe_test_3 : String
pe_test_3 = "PE    3: Inferred from homology;\n"

pe_test_4 : String
pe_test_4 = "PE    4: Predicted;\n"

pe_test_5 : String
pe_test_5 = "PE    5: Uncertain;\n"

runTest_pe : IO ()
runTest_pe = do
  case parse parsePE pe_test_1 of
    Left err => putStrLn err
    Right v  => putStrLn $ show v
  case parse parsePE pe_test_2 of
    Left err => putStrLn err
    Right v  => putStrLn $ show v
  case parse parsePE pe_test_3 of
    Left err => putStrLn err
    Right v  => putStrLn $ show v
  case parse parsePE pe_test_4 of
    Left err => putStrLn err
    Right v  => putStrLn $ show v
  case parse parsePE pe_test_5 of
    Left err => putStrLn err
    Right v  => putStrLn $ show v

--}

---------------------------------------------------------------------------------------[ KW Field ]
--{1

kw_test_1 : String
kw_test_1 ="""KW   3D-structure; Alternative splicing; Alzheimer disease; Amyloid;
KW   Apoptosis; Cell adhesion; Coated pits; Copper;
KW   Direct protein sequencing; Disease mutation; Endocytosis;
KW   Glycoprotein; Heparin-binding; Iron; Membrane; Metal-binding;
KW   Notch signaling pathway; Phosphorylation; Polymorphism;
KW   Protease inhibitor; Proteoglycan; Serine protease inhibitor; Signal;
KW   Transmembrane; Zinc.
"""

runTest_kw : IO ()
runTest_kw = do
  case parse parseKW kw_test_1 of
    Left err => putStrLn err
    Right v  => putStrLn $ show v

--}

---------------------------------------------------------------------------------------[ FT Field ]
--{1

ft_test_1 : String
ft_test_1 ="""FT   CARBOHYD    251    251       N-linked (GlcNAc...) asparagine.
FT                                /FTId=CAR_000070.
FT   CHAIN        23    611       Halfway protein.
FT                                /FTId=PRO_0000021413.
FT   PEPTIDE      20     57       Histatin 1.
FT                                /FTId=PRO_0000021416.
FT   PROPEP       25     48
FT                                /FTId=PRO_0000021449.
FT   VARIANT     214    214       V -> I.
FT                                /FTId=VAR_009122.
FT   VAR_SEQ      33     83       TPDINPAWYTGRGIRPVGRFGRRRATPRDVTGLGQLSCLPL
FT                                DGRTKFSQRG -> SECLTYGKQPLTSFHPFTSQMPP (in
FT                                isoform 2).
FT                                /FTId=VSP_004370.
"""

runTest_ft : IO ()
runTest_ft = do
  case parse parseFT ft_test_1 of
    Left err => putStrLn err
    Right v  => putStrLn $ show v

--}

---------------------------------------------------------------------------------------[ SQ Field ]
--{1

sq_test_1 : String
sq_test_1 ="""SQ   SEQUENCE   97 AA;  9110 MW;  E3C20C259858B830 CRC64;
     MTILASICKL GNTKSTSSSI GSSYSSAVSF GSNSVSCGEC GGDGPSFPNA SPRTGVKAGV
     NVDGLLGAIG KTVNGMLISP NGGGGGMGMG GGSCGCI
"""

runTest_sq : IO ()
runTest_sq = do
  case parse parseSQ sq_test_1 of
    Left err => putStrLn err
    Right v  => putStrLn $ show v

--}

-------------------------------------------------------------------------------------[  Run Tests ]
--{1

namespace Main

  main : IO ()
  main = do
    [_,fn] <- getArgs | putStrLn "No field selected"
    case fn of
      "id"  => runTest_id
      "ac"  => runTest_ac
      "dt"  => runTest_dt
      "de"  => runTest_de
      "gn"  => runTest_gn
      "os"  => runTest_os
      "og"  => runTest_og
      "oc"  => runTest_oc
      "ox"  => runTest_ox
      "oh"  => runTest_oh
      "ref" => runTest_ref
      "cc"  => runTest_cc
      "dr"  => runTest_dr
      "pe"  => runTest_pe
      "kw"  => runTest_kw
      "ft"  => runTest_ft
      "sq"  => runTest_sq
      _     => putStrLn "Data field does not exist!"

--}



