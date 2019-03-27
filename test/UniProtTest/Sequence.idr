-----------------------------------------------------------------------------------------[ Module ]
--{1
--                                                                              (\_/)
--                                                                              (o.O)
--                                                                              (> <)
--                                                                             #######
--                                                                           KILLER BUNNY
--                                                                             APPROVED

module UniProtTest.Sequence

import Database.UniProt.Types

%access export
%default total

%flag C "-O3"
%flag C "-g"

--}

---------------------------------------------------------------------------------------[ CC Field ]
--{1


cc_tests : List (String, List CC)
cc_tests = [
(-- Test 1
"""CC   -!- ALLERGEN: Causes an allergic reaction in human. Binds to IgE.
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
""",
[ MkCC "ALLERGEN"
       ("Causes an allergic reaction in human. Binds to IgE. " ++
        "Partially heat-labile allergen that may cause both respiratory and " ++
        "food-allergy symptoms in patients with the bird-egg syndrome."
        )
, MkCC "ALLERGEN"
       ("Causes an allergic reaction in human. Minor allergen of " ++
        "bovine dander."
        )
, MkCC "ALTERNATIVE PRODUCTS"
       ("Event=Alternative splicing; Named isoforms=3; " ++
        "Comment=Additional isoforms seem to exist. Experimental " ++
        "confirmation may be lacking for some isoforms; " ++
        "Name=1; Synonyms=AIRE-1; " ++
        "IsoId=O43918-1; Sequence=Displayed; " ++
        "Name=2; Synonyms=AIRE-2; " ++
        "IsoId=O43918-2; Sequence=VSP_004089; " ++
        "Name=3; Synonyms=AIRE-3; " ++
        "IsoId=O43918-3; Sequence=VSP_004089, VSP_004090;"
        )
, MkCC "ALTERNATIVE PRODUCTS"
       ("Event=Alternative initiation; Named isoforms=2; " ++
        "Name=Alpha; " ++
        "IsoId=P51636-1; Sequence=Displayed; " ++
        "Name=Beta; " ++
        "IsoId=P51636-2; Sequence=VSP_018696;"
        )
, MkCC "BIOPHYSICOCHEMICAL PROPERTIES"
       ("pH dependence: " ++
        "Optimum pH is 8-10; " ++
        "Temperature dependence: " ++
        "Highly active at low temperatures, even at 0 degree Celsius. " ++
        "Thermolabile;"
       )
, MkCC "BIOPHYSICOCHEMICAL PROPERTIES"
       ("Kinetic parameters: " ++
        "KM=98 uM for ATP; " ++
        "KM=688 uM for pyridoxal; " ++
        "Vmax=1.604 mmol/min/mg enzyme; " ++
        "pH dependence: " ++
        "Optimum pH is 6.0. Active from pH 4.5 to 10.5;"
       )
]
)
]-- End

--}

---------------------------------------------------------------------------------------[ DR Field ]
--{1

dr_tests : List (String, List DR)
dr_tests = [
(-- Test 1
"""DR   ArachnoServer; AS000173; kappa-hexatoxin-Hv1b.
DR   Araport; AT4G08920; -.
DR   Bgee; ENSRNOG00000001873; Expressed in 9 organ(s), highest expression level in skeletal muscle tissue.
""",
[ MkDR "ArachnoServer"
       "AS000173"
       ["kappa-hexatoxin-Hv1b"]
, MkDR "Araport"
       "AT4G08920"
       []
, MkDR "Bgee"
       "ENSRNOG00000001873"
       ["Expressed in 9 organ(s), highest expression level in skeletal muscle tissue"]
]
),
(-- Test 2
"""DR   EMBL; X60307; CAA42852.1; -; Genomic_DNA.
DR   BindingDB; P06709; -.
DR   BioCyc; EcoCyc:USHA-MONOMER; -.
""",
[ MkDR "EMBL"
       "X60307"
       ["CAA42852.1", "Genomic_DNA"]
, MkDR "BindingDB"
       "P06709"
       []
, MkDR "BioCyc"
       "EcoCyc:USHA-MONOMER"
       []
]
),
(-- Test 3
"""DR   CCDS; CCDS18166.1; -. [O89019-1]
DR   BioGrid; 69392; 1.
DR   BioMuta; TF; -.
""",
[ MkDR "CCDS"
       "CCDS18166.1"
       ["-. [O89019-1]"]
, MkDR "BioGrid"
       "69392"
       ["1"]
, MkDR "BioMuta"
       "TF"
       []
]
),
(-- Test 4
"""DR   BRENDA; 3.5.99.5; 3804.
DR   CarbonylDB; Q14789; -.
DR   CAZy; GH109; Glycoside Hydrolase Family 109.
""",
[ MkDR "BRENDA"
       "3.5.99.5"
       ["3804"]
, MkDR "CarbonylDB"
       "Q14789"
       []
, MkDR "CAZy"
       "GH109"
       ["Glycoside Hydrolase Family 109"]
]
)
]-- End

--}

---------------------------------------------------------------------------------------[ PE Field ]
--{1

pe_tests : List (String, PE)
pe_tests = [
(-- Test 1
"""PE    1: Evidence at protein level;
""",
EvidenceAtProteinLevel
),
(-- Test 2
"""PE    2: Evidence at transcript level;
""",
EvidenceAtTranscriptLevel
),
(-- Test 3
"""PE    3: Inferred from homology;
""",
InferredFromHomology
),
(-- Test 4
"""PE    4: Predicted;
""",
Predicted
),
(-- Test 5
"""PE    5: Uncertain;
""",
Uncertain
)
]-- End


--}

---------------------------------------------------------------------------------------[ KW Field ]
--{1

kw_tests : List (String, KW)
kw_tests = [
(-- Test 1
"""KW   3D-structure; Alternative splicing; Alzheimer disease; Amyloid;
KW   Apoptosis; Cell adhesion; Coated pits; Copper.
""",
MkKW [ "3D-structure", "Alternative splicing", "Alzheimer disease", "Amyloid",
       "Apoptosis", "Cell adhesion", "Coated pits", "Copper"
     ]
),
(-- Test 2
"""KW   Direct protein sequencing; Disease mutation; Endocytosis;
KW   Glycoprotein; Heparin-binding; Iron; Membrane; Metal-binding;
KW   Notch signaling pathway; Phosphorylation; Polymorphism.
""",
MkKW [ "Direct protein sequencing", "Disease mutation", "Endocytosis",
       "Glycoprotein", "Heparin-binding", "Iron", "Membrane", "Metal-binding",
       "Notch signaling pathway", "Phosphorylation", "Polymorphism"
     ]
),
(-- Test 3
"""KW   Protease inhibitor; Proteoglycan; Serine protease inhibitor; Signal;
KW   Transmembrane; Zinc.
""",
MkKW [ "Protease inhibitor", "Proteoglycan", "Serine protease inhibitor", "Signal",
       "Transmembrane", "Zinc"
     ]
)
]-- End

--}

---------------------------------------------------------------------------------------[ FT Field ]
--{1


ft_tests : List (String, List FT)
ft_tests = [
(-- Test 1
"""FT   CARBOHYD    251    251       N-linked (GlcNAc...) asparagine.
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
""",
[ MkFT "CARBOHYD" "251" "251" [ "N-linked (GlcNAc...) asparagine."
                              , "/FTId=CAR_000070."
                              ]
, MkFT "CHAIN"    "23"  "611" [ "Halfway protein."
                              , "/FTId=PRO_0000021413."
                              ]
, MkFT "PEPTIDE"  "20"  "57"  ["Histatin 1."
                              , "/FTId=PRO_0000021416."
                              ]
, MkFT "PROPEP"   "25"  "48"  [ "/FTId=PRO_0000021449."
                              ]
, MkFT "VARIANT"  "214" "214" [ "V -> I."
                              , "/FTId=VAR_009122."
                              ]
, MkFT "VAR_SEQ"  "33"  "83"  [ "TPDINPAWYTGRGIRPVGRFGRRRATPRDVTGLGQLSCLPLDGRTKFSQRG " ++
                                "-> SECLTYGKQPLTSFHPFTSQMPP (in isoform 2)."
                              , "/FTId=VSP_004370."
                              ]
]
)
]-- End

--}

---------------------------------------------------------------------------------------[ SQ Field ]
--{1

sq_tests : List (String, SQ)
sq_tests = [
(-- Test 1
"""SQ   SEQUENCE   97 AA;  9110 MW;  E3C20C259858B830 CRC64;
     MTILASICKL GNTKSTSSSI GSSYSSAVSF GSNSVSCGEC GGDGPSFPNA SPRTGVKAGV
     NVDGLLGAIG KTVNGMLISP NGGGGGMGMG GGSCGCI
""",
(MkSQ 97 9110 "E3C20C259858B830"
  ("MTILASICKLGNTKSTSSSIGSSYSSAVSFGSNSVSCGECGGDGPSFPNASPRTGVKAGV" ++
   "NVDGLLGAIGKTVNGMLISPNGGGGGMGMGGGSCGCI")
)
)
]-- End

--}


