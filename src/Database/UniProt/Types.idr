-----------------------------------------------------------------------------------------[ Module ]
--{1
--                                                                              (\_/)
--                                                                              (o.O)
--                                                                              (> <)
--                                                                             #######
--                                                                           KILLER BUNNY
--                                                                             APPROVED
--
-- The data type that will be used as internal represntation of an UniProt protein entry. The data
-- description can be seen at : web.expasy.org/docs/userman.html.
--

module Database.UniProt.Types

-- %default total
%access public export

%flag C "-O3"
%flag C "-g"

--}

------------------------------------------------------------------------------------------[ Types ]
--{1

--{2 Status

data Status = Reviewed
            | Unreviewed

Eq Status where
  (==) a b with (a,b)
    | (Reviewed,Reviewed)     = True
    | (Unreviewed,Unreviewed) = True
    | _                       = False

Show Status where
  show x with (x)
    | Reviewed   = "reviewed"
    | Unreviewed = "unreviewed"

--}

--{2 Name

record Name where
  constructor MkName
  fullName  : String
  shortName : List String
  ecNum     : List String

Eq Name where
  (==) (MkName f1 s1 e1) (MkName f2 s2 e2) = (f1 == f2) && (s1 == s2) && (e1 == e2)

Show Name where
  show (MkName f s e) = (concat $ intersperse " "
       [ "full: " ++ f
       , "short: " ++ show s
       , "enzyme: " ++ show e
       ])

data AltName = Simple Name
             | Allergen String
             | Biotech String
             | CDAntigen String
             | INN String

Eq AltName where
  (==) a b with (a,b)
    | (Simple    x, Simple    y) = x == y
    | (Allergen  x, Allergen  y) = x == y
    | (Biotech   x, Biotech   y) = x == y
    | (CDAntigen x, CDAntigen y) = x == y
    | (INN       x, INN       y) = x == y
    | (_          , _          ) = False

Show AltName where
  show x with (x)
    | Simple    n = show n
    | Allergen  s = "Allergen-" ++ s
    | Biotech   s = "Biotech-" ++ s
    | CDAntigen s = "CDA-" ++ s
    | INN       s = "INN-" ++ s

data Flag = Fragment
          | Fragments
          | Precursor

Eq Flag where
  (==) a b with (a,b)
    | (Fragment,Fragment)    = True
    | (Fragments,Fragments)  = True
    | (Precursor, Precursor) = True
    | _                      = False

Show Flag where
  show x with (x)
    | Fragment  = "frag"
    | Fragments = "frags"
    | Precursor = "pre"

--}

--{2 Plastid

data PlastidType = PlastidSimple
                 | PlastidApicoplast
                 | PlastidChloroplast
                 | PlastidOrganellarChromatophore
                 | PlastidCyanelle
                 | PlastidNonPhotosynthetic

Eq PlastidType where
  (==) a b with (a,b)
    | (PlastidSimple                  , PlastidSimple                 ) = True
    | (PlastidApicoplast              , PlastidApicoplast             ) = True
    | (PlastidChloroplast             , PlastidChloroplast            ) = True
    | (PlastidOrganellarChromatophore , PlastidOrganellarChromatophore) = True
    | (PlastidCyanelle                , PlastidCyanelle               ) = True
    | (PlastidNonPhotosynthetic       , PlastidNonPhotosynthetic      ) = True
    | _                                                                 = False

Show PlastidType where
  show x with (x)
    | PlastidSimple                   = "Simple"
    | PlastidApicoplast               = "Apicoplast"
    | PlastidChloroplast              = "Chloroplast"
    | PlastidOrganellarChromatophore  = "Chromatophore"
    | PlastidCyanelle                 = "Cyanelle"
    | PlastidNonPhotosynthetic        = "Non-Photosynthetic"

--}

data RCom = STRAIN
          | PLASMID
          | TRANSPOSON
          | TISSUE

Show RCom where
  show x with (x)
    | STRAIN     = "strain"
    | PLASMID    = "plasmid"
    | TRANSPOSON = "transposon"
    | TISSUE     = "tissue"

data BibDB = MEDLINE
           | PubMed
           | DOI
           | AGRICOLA

Show BibDB where
  show x with (x)
    | MEDLINE  = "medline"
    | PubMed   = "pubmed"
    | DOI      = "doi"
    | AGRICOLA = "agricola"

--}

----------------------------------------------------------------------------------[ Record Fields ]
--{1

--{2 UID

record UID  where
  constructor MkUID
  name   : String
  status : Status
  seqLen : Int

Eq UID where
  (==) (MkUID n1 s1 q1) (MkUID n2 s2 q2) = (n1 == n2) && (s1 == s2) && (q1 == q2)

Show UID where
  show (MkUID n s q) = "Protein:\n" <+> (concat $ intersperse "\n"
       [ "uid: " ++ n
       , "status: " ++ show s
       , "length: " ++ show q
       ])

--}

--{2 AC

record AC where
  constructor MkAC
  accessions : List String

Eq AC where
  (==) (MkAC a1) (MkAC a2) = a1 == a2

Show AC where
  show (MkAC as) = "accs: " <+> show as

--}

--{2 DT

record DT where
  constructor MkDT
  init     : String
  dbLoc    : String
  seqMod   : String
  seqVer   : Int
  entryMod : String
  entryVer : Int

Eq DT where
  (==) (MkDT x1 x2 x3 x4 x5 x6) (MkDT y1 y2 y3 y4 y5 y6) =
    (x1 == y1) && (x2 == y2) &&
    (x3 == y3) && (x4 == y4) &&
    (x5 == y5) && (x6 == y6)

Show DT where
  show (MkDT i d m mv e ev) = (concat $ intersperse "\n"
       [ "initial : " ++ i
       , "db: " ++ d
       , "seq: " ++ m ++ " version " ++ show mv
       , "entry: " ++ e ++ " version " ++ show ev
       ])

--}

--{2 DE

record DE where
  constructor MkDE
  name     : Maybe Name
  altNames : List AltName
  subName  : List Name
  includes : List (Maybe Name, List AltName, List Name)
  contains : List (Maybe Name, List AltName, List Name)
  flags    : List Flag

Eq DE where
  (==) (MkDE x1 x2 x3 x4 x5 x6) (MkDE y1 y2 y3 y4 y5 y6) =
    (x1 == y1) && (x2 == y2) &&
    (x3 == y3) && (x4 == y4) &&
    (x5 == y5) && (x6 == y6)

Show DE where
  show (MkDE n a s i c f) =(concat $ intersperse "\n"
       [ "recomended: "      ++ show n
       , "alternative: " ++ show a
       , "subname: "     ++ show s
       , "includes: "    ++ show i
       , "contains: "    ++ show c
       , "flags: "       ++ show f
       ])

--}

--{2 GN

record GN where
  constructor MkGN
  name        : Maybe String
  synonyms    : List String
  orderedLoci : List String
  orfNames    : List String

Eq GN where
  (==) (MkGN x1 x2 x3 x4) (MkGN y1 y2 y3 y4) =
    (x1 == y1) && (x2 == y2) &&
    (x3 == y3) && (x4 == y4)

Show GN where
  show (MkGN n s l o) = (concat $ intersperse "\n"
       [ "gene: "  ++ show n
       , "syn: "  ++ show s
       , "loci: " ++ show l
       , "orf: "  ++ show o
       ])

--}

--{2 OS

record OS where
  constructor MkOS
  species : String
  common  : List String

Eq OS where
  (==) (MkOS x1 x2) (MkOS y1 y2) = (x1 == y1) && (x2 == y2)

Show OS where
  show (MkOS n c) = (concat $ intersperse "\n"
       [ "name: " ++ show n
       , "common: "  ++ show c
       ])

--}

--{2 OG

data OG = Hydrogenosome
        | Mitochondrion
        | Nucleomorph
        | Plastid PlastidType
        | Plasmid (List String)

Eq OG where
  (==) a b with (a,b)
    | (Hydrogenosome , Hydrogenosome) = True
    | (Mitochondrion , Mitochondrion) = True
    | (Nucleomorph   , Nucleomorph  ) = True
    | (Plastid x     , Plastid y    ) = x == y
    | (Plasmid x     , Plasmid y    ) = x == y
    | _                               = False

Show OG where
  show x with (x)
    | Hydrogenosome = "Hydrogenosome"
    | Mitochondrion = "Mitochondrion"
    | Nucleomorph   = "Nucleomorph"
    | Plastid t     = "Plastid " ++ show t
    | Plasmid xs    = "Plasmids: " ++ show xs

--}

--{2  OC

record OC where
  constructor MkOC
  taxa : List String

Eq OC where
  (==) (MkOC x) (MkOC y) = x == y

Show OC where
  show (MkOC ts) = show ts

--}

--{2 OX

record OX where
  constructor MkOX
  qualifier : String
  taxonCode : String

Eq OX where
  (==) (MkOX x1 x2) (MkOX y1 y2) = (x1 == y1) && (x2 == y2)

Show OX where
  show (MkOX q t) = "taxa-xref: " ++ q ++ " " ++ t

--}

--{2 OH

record OH where
  constructor MkOH
  taxaId     : (String, String)
  hostRec    : String
  hostCommon : List String

Eq OH where
  (==) (MkOH x1 x2 x3) (MkOH y1 y2 y3) = (x1 == y1) && (x2 == y2) && (x3 == y3)

Show OH where
  show (MkOH t n cs) = (concat . intersperse " ") [ "Host:", show t, n, show cs ]

--}

record Ref where
  constructor MkRef
  rn : Int
  rp : String
  rc : List (RCom, String)
  rx : List (BibDB, String)
  rg : List String
  ra : List String
  rt : Maybe String
  rl : String

Show Ref where
  show (MkRef n p c x g a t l) = (concat . intersperse "\n")
    [ "RN: " ++ show n
    , "RP: " ++ p
    , "RC: " ++ show c
    , "RX: " ++ show x
    , "RG: " ++ show g
    , "RA: " ++ show a
    , "RT: " ++ show t
    , "RL: " ++ show l
    ]

record CC where
  constructor MkCC
  topic   : String
  comment : String

Show CC where
  show (MkCC t c) = t ++ "=" ++ c

record DR where
  constructor MkDR
  abbr : String
  uid  : String
  info : List String

Show DR where
  show (MkDR a u i) = (concat $ intersperse " "
       [ "(db : "    ++ a
       , "id: "   ++ u
       , "info: " ++ show i ++ ")"
       ])

data PE = EvidenceAtProteinLevel
        | EvidenceAtTranscriptLevel
        | InferredFromHomology
        | Predicted
        | Uncertain

Show PE where
  show x with (x)
    | EvidenceAtProteinLevel    = "EvidenceAtProteinLevel"
    | EvidenceAtTranscriptLevel = "EvidenceAtTranscriptLevel"
    | InferredFromHomology      = "InferredFromHomology"
    | Predicted                 = "Predicted"
    | Uncertain                 = "Uncertain"

record KW where
  constructor MkKW
  keywords : List String

Show KW where
  show (MkKW x) = "keywords: " ++ show x

record FT where
  constructor MkFT
  name : String
  from : String
  to   : String
  desc : List String

Show FT where
  show (MkFT n f t d) = (concat $ intersperse " "
       [ "(name : " ++ n
       , "from : "  ++ f
       , "to : "    ++ t
       , "desc: "   ++ show d ++ ")"
       ])

record SQ where
  constructor MkSQ
  length : Int
  mass   : Int
  crc64  : String
  seq    : String

Show SQ where
  show (MkSQ l m c s) = (concat $ intersperse " "
       [ "(len : "  ++ show l
       , "mass : "  ++ show m
       , "crc64 : " ++ c
       , "seq: "    ++ s ++ ")"
       ])

--}

-----------------------------------------------------------------------------------[ Entry Record ]
--{1

record Entry where
  constructor MkEntry
  uid  : UID              -- Identification           : Once
  ac   : AC               -- Accession number(s)      : Once or more
  dt   : DT               -- Date                     : Three times
  de   : DE               -- Description              : Once or more
  gn   : Maybe (List GN)  -- Gene name(s)             : Optional
  os   : OS               -- Organism species         : Once or more
  og   : Maybe (List OG)  -- Organelle                : Optional
  oc   : OC               -- Organism classification  : Once or more
  ox   : OX               -- Taxonomy cross-refs      : Once
  oh   : Maybe (List OH)  -- Organism host            : Optional
  refs : List Ref         -- References               : Once or more
  cc   : Maybe (List CC)  -- Comments or notes        : Optional
  dr   : Maybe (List DR)  -- Database cross-refs      : Optional
  pe   : PE               -- Protein existence        : Once
  kw   : Maybe KW         -- Keywords                 : Optional
  ft   : Maybe (List FT)  -- Feature table data       : Once or more in Swiss-Prot, opt in TrEMBL
  sq   : SQ               -- Sequence header          : Once


Show Entry where
  show (MkEntry uid ac dt de gn os og oc ox oh refs cc dr pe kw ft sq) =
    "Protein:\n" <+> ((concat . intersperse "\n\n")
       [ "ID: " ++ show uid
       , "AC: " ++ show ac
       , "DT: " ++ show dt
       , "DE: " ++ show de
       , "GN: " ++ show gn
       , "OS: " ++ show os
       , "OG: " ++ show og
       , "OC: " ++ show oc
       , "OX: " ++ show ox
       , "OH: " ++ show oh
       , "RF: " ++ show refs
       , "CC: " ++ show cc
       , "DR: " ++ show dr
       , "PE: " ++ show pe
       , "KW: " ++ show kw
       , "FT: " ++ show ft
       , "SQ: " ++ show sq
       ]) <+> "\n\n\n"


--}



