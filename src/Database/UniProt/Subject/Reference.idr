-----------------------------------------------------------------------------------------[ Module ]
--{1
--                                                                              (\_/)
--                                                                              (o.O)
--                                                                              (> <)
--                                                                             #######
--                                                                           KILLER BUNNY
--                                                                             APPROVED

module Database.UniProt.Subject.Reference

import Database.UniProt.Utils

import Lightyear
import Lightyear.Char
import Lightyear.Strings

import Data.String

%default partial
%access export

%flag C "-O3"
%flag C "-g"

--}

---------------------------------------------------------------------------------------[ RN Field ]
--{1

--{2 Documentation
--
-- The 'RN' (Reference Number) line gives a sequential identifier to a reference to use as a
-- citation in the Refence Comments and Feature Table notes. The format for the line is:
--
-- > RN   [n]
--
-- Where n is a positive integer.
--
--}

--{2 RN

RN : Type
RN = Int

--}

--{2 Parser

parseRN : Parser RN
parseRN = ((fromJust 0) . parseInteger . pack)
 <$> ((string "RN") *!> whitespace *!> char '['
     *!> (many $ satisfy isDigit)
     <*! char ']') <* optEvidence <* endOfLine
 <?> "RN reference number"

--}

--}

---------------------------------------------------------------------------------------[ RP Field ]
--{1

--{2 Documentation
--
-- The 'RP' (Research Position) field describes the extent of the work relevant to the entry
-- carried out by the authors. The format of the RP line is:
--
-- > RP   COMMENT.
--
-- Where COMMENT can span multiple lines, each prefixed with the 'RP' field identifier.
--
--}

--{2 RP

RP : Type
RP = String

--}

--{2 Parser

parseRP : Parser RP
parseRP = (\x,y => case y of [] => x; _ => concat (x :: " " :: (intersperse " " y)))
 <$> (string "RP" *> whitespace *> (pack <$> (many (noneOf "\n"))))
 <*> (many (endOfLine *> string "RP" *> whitespace *> (pack <$> (many (noneOf "\n")))))
 <*  endOfLine
 <?> "RP reference position"

--}

--}

---------------------------------------------------------------------------------------[ RC Field ]
--{1

--{2 Documentation
--
-- The 'RC' (Reference Comment) field is optional, and is used to store comments relavent to the
-- cited reference. The format of the RC line is:
--
-- > RC   TOKEN1=Text1; TOKEN2=Text2; ...
--
-- Referece comments may spaan across lines. The currently defined tokens and their order in the
-- RC line are:
--
--    * STRAIN
--
--    * PLASMID
--
--    * TRANSPOSON
--
--    * TISSUE
--
-- Examples of the RC line:
--
-- > RC   STRAIN=Sprague-Dawley; TISSUE=Liver;
--
-- > RC   STRAIN=Holstein; TISSUE=Lymph node, and Mammary gland;
--
-- > RC   STRAIN=301 / Serotype 2a;
--
-- > RC   STRAIN=cv. SP753012-O; TISSUE=Leaf;
--
-- > RC   PLASMID=R1 (R7268); TRANSPOSON=Tn3;
--
-- > RC   STRAIN=AL.012, AZ.026, AZ.180, DC.005, GA.039, GA2181, IL.014, IL2.17,
-- > RC   IN.018, KY.172, KY2.37, LA.013, MI.035, MN.001, MNb027, MS.040,
-- > RC   NY.016, OH.036, TN.173, TN2.38, UT.002, and VA.015;

--}

--{2 RCom

data RCom = STRAIN
          | PLASMID
          | TRANSPOSON
          | TISSUE

Eq RCom where
  (==) x y with (x,y)
    | (STRAIN     , STRAIN    ) = True
    | (PLASMID    , PLASMID   ) = True
    | (TRANSPOSON , TRANSPOSON) = True
    | (TISSUE     , TISSUE    ) = True
    | _                         = False

Show RCom where
  show x with (x)
    | STRAIN     = "strain"
    | PLASMID    = "plasmid"
    | TRANSPOSON = "transposon"
    | TISSUE     = "tissue"

--}

--{2 RC

RC : Type
RC = List (RCom, String)

--}

--{2 Parser

parseRC : Parser RC
parseRC = (\x,y => x ++ y)
 <$> (string "RC" *> whitespace *> (many rcVal))
 <*> (concat <$> (many (endOfLine *> string "RC" *> whitespace *> many rcVal)))
 <* endOfLine
 <?> "RC reference comment"
  where
    rcTok : Parser String
    rcTok = pack <$> many (noneOf ";\n")

    rcFst : Parser RCom
    rcFst = ((string "STRAIN=")     *> pure STRAIN)
       <|>| ((string "PLASMID=")    *> pure PLASMID)
       <|>| ((string "TRANSPOSON=") *> pure TRANSPOSON)
       <|>| ((string "TISSUE=")     *> pure TISSUE)

    rcSnd : Parser String
    rcSnd = (\x,y => case y of [] => x; _ => concat (x :: " " :: (intersperse " " y)))
        <$> rcTok
        <*> (many (endOfLine *> string "RC" *> whitespace *> rcTok))

    rcVal : Parser (RCom, String)
    rcVal = MkPair <$> rcFst <*> (rcSnd <* tok (char ';'))

--}

--}

---------------------------------------------------------------------------------------[ RX Field ]
--{1

--{2 Documentation
--
-- The 'RX' (Reference Cross-Reference) field is optional and is used to indicate the identifier
-- assigned to a specific reference in a bibliographic database. The format of the RX line is:
--
-- > RX   Bibliographic_db=IDENTIFIER[; Bibliographic_db=IDENTIFIER...];
--
-- Where the valid bibliographic database names and their associated identifiers are:
--
--     MEDLINE  :  Eight-digit MEDLINE Unique Identifier (UI)
--     PubMed   :  PubMed Unique Identifier (PMID)
--     DOI      :  Digital Object Identifier (DOI)
--     AGRICOLA :  AGRICOLA Unique Identifier
--
--}

--{2 BibDb

data BibDB = MEDLINE
           | PubMed
           | DOI
           | AGRICOLA

Eq BibDB where
  (==) x y with (x,y)
    | (MEDLINE  , MEDLINE ) = True
    | (PubMed   , PubMed  ) = True
    | (DOI      , DOI     ) = True
    | (AGRICOLA , AGRICOLA) = True
    | _                     = False

Show BibDB where
  show x with (x)
    | MEDLINE  = "medline"
    | PubMed   = "pubmed"
    | DOI      = "doi"
    | AGRICOLA = "agricola"

--}

--{2 RX

RX : Type
RX = List (BibDB, String)

--}

--{2 Parser

-- Problem element in out_1 file...
--    RX   DOI=10.1002/(SICI)1098-2795(199708)47:4<370::AID-MRD3>3.0.CO;2-H;

parseRX : Parser RX
parseRX = (\x,y => x ++ y)
 <$> (string "RX" *> whitespace *> many rxVal)
 <*> (concat <$> (many (endOfLine *> string "RX" *> whitespace *> many rxVal)))
 <*  endOfLine
 <?> "RX cross reference"
  where
    rxTok : Parser String
    rxTok = pack <$> many (noneOf ";\n")

    rxFst : Parser BibDB
    rxFst = ((string "MEDLINE=")  *!> pure MEDLINE)
       <|>| ((string "PubMed=")   *!> pure PubMed)
       <|>| ((string "DOI=")      *!> pure DOI)
       <|>| ((string "AGRICOLA=") *!> pure AGRICOLA)

    rxSnd : Parser String
    rxSnd = (\x,y => case y of [] => x; _ => concat (x :: " " :: (intersperse " " y)))
        <$> rxTok
        <*> (many (endOfLine *> string "RX" *> whitespace *> rxTok))

    rxVal : Parser (BibDB, String)
    rxVal = MkPair <$> rxFst <*> (rxSnd <* tok (char ';'))

--}

--}

---------------------------------------------------------------------------------------[ RG Field ]
--{1

--{2 Documentation
--
-- The 'RG' (Reference Group) field lists the consortium name associated with a given citation. The
-- format of this line is:
--
-- > RG    group;
--
-- While this field is optional, at least one of the RG or RA fields must be present (or both).
--
--}

--{2 RG

RG : Type
RG = List String

--}

--{2 Parser

parseRG : Parser RG
parseRG = (\x,y => map trim $ x ++ y)
      <$> (string "RG" *> whitespace *> many rgVal)
      <*> (concat <$> (many (endOfLine *> string "RG" *> whitespace *> many rgVal)))
      <*  endOfLine
      <?> "RG research group"
  where
    rgTok : Parser String
    rgTok = pack <$> many (noneOf ";\n")

    rgVal : Parser String
    rgVal = (\x,y => case y of [] => x; _ => concat (x :: " " :: (intersperse " " y)))
        <$> rgTok
        <*> ((many (endOfLine *> string "RG" *> whitespace *> rgTok)) <* tok (char ';'))

--}

--}

---------------------------------------------------------------------------------------[ RA Field ]
--{1

--{2 Documentation
--
-- The 'RA' (Reference Authors) field lists the authors of the paper (or other work) cited. The
-- authors' names are separated by commas and terminated by a semicolon. Author names are not split
-- between lines, but the field may span accross multiple lines. The field is formatted as:
--
-- > RA    author[, author]...[, author];
--
-- While this field is optional, at least one of the RG or RA fields must be present (or both).
--
--}

--{2 RA

RA : Type
RA = List String

--}

--{2 Parser

parseRA : Parser RA
parseRA = (map trim) <$> (mllst "RA" ',' ';') <* endOfLine

--}

--}

---------------------------------------------------------------------------------------[ RT Field ]
--{1

--{2 Documentation
--
-- The 'RT' (Reference Title) field gives the title of the paper (or other work) cited as exactly
-- as possible given the limitations of the computer character set. The format of the RT line is:
--
-- > RT   "Title.";
--
-- It should be noted that the format of the title is not always identical to that displayed at the
-- top of the published work:
--
--     * Major title words are not capitalized;
--
--     * The text of a title ends with either a period '.', a question mark '?' or an exclamation
--       mark '!';
--
--     * Double quotation marks ' " ' in the text of the title are replaced by single quotation
--       marks;
--
--     * Titles of articles published in a language other than English have been translated into
--       English;
--
--     * Greek letters are written in full (alpha, beta, etc.).
--
--}

--{2 RT

RT : Type
RT = String

--}

--{2 Parser

parseRT : Parser RT
parseRT = (string "RT") *> (trimBy (=='"') <$> mlstr "RT" ';') <* char ';' <* endOfLine

--}

--}

---------------------------------------------------------------------------------------[ RL Field ]
--{1

--{2 Documentation
--
-- The 'RL' (Reference Location) field contains the conventional citation information for the
-- reference. In general, the RL lines alone are sufficient to find the paper in question. The
-- format of this line is:
--
-- > RL    citation
--
-- This field may span across multiple lines.
--
--}

--{2 RL

RL : Type
RL = String

--}

--{2 Parser

parseRL : Parser RL
parseRL = (\x,y => case y of [] => x; _ => concat (x :: " " :: (intersperse " " y)))
      <$> rlTok
      <*> (many (endOfLine *> rlTok))
  where
    rlTok : Parser String
    rlTok = string "RL" *!> whitespace *!> (pack <$> many (noneOf "\n"))

--}

--}

-----------------------------------------------------------------------------------[ Record Entry ]
--{1

--{2 Documentation
--
-- This is the parser and data type to read in an entire reference block.
--
--}

--{2 Ref

record Ref where
  constructor MkRef
  rn : RN
  rp : RP
  rc : Maybe RC
  rx : Maybe RX
  rg : Maybe RG
  ra : Maybe RA
  rt : Maybe RT
  rl : RL

Eq Ref where
  (==) (MkRef rn1 rp1 rc1 rx1 rg1 ra1 rt1 rl1) (MkRef  rn2 rp2 rc2 rx2 rg2 ra2 rt2 rl2) =
    (rn1 == rn2) && (rp1 == rp2) && (rc1 == rc2) &&
    (rx1 == rx2) && (rg1 == rg2) && (ra1 == ra2) &&
    (rt1 == rt2) && (rl1 == rl2)

Show Ref where
  show (MkRef n p c x g a t l) = (concat . List.intersperse "\n")
    [ "RN: " ++ show n
    , "RP: " ++ p
    , "RC: " ++ show c
    , "RX: " ++ show x
    , "RG: " ++ show g
    , "RA: " ++ show a
    , "RT: " ++ show t
    , "RL: " ++ show l
    ]

--}

--{2 Parser

parseRef : Parser Ref
parseRef = MkRef <$> parseRN
                 <*> parseRP
                 <*> opt parseRC
                 <*> opt parseRX
                 <*> opt parseRG
                 <*> opt parseRA
                 <*> opt parseRT
                 <*> parseRL
                 <*! endOfLine

--}

--}


