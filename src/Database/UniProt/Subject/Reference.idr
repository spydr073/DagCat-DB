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

-- %flag C "-O3"
-- %flag C "-g"

--}

---------------------------------------------------------------------------------------[ RN Field ]
--{1

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


