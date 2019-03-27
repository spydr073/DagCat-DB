-----------------------------------------------------------------------------------------[ Module ]
--{1
--                                                                              (\_/)
--                                                                              (o.O)
--                                                                              (> <)
--                                                                             #######
--                                                                           KILLER BUNNY
--                                                                             APPROVED

module Database.UniProt.Subject.Sequence

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

---------------------------------------------------------------------------------------[ CC Field ]
--{1

--{2 CC

record CC where
  constructor MkCC
  topic   : String
  comment : String

Eq CC where
  (==) (MkCC t1 c1) (MkCC t2 c2) = (t1 == t2) && (c1 == c2)

Show CC where
  show (MkCC t c) = t ++ "=" ++ c

--}

--{2 Parser

parseCC : Parser (List CC)
parseCC = many (MkCC <$> topic <*!> comment)
  where
    topic : Parser String
    topic = (tok $ string "CC") *!> (tok $ string "-!-")
         *!> (pack <$> many (noneOf ":"))
         <*! (tok $ char ':')

    comment : Parser String
    comment = (\x,y => concat $ intersperse " " (x::y))
          <$> ((opt (endOfLine *> (tok $ string "CC"))) *> (pack <$> many (noneOf "\n")))
          <*!> many (endOfLine *> string "CC" *> whitespace
                     *> requireFailure (string "-!-")
                     *!> (pack <$> many (noneOf "\n"))) <*! endOfLine

--}

--}

---------------------------------------------------------------------------------------[ DR Field ]
--{1

--{2 DR

record DR where
  constructor MkDR
  abbr : String
  uid  : String
  info : List String

Eq DR where
  (==) (MkDR a1 u1 i1) (MkDR a2 u2 i2) = (a1 == a2) && (u1 == u2) && (i1 == i2)

Show DR where
  show (MkDR a u i) = (concat $ List.intersperse " "
       [ "(db : " ++ a
       , "id: "   ++ u
       , "info: " ++ show i ++ ")"
       ])

--}

--{2 Parser

parseDR : Parser (List DR)
parseDR = many (MkDR <$> abbr <*!> uid <*!> info)
  where
    optBracket : Parser ()
    optBracket = whitespace <*! char '[' <*! many (noneOf "]") <*! char ']'

    abbr : Parser String
    abbr = string "DR" *!> whitespace
        *!> (pack <$> many (noneOf ";")) <*! tok (char ';')

    uid : Parser String
    uid = pack <$> many (noneOf ";") <*! tok (char ';')

    clean : List String -> List String
    clean = foldr (\x,acc => if x == "-" || x == "-."
                               then acc
                               else x::acc
                  ) []

    info : Parser (List String)
    info = let str = with List (((trimBy (=='.')) . pack) <$> (many (noneOf ";\n")))
           in (\x,y => clean (x::y))
              <$> tok str
              <*> many (char ';' *> opt (endOfLine *> string "RP") *> whitespace *> str)
              <*! endOfLine

--}

--}

---------------------------------------------------------------------------------------[ PE Field ]
--{1

--{2 PE

data PE = EvidenceAtProteinLevel
        | EvidenceAtTranscriptLevel
        | InferredFromHomology
        | Predicted
        | Uncertain

Eq PE where
  (==) x y with (x,y)
    | (EvidenceAtProteinLevel    , EvidenceAtProteinLevel)    = True
    | (EvidenceAtTranscriptLevel , EvidenceAtTranscriptLevel) = True
    | (InferredFromHomology      , InferredFromHomology)      = True
    | (Predicted                 , Predicted)                 = True
    | (Uncertain                 , Uncertain)                 = True
    | _                                                       = False

Show PE where
  show x with (x)
    | EvidenceAtProteinLevel    = "EvidenceAtProteinLevel"
    | EvidenceAtTranscriptLevel = "EvidenceAtTranscriptLevel"
    | InferredFromHomology      = "InferredFromHomology"
    | Predicted                 = "Predicted"
    | Uncertain                 = "Uncertain"

--}

--{2 Parser

parsePE : Parser PE
parsePE = string "PE" *!> whitespace *!>
   (    ((string "1: Evidence at protein level;")
          *!> pure EvidenceAtProteinLevel)
   <|>| ((string "2: Evidence at transcript level;")
          *!> pure EvidenceAtTranscriptLevel)
   <|>| ((string "3: Inferred from homology;")
          *!> pure InferredFromHomology)
   <|>| ((string "4: Predicted;")
          *!> pure Predicted)
   <|>| ((string "5: Uncertain;")
          *!> pure Uncertain)
   ) <*! endOfLine

--}

--}

---------------------------------------------------------------------------------------[ KW Field ]
--{1

--{2 KW

record KW where
  constructor MkKW
  keywords : List String

Eq KW where
  (==) (MkKW kw1) (MkKW kw2) = kw1 == kw2

Show KW where
  show (MkKW x) = "keywords: " ++ show x

--}

--{2 Parser

parseKW : Parser KW
parseKW = MkKW <$> (mllst "KW" ';' '.') <*! endOfLine

--}

--}

---------------------------------------------------------------------------------------[ FT Field ]
--{1

--{2 FT

record FT where
  constructor MkFT
  name : String
  from : String
  to   : String
  desc : List String

Eq FT where
  (==) (MkFT n1 f1 t1 d1) (MkFT n2 f2 t2 d2) = (n1 == n2) && (f1 == f2)
                                            && (t1 == t2) && (d1 == d2)

Show FT where
  show (MkFT n f t d) = (concat $ List.intersperse " "
       [ "(name : " ++ n
       , "from : "  ++ f
       , "to : "    ++ t
       , "desc: "   ++ show d ++ ")"
       ])

--}

--{2 Parser

-- Whitespace is important here!
--   cols    field
--   ---------------------
--   1-2     FT
--   6-13    Key name
--   15-20   From endpoint
--   22-27   To endpoint
--   35-75   Description

parseFT : Parser (List FT)
parseFT = many (MkFT <$> name <*!> from <*!> to <*!> desc)
  where
    name : Parser String
    name = string "FT   " *!> (trim . pack) <$> ntimes 8 anyChar <*! char ' '

    from : Parser String
    from = ((trim . pack) <$> ntimes 6 anyChar <* char ' ')
      <|>| pure ""

    to : Parser String
    to = ((trim . pack) <$> ntimes 6 anyChar <* char ' ')
      <|>| pure ""

    cleanFT : List String -> List String
    cleanFT lst with (lst)
      | Nil        = Nil
      | [x]        = [x]
      | (x::y::xs) = case last' (unpack x) of
                       Nothing => cleanFT (y::xs)
                       Just c => if c /= '.'
                                   then cleanFT ((x++y)::xs)
                                   else (x :: cleanFT (y::xs))

    desc : Parser (List String)
    desc = ((\x,y => cleanFT (x::y))
            <$> ((trim . pack) <$> many (noneOf "\n"))
            <*> (many (endOfLine *> string "FT    " *> whitespaces
                       *> ((trim . pack) <$> many (noneOf "\n")))) <* endOfLine)
      <|>| pure []

--}

--}

---------------------------------------------------------------------------------------[ SQ Field ]
--{1

--{2 SQ

record SQ where
  constructor MkSQ
  length : Int
  mass   : Int
  crc64  : String
  seq    : String

Eq SQ where
  (==) (MkSQ l1 m1 c1 s1) (MkSQ l2 m2 c2 s2) = (l1 == l2) && (m1 == m2)
                                            && (c1 == c2) && (s1 == s2)

Show SQ where
  show (MkSQ l m c s) = (concat $ List.intersperse " "
       [ "(len : "  ++ show l
       , "mass : "  ++ show m
       , "crc64 : " ++ c
       , "seq: "    ++ s ++ ")"
       ])

--}

--{2 Parser

parseSQ : Parser SQ
parseSQ = string "SQ" *!> whitespaces *!> string "SEQUENCE" *!> whitespace
       *> MkSQ <$> len <*!> mass <*!> crc64 <*!> seq <*! endOfLine
  where
    len : Parser Int
    len = ((fromJust 0) . parseInteger . pack)
      <$> ((tok $ many (satisfy isDigit)) <*! tok (string "AA;"))


    mass : Parser Int
    mass  = ((fromJust 0) . parseInteger . pack)
        <$> ((tok $ many (satisfy isDigit)) <*! tok (string "MW;"))

    crc64 : Parser String
    crc64 = pack <$> (many $ noneOf " ") <*! whitespace <*! string "CRC64;" <*! endOfLine

    seq : Parser String
    seq = concat <$> some ((opt endOfLine) *> whitespace *> (pack <$> some upper))

--}

--}


