-----------------------------------------------------------------------------------------[ Module ]
--{1
--                                                                              (\_/)
--                                                                              (o.O)
--                                                                              (> <)
--                                                                             #######
--                                                                           KILLER BUNNY
--                                                                             APPROVED

module Database.UniProt.Subject.Protein

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

---------------------------------------------------------------------------------------[ ID Field ]
--{1

--{2  Status

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

--{2 UID

record UID  where
  constructor MkUID
  name   : String
  status : Status
  seqLen : Int

Eq UID where
  (==) (MkUID n1 s1 q1) (MkUID n2 s2 q2) = (n1 == n2) && (s1 == s2) && (q1 == q2)

Show UID where
  show (MkUID n s q) = "Protein:\n" <+> (concat $ List.intersperse "\n"
       [ "uid: " ++ n
       , "status: " ++ show s
       , "length: " ++ show q
       ])

--}

--{2 Parser

parseID : Parser UID
parseID = MkUID <$> name <*!> status <*!> seqLen <*! endOfLine
      <?> "Protein ID"
  where
    validChar : Parser Char
    validChar = satisfy isAlphaNum <|>| oneOf "_"

    name : Parser String
    name = pack <$> ((tok $ string "ID") *!> (tok $ some validChar))

    status : Parser Status
    status = (string "Reviewed" *!> pure Reviewed)
        <|>| (string "Unreviewed" *!> pure Unreviewed)

    seqLen : Parser Int
    seqLen = ((fromJust 0) . parseInteger . pack)
         <$> ((tok $ char ';') *!> (tok $ many (satisfy isDigit)) <*! string "AA.")

--}

--}

---------------------------------------------------------------------------------------[ AC Field ]
--{1

--{2 AC

record AC where
  constructor MkAC
  accessions : List String

Eq AC where
  (==) (MkAC a1) (MkAC a2) = a1 == a2

Show AC where
  show (MkAC as) = "accs: " <+> show as

--}

--{2 Parser

parseAC : Parser AC
parseAC = (\x,y => MkAC $ x <+> y)
          <$> concat <$> (acStart *!> (many $ acLine <* endOfLine <* acStart))
          <*> acLine <*! endOfLine
          <?> "Accession Numbers"
  where
    validChar : Parser Char
    validChar = satisfy isAlphaNum <|>| oneOf "_-"

    acStart : Parser ()
    acStart = string "AC" *!> whitespace

    acLine : Parser (List String)
    acLine = some (pack <$> (some validChar) <* (tok $ char ';'))

--}

--}

---------------------------------------------------------------------------------------[ DT Field ]
--{1

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
  show (MkDT i d m mv e ev) = (concat $ List.intersperse "\n"
       [ "initial : " ++ i
       , "db: " ++ d
       , "seq: " ++ m ++ " version " ++ show mv
       , "entry: " ++ e ++ " version " ++ show ev
       ])

--}

--{2 Parser

parseDT : Parser DT
parseDT = MkDT <$> dtStr
              <*!> ((dtCC "integrated into UniProtKB/") <*! endOfLine)
              <*!> dtStr
              <*!> (((fromJust 0) . parseInteger) <$> (dtCC "sequence version ") <*! endOfLine)
              <*!> dtStr
              <*!> (((fromJust 0) . parseInteger) <$> (dtCC "entry version ") <*! endOfLine)
      <?> "DT field"
  where
    validChar : Parser Char
    validChar = satisfy isAlphaNum <|>| oneOf "_-"

    dtStr : Parser String
    dtStr = pack <$> ((tok $ string "DT") *!> (tok $ some validChar <*! char ','))

    dtCC : String -> Parser String
    dtCC txt = pack <$> (string txt *!> some validChar <*! char '.')

--}

--}

---------------------------------------------------------------------------------------[ DE Field ]
--{1

--{2 Name

record Name where
  constructor MkName
  fullName  : String
  shortName : List String
  ecNum     : List String

Eq Name where
  (==) (MkName f1 s1 e1) (MkName f2 s2 e2) = (f1 == f2) && (s1 == s2) && (e1 == e2)

Show Name where
  show (MkName f s e) = (concat $ List.intersperse " "
       [ "full: " ++ f
       , "short: " ++ show s
       , "enzyme: " ++ show e
       ])

--}

--{2 AltName

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

--}

--{2 Flag

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
  show (MkDE n a s i c f) =(concat $ List.intersperse "\n"
       [ "recomended: "      ++ show n
       , "alternative: " ++ show a
       , "subname: "     ++ show s
       , "includes: "    ++ show i
       , "contains: "    ++ show c
       , "flags: "       ++ show f
       ])

--}

--{2 Parser

parseDE : Parser DE
parseDE = MkDE <$>  opt (nameNode "RecName:")
              <*!> many altName
              <*!> many (nameNode "SubName:")
              <*!> many include
              <*!> many contain
              <*!> flags
              <?> "DE field"
  where
    deStr : Parser ()
    deStr = string "DE" *> whitespace

    nameStr : Parser String
    nameStr = (trim . pack) <$> (some (noneOf ";{"))
                            <*! many (noneOf ";")         -- chomp evidence tokens
                            <*! char ';' <*! endOfLine

    names : String -> String -> Parser String
    names field desc = deStr *> opt (tok $ string field)
                    *> string desc *!> char '=' *!> nameStr

    ecNumber : Parser String
    ecNumber = deStr *> (tok $ string "EC=") *!> nameStr

    nameNode : String -> Parser Name
    nameNode field = MkName <$> names field "Full"
                           <*!> many (names field "Short")
                           <*!> many ecNumber

    altType : String -> Parser String
    altType ty = deStr *> (tok $ string "AltName:") *!> string ty *!> nameStr

    altName : Parser AltName
    altName = (Simple    <$> nameNode "AltName:")
         <|>| (Allergen  <$> altType  "Allergen=")
         <|>| (Biotech   <$> altType  "Biotech=")
         <|>| (CDAntigen <$> altType  "CD_antigen=")
         <|>| (INN       <$> altType  "INN=")

    include : Parser (Maybe Name, List AltName, List Name)
    include = (\x,y,z => (x,y,z)) <$> (deStr *> (tok $ string "Includes:") *!> endOfLine
           *!> opt (nameNode "RecName:"))
          <*!> many altName
          <*!> many (nameNode "SubName:")

    contain : Parser (Maybe Name, List AltName, List Name)
    contain = (\x,y,z => (x,y,z)) <$> (deStr *> (tok $ string "Contains:") *!> endOfLine
           *!> opt (nameNode "RecName:"))
          <*!> many altName
          <*!> many (nameNode "SubName:")

    flags : Parser (List Flag)
    flags = (fromJust []) <$>
            opt (deStr *> (tok $ string "Flags:") *>
                 many ((tok $ string "Fragment;"  *!> pure Fragment)  <|>|
                       (tok $ string "Fragments;" *!> pure Fragments) <|>|
                       (tok $ string "Precursor;" *!> pure Precursor)
                      ) <*! endOfLine)

--}

--}

---------------------------------------------------------------------------------------[ GN Field ]
--{1

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
  show (MkGN n s l o) = (concat $ List.intersperse "\n"
       [ "gene: "  ++ show n
       , "syn: "  ++ show s
       , "loci: " ++ show l
       , "orf: "  ++ show o
       ])

--}

--{2 Parser

parseGN : Parser (List GN)
parseGN = (::) <$> gene <*> many (gnAnd *> gene) <?> "GN field"
  where
    gn : Parser ()
    gn = string "GN" *> whitespace

    gnAnd : Parser ()
    gnAnd = gn *> (tok $ string "and") *> endOfLine *> pure ()

    nameStr : Parser String
    nameStr = (trim . pack) <$> (many $ noneOf ",;{") <* optEvidence

    optList : String -> Parser (List String)
    optList str = (fromJust [])
              <$> (opt (gn <|>| (endOfLine *> gn)) *>
                   opt ((tok $ string str) *>
                         sepBy nameStr (char ',') <*
                         (tok $ char ';')))

    name : Parser (Maybe String)
    name = opt (gn *> (tok $ string "Name=") *> nameStr <* (tok $ char ';'))

    syn  : Parser (List String)
    syn = optList "Synonyms="

    loci : Parser (List String)
    loci = optList "OrderedLocusNames="

    orf  : Parser (List String)
    orf = optList "ORFNames="

    gene : Parser GN
    gene = MkGN <$> name <*> syn <*> loci <*> orf <* endOfLine

--}

--}


