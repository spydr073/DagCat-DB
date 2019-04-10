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

%flag C "-O3"
%flag C "-g"

--}

---------------------------------------------------------------------------------------[ ID Field ]
--{1

--{2 Documentation
--
-- The 'ID' (Identification) field contains top level metadata about a sequence. The format for
-- this line is:
--
-- >  ID   EntryName Status; SequenceLength.
--
-- [ EntryName ]:
--
-- A unique entry identifier. Useful for protein identification purposes, but the
-- accession number is more stable for this purpose. It consists of a single line.
--
-- For Swiss-Prot, this entry is up to 11 characters long in the format X_Y, where X is an
-- alphanumeric representation for protein name and Y is an alphanumeric representation for the
-- biological source of the protein (typically first three letters of genus and first two letters
-- of species).
--
-- TrEMBL consists of a similar format; however, it is up to 16 characters long and the X
-- position is the accession number rather than a protein phonetic code.
--
-- [ Status ]:
--
-- The status line indicates whether a protein entry has been manually reviewed.
--
-- For Swiss-Prot, this field reads: Reviewed
--
-- For TrEMBL, this field reads: Unreviewed
--
-- [ Sequence Length ]:
--
-- This is the length of the molecule, which is the total number of amino acids in the sequence.
-- This number includes the positions reported to be present but which have not been determined
-- (coded as 'X'). The length is followed by the letter code 'AA' (Amino Acids).
--
--}

--{2  Status

public export
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

--{2 Documentation
--
-- The 'AC' (Accession Numbers) field contains the list of accession numbers assigned to a protein
-- entry. The format for this field is:
--
-- >  AC   AC_number_1;[ AC_number_2;]...[ AC_number_N;]
--
-- This field can span across lines. Each line will always begin with the 'AC' field identifier.
--
-- Accession numbers is to provide a stable way of identifying entries from release to release.
-- The first number is the primary accession number. This uniquely identifies a protein. The
-- following (optional) accession numbers are sorted lexicographically. When entries are merged or
-- split, a new primary accession number is assigned and the previous accession number is kept for
-- archival purposes.
--
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

--{2 Documentation
--
-- The 'DT' (Date Timestamp) field contains the timestamp information about entry creation and
-- modification. The field consists of exactly 3 lines formatted as:
--
-- > DT   DD-MMM-YYYY, integrated into UniProtKB/DBNAME.
-- > DT   DD-MMM-YYYY, sequence version X.
-- > DT   DD-MMM-YYYY, entry version X.
--
-- [ Line 1 ]:
--
-- This line contains the date (DD-MMM-YYYY) that the entry was accepted into the database
-- (DBNAME). Valid databases are UniProtKB, Swiss-Prot or TrEMBL.
--
-- [ Line 2 ]:
--
-- This line contains the last date (DD-MMM-YYYY) that the sequence data of the entry was modified
-- (MM-DDD-YYYY) as well as the current version of the sequence (X).
--
-- [ Line 3 ]:
--
-- This line contains the last date (DD-MMM-YYYY) that the non-sequence data of the entry was
-- modified (MM-DDD-YYYY) as well as the current version of the entry (X).
--
--
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

--{2 Documentation
--
-- The 'DE' (Protein Description) field contains basic descriptive information about the protein.
-- This information is generally enough to uniquely identify a protein. This field is comprised of
--
--   * RecName  : 1 (Swiss-Prot) or 0-1 (TrEMBL) name recommended by the UniProt consortium
--   * AltName  : 0-n alternate names which are synonyms of the recommended name
--   * SubName  : 0 (Swiss-Prot) or 0-n (TrEMBL) subnames provided by the submitter of the
--                underlying nucleotide sequence.
--   * Includes : 0-n names of additional functional domains
--   * Contains : 0-n names of cleaved functional components
--   * Flags    : 0-1 flags about the protein sequence
--
-- [ RecName ]:
--
-- The RecName field takes the following format:
--
-- > DE   RecName: Full=X;    #-- 1     The full name.
-- > DE            Short=X;   #-- 0-n   An abbreviation of the full name or an acronym.
-- > DE            EC=X;      #-- 0-n   An Enzyme Commission number.
--
--
-- [ AltName ]:
--
-- There are 5 different formats that AltName fields can follow:
--
-- > DE   AltName: Full=X;         #-- 0-1   The full name.
-- > DE            Short=X;        #-- 0-n   An abbreviation of the full name or an acronym.
-- > DE            EC=X;           #-- 0-n   An Enzyme Commission number.
--
-- > DE   AltName: Allergen=X;     #-- 0-1   See allergen.txt.
--
-- > DE   AltName: Biotech=X;      #-- 0-1   A name used in a biotechnological context.
--
-- > DE   AltName: CD_antigen=X;   #-- 0-n   See cdlist.txt.
--
-- > DE   AltName: INN=X;          #-- 0-n   The international non-proprietary name: A generic name
--                                 #--       for a pharmaceutical substance or active
--                                 #--       pharmaceutical ingredient that is globally recognized
--                                 #--       and is a public property.
--
--
-- [ SubName ]:
--
-- The subname field takes the following format:
--
-- > DE   SubName: Full=X;    #-- 1   The full name.
-- > DE            EC=X;      #-- 0-n An Enzyme Commission number.
--
--
-- [ Includes ]:
--
-- If a protein is known to include multiple functional domains each of which is described by a
-- different name, the description starts with the name of the overall protein, followed by
-- 'Includes:' section(s). Each section could have RecName, AltName, and Subname components,
-- exactly as described above.
--
-- Each Includes field then has the following format:
--
-- > DE   Includes:
-- > DE      RecName=...
-- > DE      AltName=...
-- > DE      ...
-- > DE      SubName=...
-- > DE      ...
--
--
-- [ Contains ]:
--
-- If a protein is known to be cleaved into multiple functional components, the description starts
-- with the name of the precursor protein, followed by 'Contains:' section(s). Each individual
-- component is described in a separate 'Contains:' section. Each section could have RecName,
-- AltName, and Subname components, exactly as described above.
--
-- Each Contains field then has the following format:
--
-- > DE   Contains:
-- > DE      RecName=...
-- > DE      AltName=...
-- > DE      ...
-- > DE      SubName=...
-- > DE      ...
--
--
-- [ Flags ]:
--
-- When the mature form of a protein is derived by processing of a precursor, we indicate this
-- fact using the Flag 'Precursor'; in such cases the sequence displayed does not correspond to
-- the mature form of the protein.
--
-- If the complete sequence is not determined, we indicate it in the 'Flags' section with
-- 'Fragment' or 'Fragments'.
--
-- There are 4 different formats that Flags field can follow:
--
-- > DE   Flags: Precursor;
--
-- > DE   Flags: Precursor Fragment;
--
-- > DE   Flags: Fragment;
--
-- > DE   Flags: Fragments;
--
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
  show (MkName f s e) = (concat $ List.intersperse " "
       [ "full: " ++ f
       , "short: " ++ show s
       , "enzyme: " ++ show e
       ])

--}

--{2 AltName

public export
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

public export
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

--{2 Documentation
--
-- The 'GN' (Gene) field contains the name(s) of the gene(s) that code for the stored protein
-- sequence. This field is comprised of 4 optional tokens: Name, Synonyms, OrderedLocusNames, and
-- ORFNames. Note that each GN field could spread across lines.
--
-- The format for this field is:
--
-- > GN   Name=<name>; Synonyms=<name1>[, <name2>...]; OrderedLocusNames=<name1>[, <name2>...];
-- > GN   ORFNames=<name1>[, <name2>...];
--
-- or
--
-- > GN   Name=Jon99Cii; Synonyms=SER1, SER5, Ser99Da; ORFNames=CG7877;
-- > GN   and
-- > GN   Name=Jon99Ciii; Synonyms=SER2, SER5, Ser99Db; ORFNames=CG15519;
--
-- When there is more than one associated gene. Each additional gene is prefixed with an 'and'
-- line.
--
-- [ Name ]:
--
-- The recomended gene name.
--
-- [ Synonyms ]:
--
-- A list of additional gene names that are not official
--
-- [ OrderedLocusNames ]:
--
-- A name used to represent an ORF in a completely sequenced genome or chromosome. If two predicted
-- genes have been merged to form a new gene, both gene identifiers are indicated, separated by a
-- slash. Also called  OLN, ORF numbers, CDS numbers or Gene numbers.
--
-- [ ORFNames ]:
--
-- A name temporarily attributed by a sequencing project to an open reading frame. This name is
-- generally based on a cosmid numbering system.
--
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
              <$> opt (((endOfLine *> gn) <|> gn <|> whitespace) *>
                       (tok $ string str) *> sepBy nameStr (char ',') <* (tok $ char ';'))

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


