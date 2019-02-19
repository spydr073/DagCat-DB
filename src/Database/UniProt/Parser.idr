-----------------------------------------------------------------------------------------[ Module ]
--{1
--                                                                              (\_/)
--                                                                              (o.O)
--                                                                              (> <)
--                                                                             #######
--                                                                           KILLER BUNNY
--                                                                             APPROVED

module Database.UniProt.Parser

import Database.UniProt.Types

import public Lightyear
import public Lightyear.Char
import public Lightyear.Strings
import public Lightyear.StringFile

import Data.String
import Data.Vect

%default partial
%access export

%flag C "-O3"
%flag C "-g"

--}

------------------------------------------------------------------------------------------[ Utils ]
--{1

fromJust : a -> Maybe a -> a
fromJust def x with (x)
  | Nothing = def
  | Just  v = v

||| Execute a string parser on a file.
parseFile' : Parser a -> String -> Eff (Either String a) [FILE ()]
parseFile' p fname = do
    Result src <- readFile fname
      | FError err => pure (Left $ "File Error! - " ++ show err)
    case parse p src of
      Left  err => pure (Left $ "Parse Error! -  " ++ show err)
      Right res => pure (Right res)

whitespace : Parser ()
whitespace = skip $ many (oneOf " \t")

whitespaces : Parser ()
whitespaces = skip $ some (oneOf " \t")

-- override lightyear 'lexeme' as newlines are significant
tok : Parser a -> Parser a
tok p = p <* whitespace

-- match evidence token... and trash it
optEvidence : Parser ()
optEvidence = whitespace *> opt (char '{' *> many (noneOf "}") *> char '}') *> whitespace

||| Multiline string with end token:
|||   @pre : the uniprot field prefix
|||   @end : the terminating char
mlstr : (pre : String) -> (end : Char) -> Parser String
mlstr pre end = let str = with List (pack <$> some (noneOf (pack [end,'\n'])))
                in (\x,y => concat (intersperse " " (x::y)))
              <$> ((opt (endOfLine *> string pre)) *> whitespace *> str)
              <*> (many (endOfLine *> string pre *> whitespace *> str))

||| Multiline string without end token:
|||   @pre : the uniprot field prefix
mlstr' : (pre : String) -> Parser String
mlstr' pre = let str = pack <$> some (noneOf "\n")
             in (\x,y => concat (intersperse " " (x::y)))
           <$> ((opt (endOfLine *> string pre)) *> whitespace *> str)
           <*> (many (endOfLine *> string pre *> whitespace *> str))

||| Multiline list:
|||   @pre : the UniProt field prefix
|||   @sep : the seperator char
|||   @end : the terminating string
mllst : (pre : String) -> (sep : Char) -> (end : Char) -> Parser (List String)
mllst pre sep end = let str = with List (pack <$> many (noneOf $ pack [sep, end, '\n']))
                    in (\x,y => (x::y))
                  <$> (string pre *> whitespace *> tok str)
                  <*> many ((char sep) *> opt (endOfLine *> string pre) *> whitespace *> str)
                      <* char end

--}

---------------------------------------------------------------------------------------[ ID Field ]
--{1

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

---------------------------------------------------------------------------------------[ AC Field ]
--{1

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

---------------------------------------------------------------------------------------[ DT Field ]
--{1

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

---------------------------------------------------------------------------------------[ DE Field ]
--{1

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

---------------------------------------------------------------------------------------[ GN Field ]
--{1

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

---------------------------------------------------------------------------------------[ OS Field ]
--{1

parseOS : Parser OS
parseOS = MkOS <$> (os *> name) <*> common
     <?> "OS field"
  where
    os : Parser ()
    os = string "OS" *> whitespace

    str : String -> Parser String
    str xs = (trim . pack) <$> many (noneOf xs)

    name : Parser String
    name = (\x,y => if y == "" then x else x ++ " " ++ y)
       <$> str "(.)\n"
       <*> (concat . intersperse " ") <$> (many (endOfLine *> os *> str "()\n"))
       <?> "OS species name"

    format : (lvl : Int) -> (acc : List Char) -> (res : List String)
          -> (rest : List Char) -> List String
    format lvl acc res rest with (rest)
      | []        = if lvl == 0
                      then reverse res
                      else []
      | ('('::xs) = if lvl == 0
                      then format 1 acc res xs
                      else format (lvl + 1) ('('::acc) res xs
      | (')'::xs) = if lvl == 1
                      then format 0 [] ((pack $ reverse acc)::res) xs
                      else format (lvl - 1) (')'::acc) res xs
      | (x::xs)   = if lvl == 0
                      then format lvl acc res xs
                      else format lvl (x::acc) res xs

    common : Parser (List String)
    common = ((format 0 [] []) . unpack) <$> (mlstr' "OS")
        <?> "OS common names"

--}

---------------------------------------------------------------------------------------[ OG Field ]
--{1

parseOG : Parser (List OG)
parseOG = many (((nonPlasmid <*! many (noneOf ".") <*! (tok $ char '.'))
                 <|>| plasmid) <* endOfLine)
      <?> "OG Field"
  where
    og : Parser String
    og = tok $ string "OG"

    plasmidStr : Parser String
    plasmidStr = tok (string "Plasmid")

    nonPlasmid : Parser OG
    nonPlasmid = og *>
      (    ((string "Hydrogenosome") *!> pure Hydrogenosome)
      <|>| ((string "Mitochondrion") *!> pure Mitochondrion)
      <|>| ((string "Nucleomorph")   *!> pure Nucleomorph)

      <|>| ((string "Plastid; Apicoplast")
               *!> pure (Plastid PlastidApicoplast))
      <|>| ((string "Plastid; Chloroplast")
               *!> pure (Plastid PlastidChloroplast))
      <|>| ((string "Plastid; Organellar chromatophore")
               *!> pure (Plastid PlastidOrganellarChromatophore))
      <|>| ((string "Plastid; Cyanelle")
               *!> pure (Plastid PlastidCyanelle))
      <|>| ((string "Plastid; Non-photosynthetic plastid")
               *!> pure (Plastid PlastidNonPhotosynthetic))
      <|>| ((string "Plastid")
               *!> pure (Plastid PlastidSimple))
      )

    str : Parser String
    str = (trim . pack) <$> many (noneOf ",.")

    plasmid : Parser OG
    plasmid = (\x,y => Plasmid (x::y))
          <$> (og *!> plasmidStr *!> str)
          <*> many ((char ',' *!> whitespace) *!> (opt (tok (string "and")))
                    *> (opt (endOfLine *!> og)) *!> plasmidStr *!> str) <*! char '.'

--}

---------------------------------------------------------------------------------------[ OC Field ]
--{1

parseOC : Parser OC
parseOC = (\x,y => MkOC (x::y)) <$> (os *!> str) <*!> (taxa <*! (tok $ char '.')) <*! endOfLine
      <?> "OC Field"
  where
    os : Parser ()
    os = string "OC" *!> whitespace

    str : Parser String
    str = (trim . pack) <$> many (noneOf ";.\n")

    taxa : Parser (List String)
    taxa = many ((tok $ char ';') *!> (opt (endOfLine *!> os)) *!> str)
       <?> "OC taxa list"

--}

---------------------------------------------------------------------------------------[ OX Field ]
--{1

parseOX : Parser OX
parseOX = MkOX
     <$> (pack <$> ((tok (string "OX")) *!> many (noneOf "=") <*! char '='))
     <*> (pack <$> (many (noneOf ";")) <*! char ';') <*! endOfLine
     <?> "OX Field"

--}

---------------------------------------------------------------------------------------[ OH Field ]
--{1

parseOH : Parser (List OH)
parseOH = many (MkOH <$> tid <*!> name <*!> (common <*! char '.' <*! (opt endOfLine)))
     <?> "OH Field"
  where
    oh : Parser ()
    oh = (string "OH") *> whitespace

    tid : Parser (String,String)
    tid = MkPair
     <$> (pack <$> (oh *!> many (noneOf "=") <*! char '='))
     <*!> (pack <$> (many (noneOf ";")) <*! (tok (char ';')))

    str : Parser String
    str = (trim . pack) <$> many (noneOf "().\n")

    name : Parser String
    name = (\x,y => if y == "" then x else x ++ " " ++ y)
       <$> str
       <*!> (concat . intersperse " ") <$> (many (endOfLine *!> oh *!> str))
       <?> "OH species name"

    common : Parser (List String)
    common = many (whitespace *> (opt $ endOfLine *!> oh) *> char '(' *!> name <*! char ')')
        <?> "OH common names"

--}

-------------------------------------------------------------------------------[ Reference Fields ]
--{1

rn : Parser Int
rn = ((fromJust 0) . parseInteger . pack)
 <$> ((string "RN") *!> whitespace *!> char '['
     *!> (many $ satisfy isDigit)
     <*! char ']') <* optEvidence
 <?> "RN reference number"


rp : Parser String
rp = (concat . (intersperse " "))
 <$> many (endOfLine *> string "RP" *> whitespace *> (pack <$> (many (noneOf "\n"))))
 <?> "RP reference position"


rc : Parser (List (RCom, String))
rc = many (opt (endOfLine *> string "RC" *> whitespace)
           *> (MkPair <$> rcFst <*!> (rcSnd <* tok (char ';'))))
     <?> "RC reference comment"
  where
    rcFst : Parser RCom
    rcFst = ((string "STRAIN=")     *!> pure STRAIN)
       <|>| ((string "PLASMID=")    *!> pure PLASMID)
       <|>| ((string "TRANSPOSON=") *!> pure TRANSPOSON)
       <|>| ((string "TISSUE=")     *!> pure TISSUE)

    rcSnd : Parser String
    rcSnd = pack <$> (many ((noneOf ";\n")
                      <*! (opt (endOfLine *!> string "RC" *!> whitespace))))


rx : Parser (List (BibDB, String))
rx = many (opt (endOfLine *> string "RX" *!> whitespace)
           *> (MkPair <$> rxFst <*> (rxSnd <* tok (char ';'))))
  where
    rxFst : Parser BibDB
    rxFst = ((string "MEDLINE=")  *!> pure MEDLINE)
       <|>| ((string "PubMed=")   *!> pure PubMed)
       <|>| ((string "DOI=")      *!> pure DOI)
       <|>| ((string "AGRICOLA=") *!> pure AGRICOLA)

    rxSnd : Parser String
    rxSnd = pack <$> (many ((noneOf ";\n")
                      <* (opt (endOfLine *> string "RX" *> whitespace))))


rg : Parser (List String)
rg = (fromJust []) <$> opt (endOfLine *> string "RG" *!> many ((mlstr "RG" ';') <*! char ';'))


ra : Parser (List String)
ra = (fromJust []) <$> opt (endOfLine *> mllst "RA" ',' ';')


rt : Parser (Maybe String)
rt = opt (endOfLine *> string "RT" *!> mlstr "RT" ';') <*! char ';'


rl : Parser String
rl = (concat . intersperse " ") <$>
     (many (endOfLine *> string "RL" *!> whitespace *!> (pack <$> many (noneOf "\n"))))


--  rn : Int
--  rp : String
--  rc : List (RCom, String)
--  rx : List (BibDB, String)
--  rg : List String
--  ra : List String
--  rt : Maybe String
--  rl : String

parseRef : Parser Ref
parseRef = MkRef <$> rn <*> rp <*> rc <*> rx <*> rg <*> ra <*> rt <*> rl <*! endOfLine

--}

---------------------------------------------------------------------------------------[ CC Field ]
--{1

parseCC : Parser (List CC)
parseCC = many (MkCC <$> topic <*!> comment)
  where
    topic : Parser String
    topic = (tok $ string "CC") *!> (tok $ string "-!-")
         *!> (pack <$> many (noneOf ":"))
         <*! (tok $ char ':')

    comment : Parser String
    comment = (\x,y => concat $ intersperse " " (x::y))
          <$> (pack <$> many (noneOf "\n"))
          <*!> many (endOfLine *> string "CC" *> whitespace
                     *> requireFailure (string "-!-")
                     *!> (pack <$> many (noneOf "\n"))) <*! endOfLine

--}

---------------------------------------------------------------------------------------[ DR Field ]
--{1

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
    info = let str = with List (pack <$> (many (noneOf ";\n")))
           in (\x,y => clean (x::y))
              <$> tok str
              <*> many (char ';' *> opt (endOfLine *> string "RP") *> whitespace *> str)
              <*! endOfLine

--}

---------------------------------------------------------------------------------------[ PE Field ]
--{1

--  DR   SUPFAM; SSF55003; SSF55003; 1.
--  PE   1: Evidence at protein level;
--  KW   3D-structure; ATP-binding; Complete proteome;
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

---------------------------------------------------------------------------------------[ KW Field ]
--{1

parseKW : Parser KW
parseKW = MkKW <$> (mllst "KW" ';' '.') <*! endOfLine

--}

---------------------------------------------------------------------------------------[ FT Field ]
--{1

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

---------------------------------------------------------------------------------------[ SQ Field ]
--{1

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

--------------------------------------------------------------------------[ Parse UniProtKB Entry ]
--{1

-- UniProt Text File Fields:
--
--   uid  : UID              -- Identification           : Once
--   ac   : AC               -- Accession number(s)      : Once or more
--   dt   : DT               -- Date                     : Three times
--   de   : DE               -- Description              : Once or more
--   gn   : Maybe (List GN)  -- Gene name(s)             : Optional
--   os   : OS               -- Organism species         : Once or more
--   og   : Maybe (List OG)  -- Organelle                : Optional
--   oc   : OC               -- Organism classification  : Once or more
--   ox   : OX               -- Taxonomy cross-refs      : Once
--   oh   : Maybe (List OH)  -- Organism host            : Optional
--   refs : List Ref         -- References               : Once or more
--   cc   : Maybe (List CC)  -- Comments or notes        : Optional
--   dr   : Maybe (List DR)  -- Database cross-refs      : Optional
--   pe   : PE               -- Protein existence        : Once
--   kw   : Maybe KW         -- Keywords                 : Optional
--   ft   : Maybe (List FT)  -- Feature table data       : Once or more in Swiss-Prot
--                                                       , Optional in TrEMBL
--   sq   : SQ               -- Sequence header          : Once

parseEntry : Parser Entry
parseEntry = MkEntry
         <$> parseID
         <*!> parseAC
         <*!> parseDT
         <*!> parseDE
         <*!> opt parseGN
         <*!> parseOS
         <*!> opt parseOG
         <*!> parseOC
         <*!> parseOX
         <*!> opt parseOH
         <*!> many parseRef
         <*!> opt parseCC
         <*!> opt parseDR
         <*!> parsePE
         <*!> opt parseKW
         <*!> opt parseFT
         <*!> parseSQ
         <*! string "//"
         <*! opt endOfLine

--}


-------------------------------------------------------------------------------------[  Run Tests ]
--{1

namespace Main

  private
  main : IO ()
  main = do
    [_,fn] <- getArgs | putStrLn "No file selected"
    p <- run (parseFile' (many parseEntry) fn)
    putStrLn fn
    case p of
      Left err => putStrLn $ "Error: " ++ show err
      Right v  => putStrLn "Success"


--}


