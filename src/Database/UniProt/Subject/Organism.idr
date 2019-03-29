-----------------------------------------------------------------------------------------[ Module ]
--{1
--                                                                              (\_/)
--                                                                              (o.O)
--                                                                              (> <)
--                                                                             #######
--                                                                           KILLER BUNNY
--                                                                             APPROVED

module Database.UniProt.Subject.Organism

import Database.UniProt.Utils

import Lightyear
import Lightyear.Char
import Lightyear.Strings

%default partial
%access export

%flag C "-O3"
%flag C "-g"

--}

---------------------------------------------------------------------------------------[ OS Field ]
--{1

--{2 Documentation
--
-- The 'OS' (Organism Species) field specifies the organism that the protein sample was obtained
-- from. The line is formatted as:
--
-- > OS   Name[ (Synonym)]...[ (Synonym)].
--
-- Generally, the Name field follows the format 'Genus Species'. The Synonyms, or common names,
-- are given in English.
--
--}

--{2 OS

record OS where
  constructor MkOS
  species : String
  common  : List String

Eq OS where
  (==) (MkOS x1 x2) (MkOS y1 y2) = (x1 == y1) && (x2 == y2)

Show OS where
  show (MkOS n c) = (concat $ List.intersperse "\n"
       [ "name: " ++ show n
       , "common: "  ++ show c
       ])

--}

--{2 Parser

parseOS : Parser OS
parseOS = MkOS <$> (os *> name) <*> (common <* endOfLine)
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

--}

---------------------------------------------------------------------------------------[ OG Field ]
--{1

--{2 Documentation
--
-- The 'OG' (Organelle) field indicates if the gene coding for a protein originates from
-- mitochondria, a plastid, a nucleomorph or a plasmid. Possible values for an OG line are:
--
-- > OG   Hydrogenosome.
--
-- > OG   Mitochondrion.
--
-- > OG   Nucleomorph.
--
-- > OG   Plasmid plasmid-name.
--
-- > OG   Plastid.
--
-- > OG   Plastid; plastid-type.
--
-- The 'plasmid-name' value is an assigned string name.
--
-- The 'plastid-name' values can be assigned any of the following:
--
--     * Apicoplast
--
--     * Chloroplast
--
--     * Organellar chromatophore
--
--     * Cyanelle
--
--     * Non-photosynthetic plastid
--
-- If there are multiple organelles present, the plasmid names are separated by commas and the last
-- plasmid name is preceded by the word 'and'. Plasmid names are never written across two lines.
-- Example:
--
-- > OG   Plasmid R6-5, Plasmid IncFII R100 (NR1), and
-- > OG   Plasmid IncFII R1-19 (R1 drd-19).
--
--
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

--{2 Parser

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

--}

---------------------------------------------------------------------------------------[ OC Field ]
--{1

--{2 Documentation
--
-- The 'OC' (Organism Classification) field specifies the taxonomic classification of the organism.
-- This information is maintained at NCBI-Taxonomy, and is used by the nucleotide sequence
-- databases (EMBL/GenBank/DDBJ). The NCBI's taxonomy reflects current phylogenetic knowledge.
--
-- The classification is listed top-down as nodes in a taxonomic tree in which the most general
-- grouping is given first. The classification may be distributed over several OC lines, but nodes
-- are not split or hyphenated between lines. Semicolons separate the individual items and the list
-- is terminated by a period.
--
-- > OC   Node[; Node...].
--
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

--{2 Parser

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

--}

---------------------------------------------------------------------------------------[ OX Field ]
--{1

--{2 Documentation
--
-- The 'OX' (Organism Taxonomy Cross-Reference) field indicates the identifier of a specific
-- organism in a taxonomic database. The format is:
--
-- > OX   Taxonomy_database_Qualifier=Taxonomic code;
--
-- Currently the cross-references are made to the taxonomy database of NCBI, which is associated
-- with the qualifier 'TaxID' and a taxonomic code. Example:
--
-- > OX   NCBI_TaxID=9606;
--
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

--{2 Parser

parseOX : Parser OX
parseOX = MkOX
     <$> (pack <$> ((tok (string "OX")) *!> many (noneOf "=") <*! char '='))
     <*> (pack <$> (many (noneOf ";")) <*! char ';') <*! endOfLine
     <?> "OX Field"

--}

--}

---------------------------------------------------------------------------------------[ OH Field ]
--{1

--{2 Documentation
--
-- The 'OH' (Organism Host) field is optional and appears only in viral entries. It indicates the
-- host organism(s) that are susceptible to be infected by a virus. The format for each OH entry
-- is:
--
-- > OH   NCBI_TaxID=TaxID; HostName.
--
-- Where HostName follows the same format as the OS field.
--
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
  show (MkOH t n cs) = (concat . List.intersperse " ") [ "Host:", show t, n, show cs ]

--}

--{2 Parser

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

--}



