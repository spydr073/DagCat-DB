-----------------------------------------------------------------------------------------[ Module ]
--{1
--                                                                              (\_/)
--                                                                              (o.O)
--                                                                              (> <)
--                                                                             #######
--                                                                           KILLER BUNNY
--                                                                             APPROVED

module Database.UniProt.Parser

import Lightyear
import Lightyear.Char
import Lightyear.Strings
import Lightyear.StringFile

import Database.UniProt.Utils

import Database.UniProt.Subject.Protein
import Database.UniProt.Subject.Organism
import Database.UniProt.Subject.Reference
import Database.UniProt.Subject.Sequence

%default partial
%access export

%flag C "-O3"
%flag C "-g"

--}

--------------------------------------------------------------------------[ Parse UniProtKB Entry ]
--{1

--{2 UniProtKB Entry

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
    ("ID: " ++ show uid  ++
     "AC: " ++ show ac   ++
     "DT: " ++ show dt   ++
     "DE: " ++ show de   ++
     "GN: " ++ show gn   ++
     "OS: " ++ show os   ++
     "OG: " ++ show og   ++
     "OC: " ++ show oc   ++
     "OX: " ++ show ox   ++
     "OH: " ++ show oh   ++
     "RF: " ++ show refs ++
     "CC: " ++ show cc   ++
     "DR: " ++ show dr   ++
     "PE: " ++ show pe   ++
     "KW: " ++ show kw   ++
     "FT: " ++ show ft   ++
     "SQ: " ++ show sq
    ) ++ "\n\n\n"

--}

--{2 Parser

parseEntry : Parser Entry
parseEntry = MkEntry
         <$>  parseID
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
         <*!  string "//"
         <*!  opt endOfLine

--}

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


