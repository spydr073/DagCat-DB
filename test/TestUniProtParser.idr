-----------------------------------------------------------------------------------------[ Module ]
--{1
--                                                                              (\_/)
--                                                                              (o.O)
--                                                                              (> <)
--                                                                             #######
--                                                                           KILLER BUNNY
--                                                                             APPROVED

module TestUniProtParser

import Database.UniProt.Types
import Database.UniProt.Parser

import UniProtTest.Protein
import UniProtTest.Organism
import UniProtTest.Reference
import UniProtTest.Sequence

import Testing.Unit

import Data.AA.Map

%flag C "-O3"
%flag C "-g"

--}

-------------------------------------------------------------------------------------[  Run Tests ]
--{1

runTests : (Eq a, Show a) => String -> Parser a -> List (String, a) -> IO ()
runTests name parser tests = (putStrLn $ "Running " ++ name ++ " test suite...")
                          *> (go Z tests)
                          *> (putStrLn $ "End of " ++ name ++ " tests!\n")
  where
    printTest : (Eq a, Show a) => (String, a) -> IO ()
    printTest (s,e) = case parse parser s of
                           Left err => putStrLn err
                           Right v  => assertEq "" e v

    go : (Eq a, Show a) => Nat -> List (String, a) -> IO ()
    go n ts with (ts)
      | Nil       = pure ()
      | (t::rest) = (putStr $ "Test " ++ show n ++ ": ")
                 *> (printTest t)
                 *> (go (n + 1) rest)


tests :  Map String (IO ())
tests = fromList
  [ ("id" , runTests "ID" parseID id_tests)
  , ("ac" , runTests "AC" parseAC ac_tests)
  , ("dt" , runTests "DT" parseDT dt_tests)
  , ("de" , runTests "DE" parseDE de_tests)
  , ("gn" , runTests "GN" parseGN gn_tests)

  , ("os" , runTests "OS" parseOS os_tests)
  , ("og" , runTests "OG" parseOG og_tests)
  , ("oc" , runTests "OC" parseOC oc_tests)
  , ("ox" , runTests "OX" parseOX ox_tests)
  , ("oh" , runTests "OH" parseOH oh_tests)

  , ("rn" , runTests "RN" parseRN rn_tests)
  , ("rp" , runTests "RP" parseRP rp_tests)
  , ("rc" , runTests "RC" parseRC rc_tests)
  , ("rx" , runTests "RX" parseRX rx_tests)
  , ("rg" , runTests "RG" parseRG rg_tests)
  , ("ra" , runTests "RA" parseRA ra_tests)
  , ("rt" , runTests "RT" parseRT rt_tests)
  , ("rl" , runTests "RL" parseRL rl_tests)

  , ("cc" , runTests "CC" parseCC cc_tests)
  , ("dr" , runTests "DR" parseDR dr_tests)
  , ("pe" , runTests "PE" parsePE pe_tests)
  , ("kw" , runTests "KW" parseKW kw_tests)
  , ("ft" , runTests "FT" parseFT ft_tests)
  , ("sq" , runTests "SQ" parseSQ sq_tests)
  ]


namespace Main

  main : IO ()
  main = do
    [_,fn] <- getArgs | putStrLn "No field selected"
    if fn == "all"
      then sequence_ (valueList tests)
      else case find fn tests of
             Just t  => t
             Nothing => putStrLn "Data field does not exist!"

--}



