-----------------------------------------------------------------------------------------[ Module ]
--{1
--                                                                              (\_/)
--                                                                              (o.O)
--                                                                              (> <)
--                                                                             #######
--                                                                           KILLER BUNNY
--                                                                             APPROVED

module TestDB

import GraphDB

import Data.AA.Map               as M
import Data.AA.Set.NatIso        as NISO

%default total
%access private

%flag C "-O3"
%flag C "-g"

--}

-----------------------------------------------------------------------------[ Define Data Schema ]
--{1

--  > DB Table
--  +------------------+
--  | name | uid | age |
--  +------+-----+-----+
--  | eve  | 123 | 21  |
--  | bob  | 456 | 21  |
--  | eve  | 789 | 40  |
--  +------------------+

schema : NatIso String
schema = enumerate ["name", "uid", "age"]

schema' : Map Nat String
schema' = decodeNatIso schema

--}

---------------------------------------------------------------------------------[ Build by Field ]
--{1

nameField : Field
nameField = mkField "name" ["eve", "bob", "eve"]

uidField : Field
uidField = mkField "uid" $ show <$> [123, 456, 789]

ageField : Field
ageField = mkField "age" $ show <$> [21, 21, 40]

all_fields : Map String (NatIso String)
all_fields = buildFields [ nameField
                         , uidField
                         , ageField
                         ]


--}

------------------------------------------------------------------------------[ Build by Relation ]
--{1

name_age : Either String (Relation, Relation)
name_age = mkDagRel schema nameField ageField

name_uid : Either String (Relation, Relation)
name_uid = mkDagRel schema nameField uidField

age_uid : Either String (Relation, Relation)
age_uid = mkDagRel schema ageField uidField

all_rels : Map (UTy,UTy) Arrow
all_rels = buildRelations [ name_age
                          , name_uid
                          , age_uid
                          ]

--}

-------------------------------------------------------------------------------------------[ Main ]
--{1

namespace Main

  testdb : DataBase
  testdb = MkDB schema all_fields all_rels

  pp : MDag -> String
  pp = ppArrow schema'


  f  : MDag
  f  = mkDagM testdb "name" "age"
  f' : MDag
  f' = mkDagM testdb "age"  "name"

  g  : MDag
  g  = mkDagM testdb "uid"  "age"
  g' : MDag
  g' = mkDagM testdb "age"  "uid"

  h  : MDag
  h  = mkDagM testdb "uid"  "name"
  h' : MDag
  h' = mkDagM testdb "name" "uid"

  q1 : MDag
  q1 = comp f g'

  q2 : MDag
  q2 = prod f' g'

  q3 : MDag
  q3 = sum f g

  q4 : MDag
  q4 = comp f g'

  q5 : MDag
  q5 = comp f (comp g' h)

  q6 : MDag
  q6 = comp f (comp g' g)

  q7 : MDag
  q7 = comp (sum h f') h'

  q8 : MDag
  q8 = dag q1

  q9 : MDag
  q9 = comp f f'

  main : IO ()
  main = do
    putStrLn "Done!"

--}


