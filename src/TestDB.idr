-----------------------------------------------------------------------------------------[ Module ]
--{1
--                                                                              (\_/)
--                                                                              (o.O)
--                                                                              (> <)
--                                                                             #######
--                                                                           KILLER BUNNY
--                                                                             APPROVED

module TestDB

import Data.AA.Map               as M
import Data.AA.Set.NatIso        as NI
import Data.AA.Set.MultiSet      as MS
import Data.AA.Set.IndexMultiSet as IMS

import Database.BuildUtils

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
name_age = BuildUtils.mkDagRel schema nameField ageField

name_uid : Either String (Relation, Relation)
name_uid = BuildUtils.mkDagRel schema nameField uidField

age_uid : Either String (Relation, Relation)
age_uid = BuildUtils.mkDagRel schema ageField uidField

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

  main : IO ()
  main = do

    let f1 = name_age
    --let f2 = name_uid
    --let f3 = age_uid

    putStrLn "Done!"

--}


