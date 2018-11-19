-----------------------------------------------------------------------------------------[ Module ]
--{1
--                                                                              (\_/)
--                                                                              (o.O)
--                                                                              (> <)
--                                                                             #######
--                                                                           KILLER BUNNY
--                                                                             APPROVED

module Database.BuildUtils

import Data.AA.Map          as M
import Data.AA.Set.NatIso   as NI
import Data.AA.Set.MultiSet as MS

import Control.Arrows.MultiArrow

%default total
%access private

%flag C "-O3"
%flag C "-g"

--}

---------------------------------------------------------------------------------[ Enumerate Sets ]
--{1

export
enumerate : List String -> NatIso String
enumerate = Foldable.foldr NatIso.insert empty


mapEnum : NatIso String -> List String -> List Nat
mapEnum e lst = go lst []
  where go : List String -> List Nat -> List Nat
        go l acc with (l)
          | []      = acc
          | (x::xs) = case NI.find x e of
                        Nothing => acc
                        Just v  => go xs (v::acc)

--}

--------------------------------------------------------------------------[ Collect Data by Field ]
--{1

public export
record Field where
  constructor MkField
  field_name : String
  field_data : List String
  field_enum : NatIso String


export
mkField : Show a => String -> List a -> Field
mkField n xs = let xs = show <$> xs
               in MkField n xs $ enumerate xs


export
buildFields : List Field -> Map String (NatIso String)
buildFields = foldr (\x,acc => Map.bind (KV (field_name x) (field_enum x)) acc) empty

--}

-------------------------------------------------------------------------[ Capture Data Relations ]
--{1

public export
record Relation where
  constructor MkRel
  key : (Int,Int)
  rel : Arrow Int Int


export
mkRel : NatIso String -> Field -> Field -> Either String (Relation,Relation)
mkRel scma f1 f2 with (find (field_name f1) scma, find (field_name f2) scma)
  | (Nothing , _      ) = Left "Domain type does not exist"
  | (_       , Nothing) = Left "Codomain type does not exist"
  | (Just la , Just lb) = let as = mapEnum (field_enum f1) (field_data f1)
                              bs = mapEnum (field_enum f2) (field_data f2)
                          in case compare (length as) (length bs) of
                               LT => Left "Too many entries in the codomain"
                               GT => Left "Too many entries in the domain"
                               EQ => Right $ ( MkRel (cast la, cast lb)
                                                     (mkArrow $ zipWith (\x,y => (cast x, cast y))
                                                                        as bs)
                                             , MkRel (cast lb, cast la)
                                                     (mkArrow $ zipWith (\x,y => (cast x, cast y))
                                                                        bs as)
                                             )

--}

---------------------------------------------------------------------------------[ Arrow Database ]
--{1

public export
record DataBase where
  constructor MkDB
  schema     : NatIso String
  fields     : Map String (NatIso String)
  primArrows : Map (Int,Int) (Arrow Int Int)


export
empty : DataBase
empty = MkDB empty empty empty


-- does not handle conflicts
export
insertRel : Field -> Field -> DataBase -> DataBase
insertRel f1 f2 db@(MkDB scma fs arrs) =
  let scma' = (insert $ field_name f1) . (insert $ field_name f2) $ scma
      fs'   = (bind $ KV (field_name f1) (field_enum f1))
            . (bind $ KV (field_name f2) (field_enum f2))
            $ fs
      rels  = mkRel scma' f1 f2
  in case rels of
       Left  err => MkDB scma' fs' arrs
       Right (MkRel k1 r1, MkRel k2 r2) => let arrs' = (bind $ KV k1 r1)
                                                     . (bind $ KV k2 r2)
                                                     $ arrs
                                           in MkDB scma' fs' arrs'


export
buildRelations : List (Either String (Relation,Relation)) -> Map (Int,Int) (Arrow Int Int)
buildRelations = foldr insertBiEdge empty
  where insertBiEdge : Either String (Relation, Relation)
                    -> Map (Int,Int) (Arrow Int Int)
                    -> Map (Int,Int) (Arrow Int Int)
        insertBiEdge res m with (res)
          | Left   _      = m
          | Right (r1,r2) = (bind $ KV (key r1) (rel r1))
                          . (bind $ KV (key r2) (rel r2))
                          $ m

--}

--------------------------------------------------------------------[ Evaluate Relation Morphisms ]
--{1

export
viewTypes : DataBase -> List String
viewTypes (MkDB scma _ _) = toList scma


export
getArrow : DataBase -> String -> String -> Either String (Arrow Int Int)
getArrow (MkDB scma _ primArr) dom cod with (find dom scma, find cod scma)
  | (Nothing , _      ) = Left "Domain does not exist."
  | (_       , Nothing) = Left "Codomain does not exist."
  | (Just x  , Just y ) = case Map.find (cast x, cast y) primArr of
                            Nothing => Left "Unable to find requested relation."
                            Just r  => Right r


export
encodeData : DataBase -> String -> MSet String -> MSet Int
encodeData (MkDB _ enum _) lbl ms with (find lbl enum)
  | Nothing = empty
  | Just ty = foldl (\acc,(Elem x n) => case NatIso.find x ty of
                                          Nothing => acc
                                          Just x' => insert (cast x') acc
                    ) empty ms


export
applyArrow : Show a => DataBase -> String -> String -> List a -> MSet Int
applyArrow db dom cod xs with (getArrow db dom cod)
  | Left  _ = empty
  | Right f = f (encodeData db dom $ foldr (insert . show) empty xs)

--}

----------------------------------------------------------------------------[ Dagger Database DSL ]
--{1

record DagArrow where
  constructor MkDagArr
  dom : String
  cod : String
  rel : Arrow Int Int


comp : DagArrow -> DagArrow -> Maybe DagArrow
comp (MkDagArr d1 c1 r1) (MkDagArr d2 c2 r2) with (c1 == d2)
  | False = Nothing
  | True  = Just $ MkDagArr d1 c2 (r1 . r2)


prod : DagArrow -> DagArrow -> Maybe DagArrow
prod (MkDagArr d1 c1 r1) (MkDagArr d2 c2 r2) with (d1 == d2)
  | False = Nothing
  | True  = ?hprod


sum : DagArrow -> DagArrow -> Maybe DagArrow
sum (MkDagArr d1 c1 r1) (MkDagArr d2 c2 r2) with (c1 == c2)
  | False = Nothing
  | True  = ?hsum

--}



