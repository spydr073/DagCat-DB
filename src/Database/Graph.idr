
-----------------------------------------------------------------------------------------[ Module ]
--{1
--                                                                              (\_/)
--                                                                              (o.O)
--                                                                              (> <)
--                                                                             #######
--                                                                           KILLER BUNNY
--                                                                             APPROVED

module Database.Graph

import Data.AA.Tree              as T
import Data.AA.Map               as M
import Data.AA.Set.NatIso        as NISO
import Data.AA.Set.IndexMultiSet as IMS

import Database.Types
import Database.Arrows

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


export
decodeNatIso : NatIso String -> Map Nat String
decodeNatIso (NI _ t) = foldr alg empty $ T.toList t
  where alg : NISO.Cell String -> Map Nat String -> Map Nat String
        alg (Elem s n) m = bind (KV n s) m


export
showDecoded : Show a => Nat -> Map Nat String -> String
showDecoded n m with (find n m)
  | Nothing = "Index Not Found"
  | Just a  = show a


mapEnum : NatIso String -> List String -> List Nat
mapEnum e lst = go lst []
  where go : List String -> List Nat -> List Nat
        go l acc with (l)
          | []      = acc
          | (x::xs) = case NISO.find x e of
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
  key : (UTy , UTy)
  rel : Arrow


export
mkDagRel : NatIso String -> Field -> Field -> Either String (Relation , Relation)
mkDagRel scma f1 f2 with (find (field_name f1) scma, find (field_name f2) scma)
  | (Nothing , _      ) = Left "Domain type does not exist"
  | (_       , Nothing) = Left "Codomain type does not exist"
  | (Just la , Just lb) = let as = mapEnum (field_enum f1) (field_data f1)
                              bs = mapEnum (field_enum f2) (field_data f2)
                          in case compare (length as) (length bs) of
                               LT => Left "Too many entries in the codomain"
                               GT => Left "Too many entries in the domain"
                               EQ => Right $ ( MkRel (Atom $ cast la, Atom $ cast lb)
                                                   (mkRel $ zipWith (\x,y => ( A $ cast x
                                                                             , A $ cast y))
                                                                      as bs)
                                             , MkRel (Atom $ cast lb, Atom $ cast la)
                                                     (mkRel $ zipWith (\x,y => ( A $ cast x
                                                                               , A $ cast y))
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
  primArrows : Map (UTy,UTy) Arrow


export
empty : DataBase
empty = MkDB empty empty empty


-- does not handle conflicts
insertRel : Field -> Field -> DataBase -> DataBase
insertRel f1 f2 db@(MkDB scma fs arrs) =
  let scma' = (insert $ field_name f1) . (insert $ field_name f2) $ scma
      fs'   = (bind $ KV (field_name f1) (field_enum f1))
            . (bind $ KV (field_name f2) (field_enum f2))
            $ fs
      rels  = mkDagRel scma' f1 f2
  in case rels of
       Left  err => MkDB scma' fs' arrs
       Right (MkRel k1 r1, MkRel k2 r2) => let arrs' = (bind $ KV k1 r1)
                                                     . (bind $ KV k2 r2)
                                                     $ arrs
                                           in MkDB scma' fs' arrs'


export
buildRelations : List (Either String (Relation,Relation)) -> Map (UTy,UTy) Arrow
buildRelations = foldr insertBiEdge empty
  where insertBiEdge : Either String (Relation, Relation)
                    -> Map (UTy,UTy) Arrow
                    -> Map (UTy,UTy) Arrow
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
getArrow : DataBase -> String -> String -> Either String (UTy, UTy, Arrow)
getArrow (MkDB scma _ primArr) dom cod with (find dom scma, find cod scma)
  | (Nothing , _      ) = Left "Domain does not exist."
  | (_       , Nothing) = Left "Codomain does not exist."
  | (Just x  , Just y ) = let d = Atom $ cast x
                              c = Atom $ cast y
                          in case Map.find (c,d) primArr of
                               Nothing => Left "Unable to find requested relation."
                               Just r  => Right (d, c, r)



--encodeData : DataBase -> String -> MSet String -> MSet DTy
--encodeData (MkDB _ enum _) lbl ms with (find lbl enum)
--  | Nothing = empty
--  | Just ty = MS.foldr (\(Elem x n),acc => case NISO.find x ty of
--                                             Nothing => acc
--                                             Just x' => insert (A $ cast x') acc
--                       ) empty ms


--export
--applyArrow : Arrow -> List (MSet DTy) -> MSet DTy
--applyArrow db dom cod xs with (getArrow db dom cod)
--  | Left   _      = empty
--  | Right (d c r) = r (encodeData db dom $ foldr (insert . show) empty xs)

--}
