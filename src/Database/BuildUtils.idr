-----------------------------------------------------------------------------------------[ Module ]
--{1
--                                                                              (\_/)
--                                                                              (o.O)
--                                                                              (> <)
--                                                                             #######
--                                                                           KILLER BUNNY
--                                                                             APPROVED

module Database.BuildUtils

import Data.AA.Map               as M
import Data.AA.Set.NatIso        as NI
import Data.AA.Set.MultiSet      as MS
import Data.AA.Set.IndexMultiSet as IMS

--import Control.Arrows.PointSetCat
--import Control.Arrows.MultiArrow

%default total
%access private

%flag C "-O3"
%flag C "-g"

--}

----------------------------------------------------------------------------------[ Idris Prelude ]
--{1

public export
(Ord a, Ord b) => Ord (Either a b) where
  compare l r with (l,r)
    | (Left  x , Left  y) = compare x y
    | (Right x , Right y) = compare x y
    | (Left  _ , Right _) = LT
    | (Right _ , Left  _) = GT

--}

---------------------------------------------------------------------------------[ Universe Space ]
--{1

export
data UTy : Type where
  Atom : Int -> UTy
  Prod : UTy -> UTy -> UTy
  Sum  : UTy -> UTy -> UTy


export
Eq UTy where
  -- Not sure how to write the assert_smaller here...
  -- So we use the more radical assert_total for now...
  (==) t1 t2 with (t1,t2)
    | (Atom x     , Atom y    ) = x == y
    | (Prod x1 x2 , Prod y1 y2) = assert_total $ x1 == y1 && x2 == y2
    | (Sum  x1 x2 , Sum  y1 y2) = assert_total $ x1 == y1 && x2 == y2
    | (_          , _         ) = False


export
Ord UTy where
  -- Not sure how to write the assert_smaller here...
  -- So we use the more radical assert_total for now...
  compare t1 t2 with (t1,t2)
    | (Atom x     , Atom y    ) = compare x y
    | (Prod x1 x2 , Prod y1 y2) = assert_total $
                                  case compare x1 y1 of
                                       LT => LT
                                       EQ => compare x2 y2
                                       GT => GT
    | (Sum  x1 y1 , Sum  x2 y2) = assert_total $
                                  case compare x1 y1 of
                                       LT => LT
                                       EQ => compare x2 y2
                                       GT => GT
    | (Atom _     , _         ) = LT
    | (_          , Atom _    ) = GT
    | (Sum  _ _   , _         ) = GT
    | (_          , Sum  _ _  ) = LT


export
Show UTy where
  -- Not sure how to write the assert_smaller here...
  -- So we use the more radical assert_total for now...
  show ty with (ty)
    | Atom x   = show x
    | Prod x y = assert_total $ "(" <+> show x <+> "*" <+> show y <+> ")"
    | Sum  x y = assert_total $ "(" <+> show x <+> "+" <+> show y <+> ")"

--}

---------------------------------------------------------------------------------[ Universe Arrow ]
--{1

export
data DTy : Type where
  A : Int            -> DTy
  P : Pair   DTy DTy -> DTy
  S : Either DTy DTy -> DTy

Eq DTy where
  -- Not sure how to write the assert_smaller here...
  -- So we use the more radical assert_total for now...
  (==) t1 t2 with (t1,t2)
    | (A x , A y) = x == y
    | (P x , P y) = assert_total $ x == y
    | (S x , S y) = assert_total $ x == y
    | (_   , _  ) = False

Ord DTy where
  -- Not sure how to write the assert_smaller here...
  -- So we use the more radical assert_total for now...
  compare t1 t2 with (t1,t2)
    | (A x , A y) = compare x y
    | (P x , P y) = assert_total $ compare x y
    | (S x , S y) = assert_total $ compare x y
    | (A _ , _  ) = LT
    | (_   , A _) = GT
    | (S _ , _  ) = GT
    | (_   , S _) = LT

public export
Arrow : Type
Arrow = IMSet DTy DTy


||| Build a model of the relations between domain and codomain.
export
mkRel : List (DTy,DTy) -> Arrow
mkRel = foldr alg empty
  where alg : (DTy,DTy) -> Arrow -> Arrow
        alg (k,v) r = insert k (MS.singleton v) r


||| Apply a multiset to a relation, treating the relation like a function.
||| Returns the resulting multiset.
export
app : (f : Arrow) -> (xs : MSet DTy) -> MSet DTy
app f xs = foldr alg empty xs
  where alg : Cell DTy -> MSet DTy -> MSet DTy
        alg (Elem v n) r with (find v f)
          | Nothing = r
          | Just ys = union ys r


infixr 9 :.:
||| Compose two relations into a composite relation.
export
(:.:) : Arrow -> Arrow -> Arrow
(:.:) fmap gmap = foldr alg empty fmap
  where alg : IMS.Cell DTy DTy -> Arrow -> Arrow
        alg (Bin i x) acc = insert i (app gmap x) acc


infixr 9 :*:
||| Take the cross product between two relations iff the elements share their domain element.
export
(:*:) : Arrow -> Arrow -> Arrow
(:*:) fmap gmap = foldr alg empty fmap
  where alg : IMS.Cell DTy DTy -> Arrow -> Arrow
        alg (Bin i x) acc with (IMS.find i gmap)
          | Nothing = acc
          | Just y  = let vs = MS.fromList [ P (x',y') | x' <- elems x, y' <- elems y ]
                      in insert i vs acc


infixr 8 :+:
||| Coproduct between two arrows.
export
(:+:) : Arrow -> Arrow -> Arrow
(:+:) fmap gmap = foldr ralg (foldr lalg empty fmap) gmap
  where
    lalg : IMS.Cell DTy DTy -> Arrow -> Arrow
    lalg (Bin i x) acc = insert (S $ Left i) x acc

    ralg : IMS.Cell DTy DTy -> Arrow -> Arrow
    ralg (Bin i x) acc = insert (S $ Right i) x acc

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
export
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


export
encodeData : DataBase -> String -> MSet String -> MSet DTy
encodeData (MkDB _ enum _) lbl ms with (find lbl enum)
  | Nothing = empty
  | Just ty = MS.foldr (\(Elem x n),acc => case NI.find x ty of
                                             Nothing => acc
                                             Just x' => insert (A $ cast x') acc
                       ) empty ms


--export
--applyArrow : Arrow -> List (MSet DTy) -> MSet DTy
--applyArrow db dom cod xs with (getArrow db dom cod)
--  | Left   _      = empty
--  | Right (d c r) = r (encodeData db dom $ foldr (insert . show) empty xs)

--}

----------------------------------------------------------------------------[ Dagger Database DSL ]
--{1

public export
record DagArrow where
  constructor MkDagArr
  dom : UTy
  cod : UTy
  rel : Arrow

export
Show DagArrow where
  show (MkDagArr d c r) = show d <+> " -> " <+> show c

public export
DagError : Type
DagError = String

public export
MDag : Type
MDag = Either DagError DagArrow

export
mkDagM : DataBase -> String -> String -> MDag
mkDagM db dom cod with (getArrow db dom cod)
  | Left   _      = Left $ "Arrow '" <+> dom <+> " -> " <+> cod <+> "' does not exist!"
  | Right (d,c,r) = Right $ MkDagArr d c r


export
comp : MDag -> MDag -> MDag
comp (Left e)  _        = Left e
comp  _       (Left e)  = Left e
comp (Right $ MkDagArr d1 c1 r1) (Right $ MkDagArr d2 c2 r2) with (c1 == d2)
  | False = Left $ "Type mismatch on second argument : \n"
                 <+> "  expected '" <+> show c1 <+> " -> _'\n"
                 <+> "  recieved '" <+> show d2 <+> " -> _'"
  | True  = Right $ MkDagArr d1 c2 (r1 :.: r2)


export
prod : MDag -> MDag -> MDag
prod (Left e)  _        = Left e
prod  _       (Left e)  = Left e
prod (Right $ MkDagArr d1 c1 r1) (Right $ MkDagArr d2 c2 r2) with (d1 == d2)
  | False = Left $ "Type mismatch on second argument : \n"
                 <+> "  expected '" <+> show d1 <+> " -> _'\n"
                 <+> "  recieved '" <+> show d2 <+> " -> _'"
  | True  = Right $ MkDagArr d1 (Prod c1 c2) (r1 :*: r2)


export
sum : MDag -> MDag -> MDag
sum (Left e)  _        = Left e
sum  _       (Left e)  = Left e
sum (Right $ MkDagArr d1 c1 r1) (Right $ MkDagArr d2 c2 r2) with (c1 == c2)
  | False = Left $ "Type mismatch on second argument : \n"
                 <+> "  expected  '_ -> " <+> show c1 <+> "'\n"
                 <+> "  recieved  '_ -> " <+> show c2 <+> "'"
  | True  = Right $ MkDagArr (Sum c1 c2) c1 (r1 :+: r2)

--}



