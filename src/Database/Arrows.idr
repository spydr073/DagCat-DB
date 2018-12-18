
-----------------------------------------------------------------------------------------[ Module ]
--{1
--                                                                              (\_/)
--                                                                              (o.O)
--                                                                              (> <)
--                                                                             #######
--                                                                           KILLER BUNNY
--                                                                             APPROVED

module Database.Arrows

import Data.AA.Set.MultiSet      as MS
import Data.AA.Set.IndexMultiSet as IMS

import Database.Types

%default total
%access private

%flag C "-O3"
%flag C "-g"

--}

---------------------------------------------------------------------------------[ Universe Arrow ]
--{1

public export
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
  where alg : MS.Cell DTy -> MSet DTy -> MSet DTy
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


||| Dagger operator
export
dag_op : Arrow -> Arrow
dag_op = IMS.foldr alg empty
  where alg : IMS.Cell DTy DTy -> Arrow -> Arrow
        alg (Bin i xs) acc = MS.foldr (\(Elem k n),m => insert k (singleton i) m) acc xs

--}


