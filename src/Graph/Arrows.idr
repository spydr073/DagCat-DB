-----------------------------------------------------------------------------------------[ Module ]
--{1
--                                                                              (\_/)
--                                                                              (o.O)
--                                                                              (> <)
--                                                                             #######
--                                                                           KILLER BUNNY
--                                                                             APPROVED

module Graph.Arrows

import Data.AA.Set.MultiSet      as MS
import Data.AA.Set.IndexMultiSet as IMS

import Graph.Types

%default total
%access private

%flag C "-O3"
%flag C "-g"

--}

---------------------------------------------------------------------------------[ Universe Arrow ]
--{1

public export
Arrow : Type
Arrow = IMSet UTy UTy


||| Build a model of the relations between domain and codomain.
export
mkRel : List (UTy,UTy) -> Arrow
mkRel = foldr alg empty
  where alg : (UTy,UTy) -> Arrow -> Arrow
        alg (k,v) r = insert k (MS.singleton v) r


||| Apply a multiset to a relation, treating the relation like a function.
||| Returns the resulting multiset.
export
app : (f : Arrow) -> (xs : MSet UTy) -> MSet UTy
app f xs = foldr alg empty xs
  where alg : MS.Cell UTy -> MSet UTy -> MSet UTy
        alg (Elem v n) r with (find v f)
          | Nothing = r
          | Just ys = union ys r


infixr 9 :.:
||| Compose two relations into a composite relation.
export
(:.:) : Arrow -> Arrow -> Arrow
(:.:) fmap gmap = foldr alg empty fmap
  where alg : IMS.Cell UTy UTy -> Arrow -> Arrow
        alg (Bin i x) acc = insert i (app gmap x) acc


infixr 9 :*:
||| Take the cross product between two relations iff the elements share their domain element.
export
(:*:) : Arrow -> Arrow -> Arrow
(:*:) fmap gmap = foldr alg empty fmap
  where alg : IMS.Cell UTy UTy -> Arrow -> Arrow
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
    lalg : IMS.Cell UTy UTy -> Arrow -> Arrow
    lalg (Bin i x) acc = insert (S $ Left i) x acc

    ralg : IMS.Cell UTy UTy -> Arrow -> Arrow
    ralg (Bin i x) acc = insert (S $ Right i) x acc


||| Dagger operator
export
dag_op : Arrow -> Arrow
dag_op = IMS.foldr alg empty
  where alg : IMS.Cell UTy UTy -> Arrow -> Arrow
        alg (Bin i xs) acc = MS.foldr (\(Elem k n),m => insert k (singleton i) m) acc xs

--}


