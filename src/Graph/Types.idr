-----------------------------------------------------------------------------------------[ Module ]
--{1
--                                                                              (\_/)
--                                                                              (o.O)
--                                                                              (> <)
--                                                                             #######
--                                                                           KILLER BUNNY
--                                                                             APPROVED

module Graph.Types

import Data.AA.Tree as T
import Data.AA.Map  as M

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

-----------------------------------------------------------------------------------[ Type Masking ]
--{1

typeMask  : Bits64
typeMask  = 0xFFFF000000000000

valueMask : Bits64
valueMask = 0x0000FFFFFFFFFFFF

export
encode64 : (Bits64, Bits64) -> Either String Bits64
encode64 (t,v) = if      t > 0xFFFF    then Left "Type out of range"
                 else if v > valueMask then Left "Value out of range"
                 else Right $ prim__orB64 (prim__shlB64 t 48) v

export
decode64 : Bits64 -> (Bits64, Bits64)
decode64 b = (prim__lshrB64 (prim__andB64 b typeMask) 48 , prim__andB64 b valueMask)

--}

---------------------------------------------------------------------------------[ Universe Space ]
--{1

public export
data UTy : Type where
  A : Bits64         -> UTy
  P : Pair   UTy UTy -> UTy
  S : Either UTy UTy -> UTy

export
Eq UTy where
  -- Not sure how to write the assert_smaller here...
  -- So we use the more radical assert_total for now...
  (==) t1 t2 with (t1,t2)
    | (A x , A y) = x == y
    | (P x , P y) = assert_total $ x == y
    | (S x , S y) = assert_total $ x == y
    | (_   , _  ) = False


export
Ord UTy where
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


typeOf : Bits64 -> Bits64
typeOf = (fst . decode64)

infix 6 ~=
export
(~=) : UTy -> UTy -> Bool
(~=) t1 t2 with (t1,t2)
    | (A x         , A y        ) = typeOf x == typeOf y
    | (P (x1,y1)   , P (x2,y2)  ) = assert_total $ x1 ~= x2 && y1 ~= y2
    | (S (Left x)  , S (Left y) ) = assert_total $ x ~= y
    | (S (Right x) , S (Right y)) = assert_total $ x ~= y
    | (_           , _          ) = False



--export
--Show UTy where
--  -- Not sure how to write the assert_smaller here...
--  -- So we use the more radical assert_total for now...
--  show ty with (ty)
--    | A x     = typeOf x
--    | P (x,y) = assert_total $ "(" <+> (typeOf x) <+> "*" <+> (typeOf y) <+> ")"
--    | S x     = assert_total $ "(" <+> show x <+> "+" <+> () <+> ")"
--    where typeOf : Bits64 -> Bits64
--          typeOf = (show . fst . decode64)

--}


