
-----------------------------------------------------------------------------------------[ Module ]
--{1
--                                                                              (\_/)
--                                                                              (o.O)
--                                                                              (> <)
--                                                                             #######
--                                                                           KILLER BUNNY
--                                                                             APPROVED

module Database.Types

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

---------------------------------------------------------------------------------[ Universe Space ]
--{1

public export
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


export
ppUTy : Map Nat String -> UTy -> String
ppUTy m ty = case ppUTy' ty of
               Left  err => err
               Right msg => msg
  where ppUTy' : UTy -> Either String String
        ppUTy' t with (t)
          | Atom x with (M.find (cast x) m)
            | Nothing  = Left $ "Type index " <+> show x <+> " does not exist!"
            | Just str = Right str
          | Prod x y = (\x',y' => x' <+> " * " <+> y') <$> (ppUTy' x) <*> (ppUTy' y)
          | Sum  x y = (\x',y' => x' <+> " + " <+> y') <$> (ppUTy' x) <*> (ppUTy' y)

--}


