-----------------------------------------------------------------------------------------[ Module ]
--{1
--                                                                              (\_/)
--                                                                              (o.O)
--                                                                              (> <)
--                                                                             #######
--                                                                           KILLER BUNNY
--                                                                             APPROVED

module Database.Relations

import Data.AA.Map               as M
import Data.AA.Set.IndexMultiSet as IMS

import Database.Types
import Database.Arrows
import Database.Graph

%default total
%access private

%flag C "-O3"
%flag C "-g"

--}

-------------------------------------------------------------------------------[ Raw Graph Arrows ]
--{1

public export
record DagArrow where
  constructor MkDagArr
  dom : UTy
  cod : UTy
  rel : Arrow


public export
Show DagArrow where
  show (MkDagArr d c r) = show d <+> " -> " <+> show c


ppDagArr : Map Nat String -> UTy -> UTy -> String
ppDagArr m d c = (ppUTy m d) <+> " -> " <+> (ppUTy m c)

--}

------------------------------------------------------------------------------[ Graph Arrow Errors]
--{1

export
data DagError : Type where
  ArrowNotFound : (dom : String) -> (cod : String) -> DagError
  ProdMismatch  : (edom : UTy) -> (rdom : UTy) -> DagError
  SumMismatch   : (ecod : UTy) -> (rcod : UTy) -> DagError
  CompMismatch  : (edom : UTy) -> (rdom : UTy) -> DagError


showArrowMismatch : String -> String -> String -> String -> String
showArrowMismatch d1 c1 d2 c2 = "Type mismatch on second argument : \n"
                       <+> "  expected  '" <+> d1 <+> " -> " <+> c1 <+> "'\n"
                       <+> "  recieved  '" <+> d2 <+> " -> " <+> c2 <+> "'\n"


export
Show DagError where
  show e with (e)
    | ArrowNotFound  dom  cod = "Arrow '" <+> dom <+> " -> " <+> cod <+> "' does not exist!"
    | ProdMismatch  edom rdom = showArrowMismatch (show edom) "_" (show rdom) "_"
    | SumMismatch   ecod rcod = showArrowMismatch "_" (show ecod) "_" (show rcod)
    | CompMismatch  edom rdom = showArrowMismatch (show edom) "_" (show rdom) "_"


ppDagErr : Map Nat String -> DagError -> String
ppDagErr m err with (err)
  | ArrowNotFound d c  = "Arrow " <+> d <+> "->" <+> c <+> " does not exist!"
  | ProdMismatch d1 d2 = showArrowMismatch (ppUTy m d1) "_" (ppUTy m d2) "_"
  | SumMismatch  c1 c2 = showArrowMismatch "_" (ppUTy m c1)  "_" (ppUTy m c2)
  | CompMismatch d1 d2 = showArrowMismatch (ppUTy m d1) "_" (ppUTy m d2) "_"

--}

------------------------------------------------------------------------------[ Safe Graph Arrows ]
--{1

public export
MDag : Type
MDag = Either DagError DagArrow


export
mkDagM : DataBase -> String -> String -> MDag
mkDagM db dom cod with (getArrow db dom cod)
  | Left   _      = Left  $ ArrowNotFound dom cod
  | Right (d,c,r) = Right $ MkDagArr d c r


export
ppArrow : Map Nat String -> MDag -> String
ppArrow m marr with (marr)
  | Left err = ppDagErr m err
  | Right (MkDagArr d c r) = ppDagArr m d c

--}

----------------------------------------------------------------------------[ Relation Operations ]
--{1

export
comp : MDag -> MDag -> MDag
comp (Left e)  _        = Left e
comp  _       (Left e)  = Left e
comp (Right $ MkDagArr d1 c1 r1) (Right $ MkDagArr d2 c2 r2) with (c1 == d2)
  | False = Left  $ CompMismatch c1 d2
  | True  = Right $ MkDagArr d1 c2 (r1 :.: r2)


export
prod : MDag -> MDag -> MDag
prod (Left e)  _        = Left e
prod  _       (Left e)  = Left e
prod (Right $ MkDagArr d1 c1 r1) (Right $ MkDagArr d2 c2 r2) with (d1 == d2)
  | False = Left  $ ProdMismatch d1 d2
  | True  = Right $ MkDagArr d1 (Prod c1 c2) (r1 :*: r2)


export
sum : MDag -> MDag -> MDag
sum (Left e)  _        = Left e
sum  _       (Left e)  = Left e
sum (Right $ MkDagArr d1 c1 r1) (Right $ MkDagArr d2 c2 r2) with (c1 == c2)
  | False = Left  $ SumMismatch c1 c2
  | True  = Right $ MkDagArr (Sum d1 d2) c1 (r1 :+: r2)


export
dag : MDag -> MDag
dag (Left e) = Left e
dag (Right $ MkDagArr d c r) = Right $ MkDagArr c d (dag_op r)

--}



