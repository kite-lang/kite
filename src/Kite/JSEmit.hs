module Kite.JSEmit where

import Control.Monad.State
import Data.List

import Kite.Parser

type Source = String

emit :: Source -> Expr -> Source

emit src (PInteger val) = show val

emit src (PFunc (PFuncType params tRet) body) =
  let params' = concatMap (\p -> case p of
                              PTypeArg _ (PIdentifier ide) -> ide) params
  in src ++ "function(" ++ params' ++ ") {" ++ emit "" body ++ "}"

emit src (PAssign (PIdentifier ide) expr) =
  src ++ ide ++ " = " ++ emit src expr ++ ";"

emit src (PBlock StandardBlock exprs) =
  foldl emit src exprs

emit src (PBlock FuncBlock exprs) =
  foldl emit src exprs

emit src (PReturn expr) =
  src ++ "return " ++ emit src expr ++ ";"

emit src (PIdentifier ide) = ide

emit src (PCall expr args) =
  let args' = map (emit "") args
  in emit "" expr ++ "(" ++ (intercalate "," args') ++ ");"
