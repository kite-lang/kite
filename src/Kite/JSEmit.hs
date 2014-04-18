module Kite.JSEmit where

import Control.Monad
import Text.Printf

import Kite.Parser

type Source = String

reserved = ["while", "for"]
kitePrefix = "KITE_"

runtime = readFile "js/kt_runtime.js"

codegen :: Expr -> IO Source
codegen expr = do
  r <- runtime
  let r' = filter (not . (=='\n')) r
  return (emit expr ++ r' ++ "main();")

emit :: Expr -> Source

emit (PInteger val) = show val
emit (PFloat val) = show val
emit (PString val) = "\"" ++ val ++ "\""
emit (PBool val) = if val then "true" else "false"

emit (PIdentifier ide) =
  if ide `elem` reserved
    then kitePrefix ++ ide
    else ide

emit (PList elems) = printf "[%s]" (emitAll "," elems)

emit (PIf cond conseq alt) =
  printf "KT_IF(function() { return %s; })(function() { return %s; })(function() { return %s; })" (emit cond) (emit conseq) (emit alt)

emit (PFunc (PFuncType param _) body) =
  let PTypeArg _ ide = param
  in printf "(function(%s) {%s})" (emit ide) (emit body)

emit (PAssign ide expr) =
  printf "var %s = %s" (emit ide) (emit expr)

emit (PBlock _ exprs) =
  emitAll ";" exprs

emit (PReturn expr) =
  "return " ++ emit expr

emit (PCall expr arg) =
  printf "%s(%s)" (emit expr) (emit arg)

emitAll :: String -> [Expr] -> Source
emitAll delim = foldl (\acc x -> acc ++ emit x ++ delim) ""
