{-# LANGUAGE TemplateHaskell #-}

module Kite.JSEmit where

import Control.Monad
import Data.Maybe
import Data.FileEmbed
import qualified Data.ByteString.Char8 as Ch
import Text.Printf

import Kite.Parser
import Kite.Syntax

type Source = String

reserved = ["while", "for", "arguments"]
kitePrefix = "KT_"

runtime = $(embedFile "js/kt_runtime.js")
opNames = [('+', "KT_PLUS"),
           ('-', "KT_MINUS"),
           ('*', "KT_STAR"),
           ('/', "KT_SLASH"),
           ('%', "KT_PERCENT"),
           ('=', "KT_EQ"),
           ('&', "KT_AMP"),
           ('|', "KT_PIPE"),
           ('<', "KT_LT"),
           ('>', "KT_GT"),
           ('!', "KT_EXCL"),
           ('.', "KT_DOT"),
           (':', "KT_COLON"),
           ('\'', "KT_PRIME"),
           ('^', "KT_HAT"),
           ('#', "KT_POUND")]

codegen :: Expr -> IO Source
codegen expr = do
  let r' = filter (not . (=='\n')) (Ch.unpack runtime)
  return (r' ++ emit expr ++ "main();")

emit :: Expr -> Source

emit PVoid = ""
emit (PInteger val) = show val
emit (PFloat val) = show val
emit (PChar val) = '\'' : val : "'"
emit (PBool val) = if val then "true" else "false"


emit (PIdentifier ide) =
  let ide' = if ide `elem` reserved
               then kitePrefix ++ ide
               else ide
  in concatMap replace ide'
  where replace c = fromMaybe [c] (lookup c opNames)

-- special case for list of chars, emit as js string
emit (PList elems@(PChar _ : _)) =
  let str = foldl (\acc (PChar x) -> acc ++ [x]) [] elems
    in printf "\"%s\"" str

emit (PList elems) = printf "[%s]" (emitAll "," elems)

emit (PIf cond conseq alt) =
  printf "KT_IF(function() { return %s; })(function() { return %s; })(function() { return %s; })" (emit cond) (emit conseq) (emit alt)

emit (PFunc (PFuncType param _) body) =
  let PTypeArg _ ide = param
  in printf "(function(%s) {%s})" (emit ide) (emit body)

emit (PAssign ide expr) =
  printf "%s = %s" (emit ide) (emit expr)

emit (PBlock _ exprs) =
  emitAll ";" exprs

emit (PReturn expr) =
  "return " ++ emit expr

emit (PCall expr arg) =
  printf "%s(%s)" (emit expr) (emit arg)

emitAll :: String -> [Expr] -> Source
emitAll delim = foldl (\acc x -> acc ++ emit x ++ delim) ""
