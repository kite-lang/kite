{-# LANGUAGE TemplateHaskell #-}

module Kite.CodegenJS where

import Control.Monad
import Data.Char
import Data.Maybe
import Data.List
import Data.FileEmbed
import qualified Data.ByteString.Char8 as Ch
import Text.Printf

import Kite.Parser
import Kite.Syntax

type Source = String

-- | JavaScript reserved keywords. These are used to avoid name conflicts when emitting identifiers.
reserved = ["while", "for", "arguments", "case", "switch"]

-- | The prefix used as a prefix to resolve name conflicts.
kitePrefix = "KT_"

-- | The JavaScript runtime is embedded in the produced source code.
runtime = $(embedFile "js/kt_runtime.js")

-- | Operator names that are used to emite valid JavaScript identifiers.
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
           ('^', "KT_XOR"),
           ('#', "KT_POUND")]

-- | The main code generator function used in "Kite.Driver".
-- It takes a debugging flag and a list of declarations and emits
-- code for them one by one.
codegen :: Bool -> [Decl] -> IO Source
codegen eval decls = do
  let unlined = filter (not . (=='\n')) (Ch.unpack runtime)
      emitted = map (\decl ->
                      case decl of
                        PDecl ide expr -> printf "var %s = %s" (safeId ide) (emit expr)
                        _ -> fail "Unexpected type decl in CodegenJS"
                    ) decls
      linedDecls = intercalate "\n" emitted
      bin = if eval then "" else "#!/usr/bin/node\n"
  return (bin ++ unlined ++ linedDecls ++ ";main();")

-- | Convert a string to a valid JavaScript identifier
safeId str =
  let safe = if str `elem` reserved
             then kitePrefix ++ str
             else str
  in concatMap replace safe

  where replace c = fromMaybe [c] (lookup c opNames)

-- | The function that emits code for specific AST nodes
emit :: Expr -> Source

emit PVoid = ""
emit (PInteger val) = show val
emit (PFloat val) = show val
emit (PChar val) = '"' : showLitChar val "\""
emit (PBool val) = if val then "true" else "false"

emit (PIdentifier ide) = safeId ide

-- | Emit each expression in a list and wrap it in JavaScript list
-- notation.
emit (PList elems) = printf "[%s]" (emitAll "," elems)

-- | Pairs are emitted as lists with two elements, since JavaScript
-- has no notion of a pair.
emit (PPair a b) = emit (PList [a, b])

-- | If expressions uses the 'KT_if' function from the runtime, and
-- will lazily evalute | either the consequent or alternative by
-- wrapping the expressions in functions
emit (PIf cond conseq alt) =
  printf "KT_if(function() { return %s; })(function() { return %s; })(function() { return %s; })" (emit cond) (emit conseq) (emit alt)

-- | Lambdas are emitted as standard JavaScript functions
emit (PLambda param body) = printf "(function(%s) {%s})" param (emit body)

emit (PBind ide expr) =
  printf "var %s = %s;" (safeId ide) (emit expr)

emit (PBlock exprs) =
  emitAll ";" exprs

emit (PReturn (PBind _ expr)) =
  "return " ++ emit expr
  
emit (PReturn expr) =
  "return " ++ emit expr

-- | Here we optimize fully applied standard arithmetic infix
-- operators by expanding them. This avoids the overhead of function
-- application
emit (PApply (PApply (PIdentifier op) lhs) rhs)
  | op `elem` ["+", "-", "*", "/", "^", "<", "<=", ">", ">="] = printf "(%s %s %s)" (emit lhs) op (emit rhs)
  | op `elem` ["**"] = printf "(Math.pow(%s,%s))" (emit lhs) (emit rhs)

emit (PApply expr arg) =
  printf "%s(%s)" (emit expr) (emit arg)

-- | The match expression is implemented in the runtime and takes an
-- expression and a list of patterns and consequences to match
-- against.
emit (PMatch expr pats) =
  printf "KT_match(%s,%s)" (emit expr) ("[" ++ intercalate "," (map emitPattern pats) ++ "]")

emitPattern (PatPair a b, val) = printf "{ type: 'pair', conseq: function (%s, %s) { return %s } }" a b (emit val)
emitPattern (PatCons hd tl, val) = printf "{ type: 'list', conseq: function (%s, %s) { return %s } }" hd tl (emit val)
emitPattern (PatPrimitive expr, val) = printf "{ type: 'simple', expr:function(){ return %s}, conseq: function () { return %s } }" (emit expr) (emit val)
emitPattern (PatOtherwise, val) = printf "{ type: 'otherwise', conseq: function () { return %s } }" (emit val)

emitAll :: String -> [Expr] -> Source
emitAll delim = intercalate delim . map emit
