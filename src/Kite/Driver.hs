{-# LANGUAGE NoMonomorphismRestriction #-}
module Kite.Driver (runKite) where

import Prelude hiding (lex)

import Text.Show.Pretty

import Kite.Lexer
import Kite.Parser
import Kite.TypeCheck
import Kite.Preprocessor
import Control.Monad
import qualified Kite.JSEmit as Kjs

lex = alexScanTokens
parse = kiteparser
analyze = typeCheck
process = preprocess

-- ev: eval, db: debug, js: emit js, lx: lex output, pr: parser output
runKite db js lx pr source = do
  p <- process source

  let tokens = lex p
  when lx (prettyPrint tokens)

  let ast = parse tokens
  when pr (prettyPrint ast)

  let analysis = analyze db ast
  case analysis of
    Right _ -> case js of
      True -> Kjs.codegen ast >>= putStrLn
      False -> putStrLn "No emitter selected. Use kite --help to view available emitters."
    Left err -> putStrLn ("Error: " ++ show err)

  where prettyPrint = putStrLn . ppShow
