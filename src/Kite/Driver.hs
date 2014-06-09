{-# LANGUAGE NoMonomorphismRestriction, TemplateHaskell, RecordWildCards #-}
module Kite.Driver (
  runKite
, lex
, parse
, analyze
, preprocess
, preprocessFile
, foundation
) where

import Prelude hiding (lex)

import Data.List
import Control.Monad
import Text.Show.Pretty
import Data.FileEmbed
import qualified Data.ByteString.Char8 as Ch

import Kite.Opts
import Kite.Lexer
import Kite.Parser
import Kite.Syntax
import Kite.TypeCheck
import Kite.Preprocessor
import Kite.Codegen
import Kite.Optimizer
import qualified Kite.CodegenJS as GenJS

lex        = alexScanTokens
parse      = kiteparser
analyze    = typeCheck
foundation = $(embedFile "lib/Foundation.kite")

runKite KiteOpts {..} = do
  p <- if eval then preprocess input else preprocessFile input
  let p' = if noFoundation
           then p
           else Ch.unpack foundation ++ p

  let tokens = lex p'
  let decls = parse tokens
  let optimized = if doOptimize
                  then optimize decls
                  else decls

  when lexOutput (prettyPrint tokens)
  when desugar (putStrLn (prettyDecls decls))
  when parOutput (prettyPrint decls)

  if noTypeCheck
    then unless noEmit $ GenJS.codegen optimized >>= putStrLn
    else do let analysis = typeCheck debug optimized
            case analysis of
              Right _ -> case target of
                JavaScript -> unless noEmit $ GenJS.codegen optimized >>= putStrLn
                LLVM -> putStrLn "Such LLVM"
              Left err -> print err

  where prettyPrint = putStrLn . ppShow
