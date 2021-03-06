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

import System.Exit

import System.Posix.Files
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

-- aliases
lex        = alexScanTokens
parse      = kiteparser
analyze    = typeCheck
foundation = $(embedFile "lib/Foundation.kite")

-- | The main function to run the Kite compiler. It will check which
-- flags are set and act accordingly. After successful compilation an
-- executable file is written to the file system.
runKite KiteOpts {..} = do
  p <- if eval then preprocess input else preprocessFile input
  let p' = if noFoundation
           then p
           else Ch.unpack foundation ++ p

  let tokens = lex p'
  let decls = parse tokens

  when lexOutput (prettyPrint tokens)
  when desugar (putStrLn (prettyDecls decls))
  when parOutput (prettyPrint decls)

  typeCheckPassed <-
    if noTypeCheck
    then return True
    else case typeCheck debug decls of
      Right _ -> return True
      Left (err, stack) -> do
        let stackTrace = concatMap ((++"\n") . ppShow) stack
        putStrLn (show err ++ "\n\nStacktrace:\n" ++ stackTrace)
        exitWith (ExitFailure 1)

  let optimized = if noOpti
                  then decls
                  else optimize decls

  out <- case target of
        JavaScript -> GenJS.codegen eval optimized
        LLVM -> return "Such LLVM"

  let outPath = if null output
                then "main"
                else output

  if eval
    then putStrLn out
    else do
    writeFile outPath out
    setFileMode outPath (unionFileModes stdFileMode ownerExecuteMode)

  where prettyPrint = putStrLn . ppShow
