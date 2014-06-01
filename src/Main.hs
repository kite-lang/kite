{-# LANGUAGE DeriveDataTypeable, RecordWildCards, NoMonomorphismRestriction #-}
module Main where

import Kite.Driver
import Kite.Codegen

import System.Console.CmdArgs
import Control.Monad
import Kite.Preprocessor

data KiteArgs = KiteArgs {
  input :: String,
  eval :: Bool,
  lexOutput :: Bool,
  parOutput :: Bool,
  debugOutput :: Bool,
  noFoundation :: Bool,
  target :: CodegenTarget,
  noEmit :: Bool,
  noTypeCheck :: Bool
  } deriving (Data, Typeable, Show)

kiteArgs = cmdArgsMode $ KiteArgs {
  input = "" &= argPos 0 &= typ "file",
  eval = False &= help "Evaluate expression",
  lexOutput = False &= help "Emit lexer output",
  parOutput = False &= help "Emit parser output",
  debugOutput = False &= help "Output debug information",
  noFoundation = False &= help "Exclude the Foundation standard library",
  noEmit = False &= help "Prevent emitting compiled code",
  noTypeCheck = False &= help "Prevent type checking",
  target = JavaScript &= help "Compilation target"}
           &= summary "Kite compiler v0.0.1"

main = do
  KiteArgs {..} <- cmdArgsRun kiteArgs

  runKite noFoundation noEmit noTypeCheck eval debugOutput target lexOutput parOutput input
