{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Main where

import Kite.Lexer
import Kite.Parser
import System.Console.CmdArgs
import Control.Monad

klex = alexScanTokens
kpar = kiteparser

data KiteArgs = KiteArgs {
  input :: String,
  eval :: Bool,
  lexOutput :: Bool,
  parOutput :: Bool
  } deriving (Data, Typeable, Show)

kiteArgs = cmdArgsMode $ KiteArgs {
  input = "" &= argPos 0 &= typ "file",
  eval = False &= help "Evaluate expression",
  lexOutput = False &= help "Emit lexer output",
  parOutput = False &= help "Emit parser output"}
           &= summary "Kite compiler v0.0.1"

main = do
  KiteArgs {..} <- cmdArgsRun kiteArgs
  inp <- if eval then return input else readFile input
  when lexOutput $ (print . alexScanTokens) inp
  when parOutput $ (print . kiteparser . alexScanTokens) inp
