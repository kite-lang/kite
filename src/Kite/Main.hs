{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Main where

import Kite.Lexer
import Kite.Parser
import Kite.TypeCheck

import System.Console.CmdArgs
import Control.Monad
import Text.Show.Pretty

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
  when lexOutput $ (putStrLn . ppShow . alexScanTokens) inp
  when parOutput $ do
    let ast = (kiteparser . alexScanTokens) inp
    (putStrLn . ppShow) ast
    case typeCheck ast of
      Right _ -> print "Type check passed"
      Left err -> putStrLn $ "Type error: " ++ (show err)
