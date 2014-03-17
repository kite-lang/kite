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
  let ast = (kiteparser . alexScanTokens) inp
  when parOutput $ (putStrLn . ppShow) ast
  case typeCheck ast of
    Right (ty, env) -> print ("Type check passed. FTV: " ++ show (ftv env) ++ " SYM: " ++ show (ftv env))
    Left err -> putStrLn $ "Type error: " ++ show err
