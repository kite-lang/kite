{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
import Lexer
import Parser
import System.Console.CmdArgs
import Control.Monad

data KiteArgs = KiteArgs {
  input :: String,
  eval :: Bool,
  lex :: Bool,
  par :: Bool
  } deriving (Data, Typeable, Show)

kiteArgs = cmdArgsMode $ KiteArgs {
  input = "" &= argPos 0 &= typ "file",
  eval = False &= help "Evaluate expression",
  lex = False &= help "Emit lexer output",
  par = False &= help "Emit parser output"}
           &= summary "Kite compiler v0.0.1"

main = do
  KiteArgs {..} <- cmdArgsRun kiteArgs
  inp <- if eval then return input else readFile input
  when lex $ (print . alexScanTokens) inp
  when par $ (print . kiteparser . alexScanTokens) inp
