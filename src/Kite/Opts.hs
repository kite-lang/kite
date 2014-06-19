{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Kite.Opts where

import System.Console.CmdArgs
import System.Environment (getArgs, withArgs)

import Kite.Codegen

_NAME    = "kite"
_VERSION = "0.5.0"
_ABOUT   = "Kite compiler"

-- | KiteOpts type construtor
data KiteOpts = KiteOpts {
  input :: String,
  eval :: Bool,
  lexOutput :: Bool,
  parOutput :: Bool,
  debug :: Bool,
  noFoundation :: Bool,
  target :: CodegenTarget,
  desugar :: Bool,
  noEmit :: Bool,
  noOpti :: Bool,
  output :: String,
  noTypeCheck :: Bool
} deriving (Show, Data, Typeable)

-- | Declare kite's opts with name, type and help text
kiteOpts :: KiteOpts
kiteOpts = KiteOpts {
  input        = "" &= typ "file" &= args,
  eval         = False &= help "Evaluate expression",
  lexOutput    = False &= help "Emit lexer output",
  parOutput    = False &= help "Emit parser output",
  debug        = False &= name "d" &= help "Output debug information",
  noFoundation = False &= help "Exclude the Foundation standard library",
  noEmit       = False &= help "Prevent emitting compiled code",
  noOpti       = False &= help "Disable optimizations",
  noTypeCheck  = False &= help "Prevent type checking",
  output       = ""    &= name "o" &= help "Output file name",
  desugar      = False &= help "Emit desugared source",
  target       = JavaScript &= typ "TARGET" &= help "Compilation target"
 }
  &= summary (_NAME ++ " v" ++ _VERSION)
  &= help _ABOUT
  &= helpArg [explicit, name "help", name "h"]
  &= program _NAME

-- | Returns parsed KiteOpts, with help if no args was given
getOpts :: IO KiteOpts
getOpts = do
  args <- getArgs
  (if null args then withArgs ["--help"] else id) $ cmdArgs kiteOpts
