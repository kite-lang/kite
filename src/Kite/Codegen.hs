{-# LANGUAGE DeriveDataTypeable #-}
module Kite.Codegen where

import System.Console.CmdArgs

data CodegenTarget = JavaScript | LLVM
                   deriving (Show, Data, Typeable)
