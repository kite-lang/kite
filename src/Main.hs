module Main where

import Kite.Opts
import Kite.Driver

main = getOpts >>= runKite
