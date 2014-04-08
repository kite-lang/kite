module Kite.Driver (lex, parse, analyze, kite, kited) where

import Prelude hiding (lex)
import Kite.Lexer
import Kite.Parser
import Kite.TypeCheck

lex = alexScanTokens
parse = kiteparser
analyze = typeCheck
kite = analyze False . parse . lex

kited = analyze True . parse . lex
kitef file = do
  inp <- readFile file
  --print $ (parse . lex) inp
  return $ kite inp
