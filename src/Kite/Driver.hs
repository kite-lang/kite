module Kite.Driver (lex, parse, analyze, kite, kited) where

import Prelude hiding (lex)
import Kite.Lexer
import Kite.Parser
import Kite.JSEmit
import Kite.TypeCheck

lex = alexScanTokens
parse = kiteparser
analyze = typeCheck
kite = analyze False . parse . lex

kited = analyze True . parse . lex
kitef file = do
  inp <- readFile file
  case kited inp of
    Right ast -> codegen ((parse . lex) inp) >>= print
    Left err -> print $ "meh " ++ show err
