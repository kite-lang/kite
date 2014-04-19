module Kite.Driver (lex, parse, analyze, kite) where

import Prelude hiding (lex)
import Kite.Lexer
import Kite.Parser
import Kite.JSEmit
import Kite.TypeCheck
import Kite.Preprocessor

lex = alexScanTokens
parse = kiteparser
analyze = typeCheck
process = preprocess

kite file = do
  p <- process file
  print p
  case (analyze False . parse . lex) p of
     Right ast -> codegen ((parse . lex) p)
     Left err -> return $ "meh " ++ show err

-- kitef file = do
--   p <- process file
--   case kited file of
--     Right ast -> codegen ((parse . lex) p) >>= print
--     Left err -> print $ "meh " ++ show err
