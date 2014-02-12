import System.Environment
import Lexer
import Parser

main = getArgs >>= \(t:prog:_) -> if t == "l"
                                  then (print . alexScanTokens) prog
                                  else (print . kiteparser . alexScanTokens) prog
