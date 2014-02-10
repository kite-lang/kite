import System.Environment
import Lexer

main = getArgs >>= (\prog -> putStrLn (show $ alexScanTokens (prog !! 0)))
