module TypeCheckTest (typeCheckTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Kite.Lexer
import Kite.Parser
import Kite.TypeCheck

data TypeCheckError = TypeE | RefE | ArE | UnE deriving(Show, Eq)

analyze prog = case (typeCheck . kiteparser . alexScanTokens) prog of
  Right _ -> Nothing
  Left (TypeError _) -> Just TypeE
  Left (ReferenceError _) -> Just RefE
  Left (ArityError _) -> Just ArE
  Left UnknownError -> Just UnE

-- test expression
testE name ex prog = testCase name $ ex @?= analyze prog

typeCheckTests = testGroup "Type Check"
  [ testE "Assignment"
    Nothing "one = 2"

  , testE "Illegal assignment"
    (Just TypeE) "{ one = 2; one = \"the\"; }"

  , testE "Reference"
    Nothing "{ one = 1; two = 1 + one; }"

  , testE "Reference not defined"
    (Just RefE) "two = 1 + one"

  , testE "Reference itself not defined"
    (Just RefE) "two = 1 + two"

  , testE "Function call"
    Nothing "{ one = (Int a) -> Int { return a }; one(1); }"

  , testE "Function call with arg of wrong type"
    (Just TypeE) "{ one = (Int a) -> Int { return a }; one(\"1\"); }"

  , testE "Function call with multiple parameters"
    Nothing "{ foo = (Int a, Int b) -> Int { return 1 }; foo(1, 2) }"

  , testE "Function call with multiple parameters and wrong arity"
    (Just ArE) "{ foo = (Int a, Int b) -> Int { return 1 }; foo(1) }"

  , testE "Function call with wrong number of args"
    (Just ArE) "{ one = () -> Int { return 1 }; one(1); }"

  , testE "Function call to undefined function"
    (Just RefE) "{ foo(2) }"

  , testE "Recursive function"
    Nothing "{ fib = (Int n) -> Int { return if n == 0 then 0 else if n == 1 then 1 else fib (n - 1) + fib (n - 2)}; fib(5); }"

  , testE "Recursive function wrong return type"
    (Just TypeE) "{ fib = (Int n) -> Bool { return if n == 0 then 0 else if n == 1 then 1 else fib (n - 1) + fib (n - 2)}; fib(5); }"

  , testE "List assignment same type"
    Nothing "{ list = [1, 2, 3] }"

  , testE "List assignment illegal values"
    (Just TypeE) "{ list = [1, True, \"Three\"] }"

  , testE "Append to list"
    Nothing "{ list = [1, 2] + 3 }"

  , testE "Append to list with wrong type"
    (Just TypeE) "{ list = [1, 2] + 3.0 }"

  , testE "Concatenate string"
    Nothing "{ s = \"str\" + \"ing\" }"

  , testE "Concatenate string with numbers in sting"
    Nothing "{ s = \"2\" + \"2.0\" }"

  , testE "Concatenate string with numbers"
    (Just TypeE) "{ s = \"2\" + 2.0 }"

  , testE "Check Bool operators"
    Nothing "{ foo = () -> Bool { return 2 > 1 }; }"

  , testE "Index of list"
    Nothing "{ foo = () -> Bool{ list = [True, False] return list # 1}; }"

  , testE "Index of list illegal argument"
    (Just TypeE) "{ foo = [1, 2, 3] # \"one\"}"

  , testE "Illegal arithmic operator"
    (Just TypeE) "{ list = [1, 2, 3] / [4, 5, 6]; s = \"test\" / \"string\"; a = 1 + \"two\" }"
  ]
