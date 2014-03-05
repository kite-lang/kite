module TypeCheckTest (typeCheckTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Kite.Lexer
import Kite.Parser
import Kite.TypeCheck

data TypeCheckError = TypeE | RefE | ArE deriving(Show, Eq)

analyze prog = case (typeCheck . kiteparser . alexScanTokens) prog of
  Right _ -> Nothing
  Left (TypeError _) -> Just TypeE
  Left (ReferenceError _) -> Just RefE
  Left (ArityError _) -> Just ArE

typeCheckTests = testGroup "Type Check"
  [ testCase "Assignment" $
    Nothing @?= analyze "one = 2"

  , testCase "Illegal assignment" $
    Just TypeE @?= analyze "{ one = 2; one = \"the\"; }"

  , testCase "Reference" $
    Nothing @?= analyze "{ one = 1; two = 1 + one; }"

  , testCase "Reference not defined" $
    Just RefE @?= analyze "two = 1 + one"

  , testCase "Function call" $
    Nothing @?= analyze "{ one = (Int a) -> Int { return a }; one(1); }"

  , testCase "Function call with arg of wrong type" $
    Just TypeE @?= analyze "{ one = (Int a) -> Int { return a }; one(\"1\"); }"

  , testCase "Function call with wrong number of args" $
    Just ArE @?= analyze "{ one = () -> Int { return 1 }; one(1); }"

  , testCase "Function call to undefined function" $
    Just RefE @?= analyze "{ foo(2) }"

  , testCase "Recursive function" $
    Nothing @?= analyze "{ fib = (Int n) -> Int { return if n == 0 then 0 else if n == 1 then 1 else fib (n - 1) + fib (n - 2)}; fib(5); }"

  ,  testCase "Recursive function wrong return type" $
    Just TypeE @?= analyze "{ fib = (Int n) -> Bool { return if n == 0 then 0 else if n == 1 then 1 else fib (n - 1) + fib (n - 2)}; fib(5); }"

  , testCase "List assignment same type" $
    Nothing @?= analyze "{ list = [1, 2, 3] }"

  , testCase "List assignment illegal values" $
    Just TypeE @?= analyze "{ list = [1, True, \"Three\"] }"

  , testCase "Check Bool operators" $
    Nothing @?= analyze "{ foo = () -> Bool { return 2 > 1 }; }"

  , testCase "Index of list" $
    Nothing @?= analyze "{ foo = () -> Bool{ list = [True, False] return list # 1}; }"

  , testCase "Index of list illegal argument" $
    Just TypeE @?= analyze "{ foo = [1, 2, 3] # \"one\"}"

  , testCase "Illegal arithmic operator" $
    Just TypeE @?= analyze "{ list = [1, 2, 3] / [4, 5, 6]; s = \"test\" / \"string\"; a = 1 + \"two\" }"
  ]
