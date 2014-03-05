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

  , testCase "Illeagal assignment" $
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
  ]
