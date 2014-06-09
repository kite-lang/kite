module Kite.Test.TypeCheck (typeCheckTests) where

import Prelude hiding (lex)

import Test.Tasty
import Test.Tasty.HUnit

import Kite.Driver
import Kite.Environment

data TypeCheckError = TypeE | RefE | ArE | UnE deriving (Show, Eq)

run prog = case (analyze False . parse . lex) prog of
  Right _ -> Nothing
  Left (TypeError _) -> Just TypeE
  Left (ReferenceError _) -> Just RefE
  Left (ArityError _) -> Just ArE
  Left UnknownError -> Just UnE

-- test expression
testE name ex prog = testCase name $ run prog @?= ex

typeCheckTests =
  testGroup "Inference test"
  [ testGroup "Function application"
    [ testE "Simple function application"
      Nothing "id = |x| -> { return x}\
              \one = id(1)"

     , testE "Immediate function application"
      Nothing "one = |x| -> { return x}(1)"

     , testE "Immediate application of non-function"
      (Just TypeE) "foo = \"foo\"(1)"

     , testE "Application of returned function (HoF)"
      Nothing "id = |x| -> { return x}\
              \one = id(|x| -> { return x})(1)"

     , testE "Multiple nested applications of returned functions (HoF)"
      Nothing "id = |x| -> { return x }\
              \foo = id(|x| -> { \
              \    return |y| -> {\
              \       return x + y\
              \    }\
              \})(1)(1)"
    ]

  , testGroup "Type Check"
    [ testE "Assignment"
      Nothing "one = 2"

    , testE "Reassignment"
      (Just TypeE) "one = 2; one = 1;"

    , testE "Reassignment (top level)"
      (Just TypeE) "main = -> { one = 2; one = 1; }"

    , testE "Illegal reassignment"
      (Just TypeE) "one = 2; one = \"the\";"

    , testE "Illegal reassignment (top level)"
      (Just TypeE) "main = -> { one = 2; one = \"the\"; }"

    , testE "Reference"
      Nothing "main = -> { one = 1; two = 1 + one; }"

    , testE "Reference not defined"
      (Just RefE) "two = 1 + one"

    , testE "Reference itself not defined"
      (Just RefE) "two = 1 + two"

    , testE "Function call"
      Nothing "one = |a| -> { return a }; main = -> { one(1) }"

    , testE "Function call with arg of wrong type"
      (Just TypeE) "one = |a| -> { return 1+a }; main = -> { one(\"1\") }"

    , testE "Function call with multiple parameters"
      Nothing "foo = |a, b| -> { return 1 }; main = -> { foo(1, 2) }"

    , testE "Function call with wrong number of args"
      (Just TypeE) "foo = |a| -> { return a }; main = -> { foo(1, 2) }"

    , testE "Function call to undefined function"
      (Just RefE) "main = -> { foo(2) }"

    , testE "Recursive function"
      Nothing "fib = |n| -> { return if n == 0 then 0 else if n == 1 then 1 else fib (n - 1) + fib (n - 2)}; main = -> { fib(5) }"

    , testE "List assignment same type"
      Nothing "list = [1, 2, 3]"

    , testE "List assignment illegal values"
      (Just TypeE) "list = [1, True, \"Three\"]"

    , testE "Varying types in list"
      (Just TypeE) "list = [1, 2.0, 3]"

    , testE "Append to list"
      Nothing "list = [1, 2] + [3]"

    , testE "Append to list with wrong type"
      (Just TypeE) "list = [1, 2] + 3.0"

    , testE "Concatenate string"
      Nothing "s = \"str\" + \"ing\""

    , testE "Concatenate string with numbers in sting"
      Nothing "s = \"2\" + \"2.0\""

    , testE "Concatenate string with numbers"
      (Just TypeE) "s = \"2\" + 2.0"

    , testE "Illegal arithmic operator"
      (Just TypeE) "list = [1, 2, 3] / [4, 5, 6]; s = \"test\" / \"string\"; a = 1 + \"two\""
    ]
  ]
