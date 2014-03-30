module TypeCheckTest (typeCheckTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Kite.Lexer
import Kite.Parser
import Kite.TypeCheck

data TypeCheckError = TypeE | RefE | ArE | UnE deriving(Show, Eq)

analyze prog = case (typeCheck False . kiteparser . alexScanTokens) prog of
  Right _ -> Nothing
  Left (TypeError _) -> Just TypeE
  Left (ReferenceError _) -> Just RefE
  Left (ArityError _) -> Just ArE
  Left UnknownError -> Just UnE

-- test expression
testE name ex prog = testCase name $ analyze prog @?= ex

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

    , testE "Illegal reassignment"
      (Just TypeE) "one = 2; one = \"the\";"

    , testE "Reference"
      Nothing "one = 1; two = 1 + one;"

    , testE "No return statements"
      (Just TypeE) "f = |a: Int| -> Int { 2 }"

    , testE "Varying return types"
      (Just TypeE) "f = |a: Int| -> Int { return 2; return 2.0 }"

    , testE "Reference not defined"
      (Just RefE) "two = 1 + one"

    , testE "Reference itself not defined"
      (Just RefE) "two = 1 + two"

    , testE "Function call"
      Nothing "one = |a: Int| -> Int { return a }; one(1)"

    , testE "Function call with arg of wrong type"
      (Just TypeE) "one = |a: Int| -> Int { return a }; one(\"1\")"

    , testE "Function call with multiple parameters"
      Nothing "foo = |a: Int, b: Int| -> Int { return 1 }; foo(1, 2)"

    , testE "Function call with multiple parameters and wrong arity"
      (Just ArE) "foo = |a: Int, b: Int| -> Int { return 1 }; foo(1)"

    , testE "Function call with wrong number of args"
      (Just ArE) "one = || -> Int { return 1 }; one(1)"

    , testE "Function call to undefined function"
      (Just RefE) "foo(2)"

    , testE "Recursive function"
      Nothing "fib = |n: Int| -> Int { return if n == 0 then 0 else if n == 1 then 1 else fib (n - 1) + fib (n - 2)}; fib(5)"

    , testE "Recursive function wrong return type"
      (Just TypeE) "fib = |n: Int| -> Bool { return if n == 0 then 0 else if n == 1 then 1 else fib (n - 1) + fib (n - 2)}; fib(5)"

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

    , testE "Check Bool operators"
      Nothing "foo = || -> Bool { return 2 > 1 }"

    , testE "Index of list"
      Nothing "foo = || -> Bool { list = [True, False] return list # 1}"

    , testE "Index of list illegal argument"
      (Just TypeE) "foo = [1, 2, 3] # \"one\""

    , testE "Illegal arithmic operator"
      (Just TypeE) "list = [1, 2, 3] / [4, 5, 6]; s = \"test\" / \"string\"; a = 1 + \"two\""
    ]
  ]
