module Test where

import Test.HUnit
import Lexer

tst prog expected = TestCase (alexScanTokens prog @?= expected)

tests = [
  ("1",
   [Integer 1]),
  ("1.0",
   [Float 1.0]),
  ("\"foobar\"",
   [String "foobar"]),
  ("(1)",
   [Symbol '(', Integer 1, Symbol ')']),
  ("return foo",
   [Keyword "return", Identifier "foo"]),
  ("(Int) -> Float",
   [Symbol '(', Type "Int", Symbol ')', Operator "->", Type "Float"])]

main = runTestTT $ test $ map (uncurry tst) tests
