module Test where

import Test.HUnit
import Lexer

tst prog expected = TestCase (alexScanTokens prog @?= expected)

tests = [
  ("1",
   [Integer 1]),
  ("1.0",
   [Float 1.0]),
  ("(1)",
   [Symbol '(', Integer 1, Symbol ')']),
  ("return foo",
   [Keyword "return", Identifier "foo"]),
  ("(Int) -> Float",
   [Symbol '(', Type "Int", Symbol ')', Operator "->", Type "Float"]),
  ("\"swag\"",
   [String "swag"]),

-- Comment tests --

  ("2 {- -}; foo",
   [Integer 2, Symbol ';', Identifier "foo"]),
  ("{- \
\test\
\ more test-}",
   []),
  ("1337 -- a comment \n\
                      \ 42",
   [Integer 1337, Integer 42])

-- Comment tests --

  ]

main = runTestTT $ test $ map (uncurry tst) tests
