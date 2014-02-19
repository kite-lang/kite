module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Lexer

main :: IO ()
main = defaultMain lexerTests

lexerTests = testGroup "Lexer tests"
  [ testCase "Integer" $
      alexScanTokens "1" @?= [Integer 1]

  , testCase "Float" $
      alexScanTokens "1.0" @?= [Float 1]

  , testCase "Symbol" $
      alexScanTokens "(1)" @?= [Symbol '(', Integer 1, Symbol ')']

  , testCase "Keyword return" $
      alexScanTokens "return foo" @?= [Keyword "return", Identifier "foo"]

  , testCase "Function" $
      alexScanTokens "(Int) -> Float" @?= [Symbol '(', Type "Int", Symbol ')', Operator "->", Type "Float"]

  , testCase "String" $
      alexScanTokens "\"swag\"" @?= [String "swag"]

  , testCase "Comment infix" $
      alexScanTokens "2 {- -}; foo" @?= [Integer 2, Symbol ';', Identifier "foo"]

  , testCase "Comment Multi-line" $
      alexScanTokens "{- \
\test\
\ more test-}" @?= []

  , testCase "Comment suffix" $
      alexScanTokens "1337 -- a comment \n\
                      \ 42" @?= [Integer 1337, Integer 42]
  ]
