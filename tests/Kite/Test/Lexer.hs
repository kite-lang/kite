module Kite.Test.Lexer (lexerTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Kite.Test.Exception

import Kite.Lexer

lexerTests = testGroup "Lexer"
  [
    testCase "Integer" $
      alexScanTokens "1" @?= [TInteger 1]

  , testCase "Float" $
      alexScanTokens "1.0" @?= [TFloat 1]

  , testCase "String" $
      alexScanTokens "\"swag\"" @?= [TString "swag"]

  , testCase "TSymbol" $
      alexScanTokens "(1)" @?= [TSymbol '(', TInteger 1, TSymbol ')']

  , testCase "TKeyword return" $
      alexScanTokens "return foo" @?= [TKeyword "return", TIdentifier "foo"]

  , testCase "Function" $
      alexScanTokens "(Int) -> Float" @?= [TSymbol '(', TType "Int", TSymbol ')', TOperator "->", TType "Float"]

  , testCase "Comment infix" $
      alexScanTokens "2 {- -}; foo" @?= [TInteger 2, TSymbol ';', TIdentifier "foo"]

  , testCase "Comment block" $
      alexScanTokens "{- \
\test\
\ more test-}" @?= []

  , testCase "Comment suffix" $
      alexScanTokens "1337 -- a comment \n\
                      \ 42" @?= [TInteger 1337, TInteger 42]
  ]
