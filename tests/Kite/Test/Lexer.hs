module Kite.Test.Lexer (lexerTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Kite.Test.Exception

import Kite.Lexer

lexerTests = testGroup "Lexer"
  [
    testCase "Integer" $
      alexScanTokens "1" @?= [ TInteger (AlexPn 0 1 1) 1 ]

   , testCase "Float" $
       alexScanTokens "1.0" @?= [ TFloat (AlexPn 0 1 1) 1.0 ]

  , testCase "String" $
      alexScanTokens "\"swag\"" @?= [ TString (AlexPn 0 1 1) "swag" ]

  , testCase "Symbol" $
      alexScanTokens "(1)" @?= [ TSymbol (AlexPn 0 1 1) '('
                               , TInteger (AlexPn 1 1 2) 1
                               , TSymbol (AlexPn 2 1 3) ')' ]

  , testCase "Keyword return" $
      alexScanTokens "return foo" @?= [ TKeyword (AlexPn 0 1 1) "return"
                                      , TIdentifier (AlexPn 7 1 8) "foo" ]

  , testCase "Function" $
      alexScanTokens "(Int) -> Float" @?= [ TSymbol (AlexPn 0 1 1) '('
                                          , TType (AlexPn 1 1 2) "Int"
                                          , TSymbol (AlexPn 4 1 5) ')'
                                          , TOperator (AlexPn 6 1 7) "->"
                                          , TType (AlexPn 9 1 10) "Float" ]

  , testCase "Comment infix" $
      alexScanTokens "2 {- -}; foo" @?= [ TInteger (AlexPn 0 1 1) 2
                                        , TSymbol (AlexPn 7 1 8) ';'
                                        , TIdentifier (AlexPn 9 1 10) "foo"
                                        ]

  , testCase "Comment block" $
      alexScanTokens "{- \
\test\
\ more test-}" @?= []

  , testCase "Comment suffix" $
      alexScanTokens "1337 -- a comment \n\
                      \ 42" @?= [ TInteger (AlexPn 0 1 1) 1337
                                , TInteger (AlexPn 20 2 2) 42 ]
  ]
