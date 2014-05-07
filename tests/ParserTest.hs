module ParserTest (parserTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Kite.Lexer
import Kite.Parser

parserTests = testGroup "Parser"
  [ testCase "Integer" $
      kiteparser [Integer 3] @?= PTerm (PInteger 3)

  , testCase "Float" $
      kiteparser [Float 21.1] @?= PTerm (PFloat 21.1)

  , testCase "String" $
      kiteparser [String "yolo"] @?= PTerm (PString "yolo")

  , testCase "Symbol" $
      kiteparser [Symbol '(', Float 21.1, Symbol ')'] @?= PGroup (PTerm (PFloat 21.1))

  , testCase "Keyword return" $
      kiteparser [Keyword "return", Identifier "moby"] @?= PReturn (PTerm (PIdentifier "moby"))

  , testCase "Function" $
      kiteparser [
        Symbol '(', Type "Int", Symbol ')',
        Operator "->",
        Type "Float", Identifier "foo",
        Operator "=",
        Symbol '(', Type "Int", Identifier "a", Symbol ')',
        Operator "->",
        Type "Float",
        Symbol '{', Integer 2, Symbol ';', Symbol '}'
        ] @?=
      PBind
      (PLambdaType [PPrimType "Int"] (PPrimType "Float")) (PIdentifier "foo")
      (PLambda
       (PLambdaType
        [PTypeArg (PPrimType "Int") (PIdentifier "a")]
        (PPrimType "Float"))
       (PBlock [PTerm (PInteger 2)]))
  ]
