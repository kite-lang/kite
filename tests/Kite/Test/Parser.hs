module Kite.Test.Parser (parserTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Kite.Lexer
import Kite.Parser
import Kite.Syntax



parserTests = testGroup "Parser"
  [testCase "Integer" $
    kiteparser (alexScanTokens "main = ->{3}") @?=  [PDecl "main" (PLambda (PLambdaType (PTypeArg PVoidType (PIdentifier "Void")) (PTypeVar "sugarType")) (PBlock [PReturn (PInteger 3)]))]

  , testCase "Float" $
    kiteparser (alexScanTokens "main = ->{21.1}") @?=  [PDecl "main" (PLambda (PLambdaType (PTypeArg PVoidType (PIdentifier "Void")) (PTypeVar "sugarType")) (PBlock [PReturn (PFloat 21.1)]))]
  -- , testCase "Float" $
  --     kiteparser [Float 21.1] @?= PTerm (PFloat 21.1)

  -- , testCase "String" $
  --     kiteparser [String "yolo"] @?= PTerm (PString "yolo")

  -- , testCase "Symbol" $
  --     kiteparser [Symbol '(', Float 21.1, Symbol ')'] @?= PGroup (PTerm (PFloat 21.1))

  -- , testCase "Keyword return" $
  --     kiteparser [Keyword "return", Identifier "moby"] @?= PReturn (PTerm (PIdentifier "moby"))

  -- , testCase "Function" $
  --     kiteparser [
  --       Symbol '(', Type "Int", Symbol ')',
  --       Operator "->",
  --       Type "Float", Identifier "foo",
  --       Operator "=",
  --       Symbol '(', Type "Int", Identifier "a", Symbol ')',
  --       Operator "->",
  --       Type "Float",
  --       Symbol '{', Integer 2, Symbol ';', Symbol '}'
  --       ] @?=
  --     PBind
  --     (PLambdaType [PPrimType "Int"] (PPrimType "Float")) (PIdentifier "foo")
  --     (PLambda
  --      (PLambdaType
  --       [PTypeArg (PPrimType "Int") (PIdentifier "a")]
  --       (PPrimType "Float"))
  --      (PBlock [PTerm (PInteger 2)]))
  ]
