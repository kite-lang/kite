module Kite.Test.TypeCheck (typeCheckTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Kite.Lexer
import Kite.Parser
import Kite.TypeCheck

-- test expression
test name a b ex = do
  actual <- unify a b
  return $ testCase name $ actual @?= ex

typeCheckTests =
  testGroup "Unification"
  [  testGroup "Primitives"
     [ test "Bool"
       (PBoolType) (PBoolType)
       (PBoolType)

     , test "Integer"
       (PIntegerType) (PIntegerType)
       (PIntegerType)

     , test "Float"
       (PFloatType) (PFloatType)
       (PFloatType)

     , test "String"
       (PStringType) (PStringType)
       (PStringType)
     ]
  ]
