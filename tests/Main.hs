module Main where

import Control.Exception
import Test.Tasty
import Test.Tasty.HUnit

import Kite.Test.Exception
import Kite.Test.Lexer
-- import Kite.Test.Parser
-- import Kite.Test.TypeCheck
-- import Kite.Test.Inference


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Kite Tests"
        [
         -- truthTests
          lexerTests
        -- , typeCheckTests
        -- , inferenceTests
        -- , parserTests
        ]

truthTests = testGroup "Truth"
  [
    testCase "Pass" $
      assertException DivideByZero (evaluate $ 5 `div` 0)
  , testCase "Fail" $
      assertException DivideByZero (evaluate $ 5 `div` 1)
  ]
