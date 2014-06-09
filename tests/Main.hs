module Main where

import Control.Exception
import Test.Tasty
import Test.Tasty.HUnit

import Kite.Test.Exception
-- import LexerTest
-- import ParserTest
-- import TypeCheckTest
-- import InferenceTest


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Kite Tests"
        [
          truthTests
        -- , typeCheckTests
        -- , inferenceTests
        -- , lexerTests
        -- , parserTests
        ]

truthTests = testGroup "Truth"
  [
    testCase "Pass" $
      assertException DivideByZero (evaluate $ 5 `div` 0)
  , testCase "Fail" $
      assertException DivideByZero (evaluate $ 5 `div` 1)
  ]
