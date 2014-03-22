module Main where

import Test.Tasty
import Test.Tasty.HUnit

-- import LexerTest
-- import ParserTest
import TypeCheckTest
import InferenceTest


main :: IO ()
main = defaultMain tests

tests = testGroup "Kite Tests"
        [ typeCheckTests
        , inferenceTests
          --truthTest,
          --lexerTests,
          --parserTests,
        ]

truthTest = testCase "Truth" $
             1+1 @?= 3
