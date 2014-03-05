module Main where

import Test.Tasty
import Test.Tasty.HUnit

-- import LexerTest
-- import ParserTest
import TypeCheckTest


main :: IO ()
main = defaultMain tests

tests = testGroup "Tests"
        [
          --truthTest,
          --lexerTests,
          --parserTests,
          typeCheckTests
        ]

truthTest = testCase "Truth" $
             1+1 @?= 3
