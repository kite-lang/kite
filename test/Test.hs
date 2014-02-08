module Test where

import Test.HUnit
import Lexer

lexTest desc prog expected = TestCase (assertEqual desc (alexScanTokens prog) expected)

test1 = lexTest "Simple int" "2" [Integer 2]

tests = TestList [TestLabel "Test int" test1]

main = runTestTT tests
