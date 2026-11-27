module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit

import Pan.TestScanner
import Pan.TestParser
import Pan.TestEval

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Pan"
  [ testScanner
  , testParser
  , testEval
  ]
