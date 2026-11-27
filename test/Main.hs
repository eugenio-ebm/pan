module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit

import Pan.TestScanner
import Pan.TestParser

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Pan"
  [ testScanner
  , testParser
  ]
