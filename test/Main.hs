module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit

import Pan.TestScanner

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Pan"
  [ testScanner
  ]
