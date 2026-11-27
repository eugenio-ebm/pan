module Pan.TestScanner
  ( testScanner
  ) where

import Pan.Scanner
  ( scan
  )

import Test.Tasty
import Test.Tasty.HUnit

import Pan.Token
  ( Keyword (..)
  , Symbol (..)
  , Token (..)
  )

testScanner :: TestTree
testScanner = testGroup "Scanner"
  [
    scan_unitTests
  ]

scan_unitTests :: TestTree
scan_unitTests = testGroup "Unit Tests" tests
  where
    baseCases =
      [ ( "1 = 1"
        , [ Int 1
          , Symbol Eq
          , Int 1
          ]
        )
      , ( "let x = 3 in x"
        , [ Keyword Let
          , Ident "x"
          , Symbol Eq
          , Int 3
          , Keyword In
          , Ident "x"
          ]
        )
      , ( "(f x) (g x)"
        , [ Symbol LP
          , Ident "f"
          , Ident "x"
          , Symbol RP
          , Symbol LP
          , Ident "g"
          , Ident "x"
          , Symbol RP
          ]
        )
      ]

    edgeCases =
      [ ( "let"
        , [ Keyword Let
          ]
        )
      , ( "letter"
        , [ Ident "letter"
          ]
        )
      ]

    f = \(s, ts) -> testCase s $ scan s @?= ts

    tests =
      [ testGroup "Base Cases" (map f baseCases)
      , testGroup "Edge Cases" (map f edgeCases)
      ] 
