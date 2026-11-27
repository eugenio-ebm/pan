module Pan.TestParser
  ( testParser
  ) where

import Pan.Parser
  ( runParser
  , parsePratt
  )

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Pan.Token
  ( Token
  )
import qualified Pan.Token as T

import Pan.Expr
  ( Op
  , Expr
  )
import qualified Pan.Expr as E

testParser :: TestTree
testParser = testGroup "Parser"
  [ testGroup "parsePratt"
    [ parsePratt_unitTests
    ]
  ]

parsePratt_unitTests :: TestTree
parsePratt_unitTests = testGroup "Unit Tests" tests
  where
    baseCases =
      [ ( [ T.Int 1
          , T.Symbol T.Plus
          , T.Int 2
          ]
        , E.BinOp E.Add (E.Int 1) (E.Int 2)
        , []
        )
      , ( [ T.Int 1
          , T.Symbol T.Plus
          , T.Int 2
          , T.Symbol T.Minus
          , T.Int 3
          ]
        , E.BinOp E.Sub (E.BinOp E.Add (E.Int 1) (E.Int 2)) (E.Int 3)
        , []
        )
      , ( [ T.Int 1
          , T.Symbol T.Plus
          , T.Int 2
          , T.Symbol T.Star
          , T.Int 3
          ]
        , E.BinOp E.Add (E.Int 1) (E.BinOp E.Mul (E.Int 2) (E.Int 3))
        , []
        )
      ]

    edgeCases =
      [
      ]

    f (ts, e, msgs) =
      testCase (T.ppTokens ts) $ runParser ts (parsePratt 0) @?= (e, msgs)

    tests =
      [ testGroup "Base Cases" (map f baseCases)
      , testGroup "Edge Cases" (map f edgeCases)
      ]
