module Pan.TestEval
  ( testEval
  ) where

import Pan.Eval
  ( eval
  )

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Pan.Expr
  ( Op (..)
  , Expr (..)
  , ppExpr
  )

import Pan.Val
  ( Val
  )
import qualified Pan.Val as V

testEval :: TestTree
testEval = testGroup "Eval"
  [ testGroup "eval"
    [ eval_unitTests
    , eval_properties
    ]
  ]

eval_unitTests :: TestTree
eval_unitTests = testGroup "Unit Tests" tests
  where
    baseCases =
      [ ( BinOp Add (Int 6) (Int 7)
        , V.Int 13
        )
      , ( UnOp Sub $ BinOp Add (Int 6) (Int 7)
        , V.Int (- 13)
        )
      ]

    edgeCases =
      [
      ]

    f (e, v) = testCase (ppExpr e) $ eval e @?= v

    tests =
      [ testGroup "Base Cases" (map f baseCases)
      , testGroup "Edge Cases" (map f edgeCases)
      ] 

eval_properties :: TestTree
eval_properties = testGroup "Properties"
  [ testProperty "add two integers" $
    \x y -> eval (BinOp Add (Int x) (Int y)) === V.Int (x + y)
  ]
