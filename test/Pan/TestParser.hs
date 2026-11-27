module Pan.TestParser
  ( testParser
  ) where

import Pan.Parser
  ( runParser
  , parseOp
  , parseAtom
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
  [ testGroup "parseOp"
    [ parseOp_unitTests
    ]
  , testGroup "parseAtom"
    [ parseAtom_unitTests
    ]
  ]

parseOp_unitTests :: TestTree
parseOp_unitTests = testGroup "Unit Tests" tests
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
      , ( [ T.Symbol T.LP
          , T.Int 1
          , T.Symbol T.Plus
          , T.Int 2
          , T.Symbol T.RP
          , T.Symbol T.Star
          , T.Int 3
          ]
        , E.BinOp E.Mul (E.BinOp E.Add (E.Int 1) (E.Int 2)) (E.Int 3)
        , []
        )
      ]
      
    edgeCases =
      [ ( [ T.Int 1
          , T.Symbol T.Plus
          , T.Symbol T.Minus
          , T.Int 2
          ]
        , E.BinOp E.Add (E.Int 1) (E.UnOp E.Sub (E.Int 2))
        , []
        )
      , ( [ T.Symbol T.Minus
          , T.Symbol T.Minus
          , T.Int 42
          ]
        , E.UnOp E.Sub (E.UnOp E.Sub (E.Int 42))
        , []
        )
      ]

    f' (ts, e, msgs) =
      testCase (T.ppTokens ts) $ runParser parseOp ts @?= Just (e, msgs)

    tests =
      [ testGroup "Base Cases" (map f' baseCases)
      , testGroup "Edge Cases" (map f' edgeCases)
      ]

parseAtom_unitTests = testGroup "Unit Tests" tests
  where
    baseCases =
      [ ( [ T.Ident "f"
          ]
        , E.Var "f"
        , []
        )
      , ( [ T.Int 42
          ]
        , E.Int 42
        , []
        )
      , ( [ T.Symbol T.LP
          , T.Ident "f"
          , T.Symbol T.RP
          ]
        , E.Var "f"
        , []
        )
      ]
      
    edgeCases =
      [ ( [ T.Symbol T.LP
          , T.Ident "f"
          ]
        , E.Var "f"
        , [ "Missing ')'" ]
        )
      , ( [ T.Ident "f"
          , T.Symbol T.RP
          ]
        , E.Var "f"
        , []
        )
      , ( [ T.Symbol T.LP
          , T.Symbol T.LP
          , T.Ident "f"
          ]
        , E.Var "f"
        , [ "Missing ')'"
          , "Missing ')'"
          ]
        )
      ]

    f (ts, e, msgs) =
      testCase (T.ppTokens ts) $ runParser parseAtom ts @?= Just (e, msgs)

    tests =
      [ testGroup "Base Cases" (map f baseCases)
      , testGroup "Edge Cases" (map f edgeCases)
      ]
