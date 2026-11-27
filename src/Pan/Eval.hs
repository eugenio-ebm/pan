module Pan.Eval
  ( eval
  ) where

import Control.Monad.Reader

import Pan.Expr
  ( Op (..)
  , Expr (..)
  )

import Pan.Val
  ( Val
  )
import qualified Pan.Val as V


eval :: Expr -> Val
eval expr = runReader (eval' expr) []
  where
    eval' :: Expr -> Reader [(String, Val)] Val
    eval' (BinOp op lhs rhs) = do
      lhs' <- eval' lhs
      rhs' <- eval' rhs
      case (op, lhs', rhs') of
        (Add, V.Int l, V.Int r) -> return $ V.Int (l + r)
        (Sub, V.Int l, V.Int r) -> return $ V.Int (l - r)
        (Mul, V.Int l, V.Int r) -> return $ V.Int (l * r)
    eval' (UnOp op e) = do
      e' <- eval' e
      case (op, e') of
        (Add, V.Int v) -> return (V.Int v)
        (Sub, V.Int v) -> return (V.Int (- v))
    eval' (Int v) = return (V.Int v)
