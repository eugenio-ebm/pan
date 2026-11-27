module Pan.Expr
  ( Op (..)
  , Expr (..)
  ) where

data Op
  = Add
  | Sub
  | Mul
  deriving (Eq, Show)

data Expr
  = Let String Expr Expr
  | App Expr Expr
  | BinOp Op Expr Expr
  | UnOp Op Expr
  | Var String
  | Int Integer
  deriving (Eq, Show)

