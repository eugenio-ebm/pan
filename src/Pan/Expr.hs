module Pan.Expr (Expr(..), Op(..), Pat(..)) where

data Expr
  = Let String Expr Expr
  | Letrec String Expr Expr
  | Lam String Expr
  | Case Expr [(Pat, Expr)]
  | App Expr Expr
  | BinOp Op Expr Expr
  | Cons String [Expr]
  | Var String
  | Int Int
  deriving (Eq, Show)

data Op = Add | Sub deriving (Eq, Show)

data Pat
  = PCons String [Pat]
  | PVar String
  | PInt Int
  deriving (Eq, Show)
