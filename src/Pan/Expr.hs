module Pan.Expr
  ( Op (..)
  , Expr (..)
  , ppExpr
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

ppExpr :: Expr -> String
ppExpr = go 0
  where
    precInfix :: Op -> (Int, Int)
    precInfix Add = (1, 2)
    precInfix Sub = (1, 2)
    precInfix Mul = (3, 4)

    precPrefix :: Int
    precPrefix = 5

    precApp :: Int
    precApp = 6

    symbol :: Op -> String
    symbol Add = "+"
    symbol Sub = "-"
    symbol Mul = "*"

    paren :: Bool -> String -> String
    paren True s = "(" ++ s ++ ")"
    paren False s = s

    go :: Int -> Expr -> String
    go _ (Var x) = x
    go _ (Int v) = show v

    go p (Let lhs rhs body) =
      paren (p > 0) $
        "let " ++ lhs ++ " = " ++ go 0 rhs ++ " in " ++ go 0 body

    go p (App f arg) =
      paren (p > precApp) $
        go precApp f ++ " " ++ go (precApp + 1) arg

    go p (UnOp op e) =
      paren (p > precPrefix) $
        symbol op ++ go precPrefix e

    go p (BinOp op lhs rhs) =
      paren (lp < p) $
        go lp lhs ++ " " ++ symbol op ++ " " ++ go rp rhs
      where
        (lp, rp) = precInfix op
