module Pan.Token (Token(..)) where

data Token
  = Let
  | Letrec
  | Eq
  | In
  | Backslash
  | Dot
  | Case
  | Bar
  | Arr
  | Plus
  | Minus
  | LP
  | Comma
  | RP
  | TypeId String
  | Id String
  | Int Int
  deriving (Eq, Show)
