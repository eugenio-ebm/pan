module Pan.Val
  ( Val (..)
  , ppVal
  ) where

data Val
  = Int Integer
  deriving (Eq, Show)

ppVal :: Val -> String
ppVal (Int v) = show v
