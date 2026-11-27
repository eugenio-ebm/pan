module Pan.Token
  ( Keyword (..)
  , Symbol (..)
  , Token (..)
  ) where

data Keyword
  = Let
  | In
  deriving (Eq, Show)

data Symbol
  = Eq
  | LP
  | RP
  deriving (Eq, Show)

data Token
  = Keyword Keyword
  | Symbol Symbol
  | Ident String
  | Int Integer
  deriving (Eq, Show)
