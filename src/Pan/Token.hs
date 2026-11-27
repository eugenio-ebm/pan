module Pan.Token
  ( Keyword (..)
  , Symbol (..)
  , Token (..)
  , ppToken
  ) where

data Keyword
  = Let
  | In
  deriving (Eq, Show)

data Symbol
  = Eq
  | Minus
  | LP
  | RP
  deriving (Eq, Show)

data Token
  = Keyword Keyword
  | Symbol Symbol
  | Ident String
  | Int Integer
  deriving (Eq, Show)

ppToken :: Token -> String
ppToken (Keyword Let) = "let"
ppToken (Keyword In) = "in"
ppToken (Symbol Eq) = "="
ppToken (Symbol Minus) = "-"
ppToken (Symbol LP) = "("
ppToken (Symbol RP) = ")"
ppToken (Ident x) = x
ppToken (Int v) = show v
