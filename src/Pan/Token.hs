module Pan.Token
  ( Keyword (..)
  , Symbol (..)
  , Token (..)
  , ppToken
  , ppTokens
  ) where

data Keyword
  = Let
  | In
  deriving (Eq, Show)

data Symbol
  = Eq
  | Plus
  | Minus
  | Star
  | LP
  | RP
  deriving (Eq, Show)

data Token
  = Keyword Keyword
  | Symbol Symbol
  | Ident String
  | Int Integer
  | EOF
  deriving (Eq, Show)

ppToken :: Token -> String
ppToken (Keyword Let) = "let"
ppToken (Keyword In) = "in"
ppToken (Symbol Eq) = "="
ppToken (Symbol Plus) = "+"
ppToken (Symbol Minus) = "-"
ppToken (Symbol Star) = "*"
ppToken (Symbol LP) = "("
ppToken (Symbol RP) = ")"
ppToken (Ident x) = x
ppToken (Int v) = show v
ppToken EOF = ""

ppTokens :: [Token] -> String
ppTokens = unwords . map ppToken
