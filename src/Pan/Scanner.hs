module Pan.Scanner
  ( scan
  ) where

import Data.Char
  ( isAlpha
  , isAlphaNum
  , isDigit
  , isSpace
  )

import Pan.Token
  ( Keyword (..)
  , Symbol (..)
  , Token (..)
  )

scan :: [Char] -> [Token]
scan [] = []
scan ('=' : cs) = Symbol Eq : scan cs
scan ('-' : c : cs) | isDigit c = scanInt True (c : cs)
scan ('-' : cs) = Symbol Minus : scan cs
scan ('(' : cs) = Symbol LP : scan cs
scan (')' : cs) = Symbol RP : scan cs
scan (c : cs)
  | isSpace c = scan cs
  | isAlpha c = scanIdent (c : cs)
  | isDigit c = scanInt False (c : cs)
  | otherwise = error $ "Unknown char '" ++ [c] ++ "'"

scanIdent :: [Char] -> [Token]
scanIdent s = t : scan cs
  where
    (x, cs) = span isAlphaNum s
    t = case x of
          "let" -> Keyword Let
          "in" -> Keyword In
          _ -> Ident x

scanInt :: Bool -> [Char] -> [Token]
scanInt isNegative s = Int v' : scan cs
  where
    (ds, cs) = span isDigit s
    v = (read ds)
    v' = if isNegative then (-v) else v
