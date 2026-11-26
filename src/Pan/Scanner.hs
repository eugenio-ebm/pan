module Pan.Scanner (scan) where

import Data.Char (isSpace, isAlpha, isAlphaNum, isNumber, isUpper)

import Pan.Token (Token(..))

scan :: [Char] -> [Token]
scan [] = []
scan (s : cs) | isSpace s = scan cs
scan ('=' : cs) = Eq : scan cs
scan ('\\' : cs) = Backslash : scan cs
scan ('.' : cs) = Dot : scan cs
scan ('|' : cs) = Bar : scan cs
scan ('-' : '>' : cs) = Arr : scan cs
scan ('+' : cs) = Plus : scan cs
scan ('-' : cs) = Minus : scan cs
scan ('(' : cs) = LP : scan cs
scan (',' : cs) = Comma : scan cs
scan (')' : cs) = RP : scan cs
scan (x : cs) | isAlpha x = t : scan cs'
  where
    (xs, cs') = span isAlphaNum cs
    t = case x : xs of
          "let" -> Let
          "letrec" -> Letrec
          "in" -> In
          "case" -> Case
          s | isUpper x -> TypeId s
          s -> Id s
scan (d : cs) | isNumber d = Int v : scan cs'
  where
    (ds, cs') = span isNumber cs
    v = read (d : ds)
scan (t : _) = error $ "Unknown char '" ++ [t] ++ "'"
