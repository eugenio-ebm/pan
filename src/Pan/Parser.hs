module Pan.Parser
  ( runParser
  , parsePratt
  ) where

import Control.Monad.RWS

import Pan.Token
  ( Token
  )
import qualified Pan.Token as T

import Pan.Expr
  ( Op
  , Expr
  )
import qualified Pan.Expr as E


type Parser = RWS () [String] [Token]

runParser :: [Token] -> Parser a -> (a, [String])
runParser ts p = evalRWS p () ts

peek :: Parser Token
peek = do
  ts <- get
  case ts of
    [] -> return T.EOF
    (t : _) -> return t

next :: Parser Token
next = do
  ts <- get
  case ts of
    [] -> return T.EOF
    (t : ts') -> do
      put ts'
      return t

parsePratt :: Int -> Parser Expr
parsePratt mp = do
  lhs <- parseAtom
  go lhs
  where
    isOp :: Token -> Bool
    isOp (T.Symbol T.Plus) = True
    isOp (T.Symbol T.Minus) = True
    isOp (T.Symbol T.Star) = True
    isOp _ = False

    getPrec :: Token -> (Int, Int)
    getPrec (T.Symbol T.Plus) = (1, 2)
    getPrec (T.Symbol T.Minus) = (1, 2)
    getPrec (T.Symbol T.Star) = (3, 4)
    getPrec t = error $ "bad op '" ++ T.ppToken t ++ "'"

    cons :: Token -> Expr -> Expr -> Expr
    cons (T.Symbol T.Plus) = E.BinOp E.Add
    cons (T.Symbol T.Minus) = E.BinOp E.Sub
    cons (T.Symbol T.Star) = E.BinOp E.Mul
    cons t = error $ "bad op '" ++ T.ppToken t ++ "'"

    go :: Expr -> Parser Expr
    go lhs = do
      op <- peek
      if isOp op
        then
        do
          let (lp, rp) = getPrec op
          if lp < mp
            then
            do
              return lhs
            else
            do
              _ <- next
              rhs <- parsePratt rp
              go (cons op lhs rhs)
        else
        do
          return lhs

parseAtom :: Parser Expr
parseAtom = do
  t <- peek
  case t of
    T.Ident x -> next >> return (E.Var x)
    T.Int v -> next >> return (E.Int v)
    _ -> error "Parse error"
