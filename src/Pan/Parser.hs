module Pan.Parser
  ( runParser
  , parse
  , parseExpr
  , parseOp
  , parseAtom
  ) where

import Control.Applicative
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

parse :: [Token] -> Expr
parse ts =
  case runParser parseExpr ts of
    Just (expr, []) -> expr
    Just (_, msgs) -> error $ unlines msgs
    Nothing -> error "Parse error"

type Parser = RWST () [String] [Token] Maybe

runParser :: Parser a -> [Token] -> Maybe (a, [String])
runParser p ts = evalRWST p () ts

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

warn :: String -> Parser ()
warn = tell . pure

lookahead :: Parser a -> Parser a
lookahead p = do
  s <- get
  x <- p
  put s
  return x

try :: Parser a -> Parser (Maybe a)
try p = (p >>= return . Just) <|> return Nothing

parseExpr :: Parser Expr
parseExpr = parseOp

parseOp :: Parser Expr
parseOp = parseOp' 0
  where
    parseOp' :: Int -> Parser Expr
    parseOp' mp = parseInfix mp

    precPrefix :: Op -> Int
    precPrefix E.Add = 5
    precPrefix E.Sub = 5
    precPrefix _ = -67

    parsePrefixOp :: Parser Op
    parsePrefixOp = do
      t <- next
      case t of
        T.Symbol T.Plus -> return E.Add
        T.Symbol T.Minus -> return E.Sub
        _ -> fail "bad op"

    parsePrefix :: Parser Expr
    parsePrefix = do
      op <- parsePrefixOp
      let p = precPrefix op
      e <- parseOp' p
      return (E.UnOp op e)

    precInfix :: Op -> (Int, Int)
    precInfix E.Add = (1, 2)
    precInfix E.Sub = (1, 2)
    precInfix E.Mul = (3, 4)

    parseInfixOp :: Parser Op
    parseInfixOp = do
      t <- next
      case t of
        T.Symbol T.Plus -> return E.Add
        T.Symbol T.Minus -> return E.Sub
        T.Symbol T.Star -> return E.Mul
        _ -> fail "bad op"

    parseInfix :: Int -> Parser Expr
    parseInfix mp = do
      lhs <- parsePrefix <|> parseAtom
      go lhs
        where
          go lhs = do
            l <- lookahead (try parseInfixOp)
            case l of
              Nothing -> return lhs
              Just op -> do
                let (lp, rp) = precInfix op
                if lp < mp
                  then return lhs
                  else do
                  op' <- parseInfixOp
                  rhs <- parseOp' rp
                  go (E.BinOp op' lhs rhs)

parseAtom :: Parser Expr
parseAtom = parseVar <|> parseInt <|> parseParens

parseVar :: Parser Expr
parseVar = do
  T.Ident x <- next
  return (E.Var x)

parseInt :: Parser Expr
parseInt = do
  T.Int v <- next
  return (E.Int v)

parseParens :: Parser Expr
parseParens = do
  T.Symbol T.LP <- next
  e <- parseExpr
  t <- peek
  case t of
    T.Symbol T.RP -> next >> return e
    _ -> do
      warn "Missing ')'"
      return e
