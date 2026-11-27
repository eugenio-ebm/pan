module Pan.Parser
  ( runParser
  , parse
  , parseExpr
  , parseBinOp
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
parseExpr = parseBinOp

parseBinOp :: Parser Expr
parseBinOp = parseBinOp' 0
  where
    parseBinOp' :: Int -> Parser Expr
    parseBinOp' mp = parseInfix mp

    precPrefix :: Op -> Int
    precPrefix E.Add = 5
    precPrefix E.Sub = 5
    precPrefix _ = -67

    parsePrefix :: Parser Expr
    parsePrefix = do
      op <- parseOp
      let p = precPrefix op
      e <- parseBinOp' p
      return (E.UnOp op e)

    precInfix :: Op -> (Int, Int)
    precInfix E.Add = (1, 2)
    precInfix E.Sub = (1, 2)
    precInfix E.Mul = (3, 4)
    
    parseInfix :: Int -> Parser Expr
    parseInfix mp = do
      t <- lookahead (try parseOp)
      lhs <- case t of
               Nothing -> parseAtom
               Just _ -> parsePrefix
      go lhs
        where
          go lhs = do
            l <- lookahead (try parseOp)
            case l of
              Nothing -> return lhs
              Just op -> do
                let (lp, rp) = precInfix op
                if lp < mp
                  then return lhs
                  else do
                  op' <- parseOp
                  rhs <- parseBinOp' rp
                  go (E.BinOp op' lhs rhs)
  
parseOp :: Parser Op
parseOp = do
  t <- next
  case t of
    T.Symbol T.Plus -> return E.Add
    T.Symbol T.Minus -> return E.Sub
    T.Symbol T.Star -> return E.Mul
    _ -> fail "bad op"

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
