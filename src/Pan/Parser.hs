module Pan.Parser (parse) where

import Control.Applicative ((<|>))

import qualified Pan.Token as T
import Pan.Token (Token)

import Pan.Expr (Expr(..), Op(..), Pat(..))

-- TODO: Deal with parse errors such as:
--     \x 12
--     let in 12
-- We know that after seeing 'let', we can cut backtracking, so we should just
-- throw a parse error if the next token is not an identifier.

type Parser a = [Token] -> Maybe (a, [Token])

parse :: [Token] -> Expr
parse ts = case parseExpr ts of
             Just (expr, []) -> expr
             Just (_, ts') -> error $ "Trailing tokens " ++ show ts'
             Nothing -> error "Parse error"

parseToken :: Token -> Parser Token
parseToken t (t' : ts)
  | t' == t = return (t', ts)
  | otherwise = Nothing

parseExpr :: Parser Expr
parseExpr ts = parseLet ts
               <|> parseLetrec ts
               <|> parseLam ts
               <|> parseCase ts
               <|> parseApp ts
               <|> parseBinOp ts
               <|> parseAtom ts

parseLet :: Parser Expr
parseLet (T.Let : T.Id lhs : T.Eq : ts) = do
  (rhs, ts') <- parseExpr ts
  (_, ts'') <- parseToken T.In ts'
  (body, ts''') <- parseExpr ts''
  return (Let lhs rhs body, ts''')
parseLet _ = Nothing

parseLetrec :: Parser Expr
parseLetrec (T.Letrec : T.Id lhs : T.Eq : ts) = do
  (rhs, ts') <- parseExpr ts
  case ts' of
    (T.In : ts'') -> do
      (body, ts''') <- parseExpr ts''
      return (Letrec lhs rhs body, ts''')
    _ -> Nothing -- Missing 'in'
parseLetrec _ = Nothing

parseLam :: Parser Expr
parseLam (T.Backslash : T.Id param : T.Dot : ts) = do
  (body, ts') <- parseExpr ts
  return (Lam param body, ts')
parseLam _ = Nothing

parseCase :: Parser Expr
parseCase (T.Case : ts) = do
  (e, ts') <- parseExpr ts
  (pats, ts'') <- parseClauses ts'
  return (Case e pats, ts'')
  where
    parseClauses :: Parser [(Pat, Expr)]
    parseClauses (T.Dot : ts') = Just ([], ts')
    parseClauses ts' = do
      (c, ts'') <- parseClause ts'
      (cs, ts''') <- parseClauses ts''
      return (c : cs, ts''')

    parseClause :: Parser (Pat, Expr)
    parseClause (T.Bar : ts') = do
      (pat, ts'') <- parsePat ts'
      case ts'' of
        (T.Arr : ts''') -> do
          (expr, ts'''') <- parseExpr ts'''
          return ((pat, expr), ts'''')
        _ -> Nothing
    parseClause _ = Nothing
parseCase _ = Nothing

parseApp :: Parser Expr
parseApp ts | Just (f, ts') <- parseAtom ts = go f ts'
  where
    go :: Expr -> Parser Expr
    go f' ts'' =
      case parseAtom ts'' of
        Just (arg, ts''') -> go (App f' arg) ts'''
        Nothing -> return (f', ts'')
parseApp _ = Nothing
      
parseBinOp :: Parser Expr
parseBinOp (T.Plus : ts) = do
  (lhs, ts') <- parseAtom ts
  (rhs, ts'') <- parseAtom ts'
  return (BinOp Add lhs rhs, ts'')
parseBinOp (T.Minus : ts) = do
  (lhs, ts') <- parseAtom ts
  (rhs, ts'') <- parseAtom ts'
  return (BinOp Sub lhs rhs, ts'')
parseBinOp _ = Nothing

parseAtom :: Parser Expr
parseAtom ts = parseCons ts <|> parseVar ts <|> parseInt ts <|> parseParens ts

parseAtoms :: Parser [Expr]
parseAtoms = go []
  where
    go :: [Expr] -> [T.Token] -> Maybe ([Expr], [Token])
    go as ts = case parseAtom ts of
                 Just (a, ts') -> go (a : as) ts'
                 Nothing -> return (reverse as, ts)

parseCons :: Parser Expr
parseCons (T.TypeId cons : ts) = do
  (args, ts') <- parseAtoms ts
  return (Cons cons args, ts')
parseCons _ = Nothing

parseVar :: Parser Expr
parseVar (T.Id x : ts) = Just (Var x, ts)
parseVar _ = Nothing

parseInt :: Parser Expr
parseInt (T.Int v : ts) = Just (Int v, ts)
parseInt _ = Nothing

parseParens :: Parser Expr
parseParens (T.LP : ts) = do
  (e, ts') <- parseExpr ts
  case ts' of
    (T.RP : ts'') -> return (e, ts'')
    _ -> Nothing -- Missing ')'
parseParens _ = Nothing


parsePat :: Parser Pat
parsePat ts = parsePCons ts <|> parsePVar ts <|> parsePInt ts

parsePats :: Parser [Pat]
parsePats = go []
  where
    go :: [Pat] -> Parser [Pat]
    go as ts = case parsePat ts of
                 Just (a, ts') -> go (a : as) ts'
                 Nothing -> return (reverse as, ts)
                 
parsePCons :: Parser Pat
parsePCons (T.TypeId cons : ts) = do
  (args, ts') <- parsePats ts
  return (PCons cons args, ts')
parsePCons _ = Nothing

parsePVar :: Parser Pat
parsePVar (T.Id x : ts) = Just (PVar x, ts)
parsePVar _ = Nothing

parsePInt :: Parser Pat
parsePInt (T.Int v : ts) = Just (PInt v, ts)
parsePInt _ = Nothing
